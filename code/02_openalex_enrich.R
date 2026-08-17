# ==============================================================================
# Script Name:  02_openalex_enrich.R
# Author:       Lauren Chenarides
# Last updated: July 2026
# Description:  Builds the analysis-ready files for Section 4 (Research and
#               Creative Artistry) of the DARE program review. Enriches the
#               faculty-DOI publication list with OpenAlex metadata (full
#               authorship lists, institutional affiliations, citation counts),
#               matches coauthors against the CSU conferred-degree roster to
#               identify student and alumni coauthorships, maps journal impact
#               factors to each journal-year, joins faculty appointment splits,
#               and produces a faculty-year panel with headcount and
#               research-FTE denominators.
#
# Inputs (all in `in_dir`):
#   - publications_faculty_doi_cleaned.csv   : Faculty-DOI publication list
#   - journal_impact_factors_2021_2026.xlsx  : Wide IF table, sheet "Impact Factors"
#   - conferred_degrees.xlsx                 : CSU conferred degrees, sheet "conferred"
#   - appointment_splits.csv                 : Effort distribution, type, rank
#   - roster.csv                             : Per-year active flags; departed faculty splits
#
# Outputs (all in `out_dir`):
#   - oa_works_raw.rds            : Cached raw OpenAlex response (list form)
#   - doi_integrity_audit.csv     : DOIs with conflicting years, titles, or venues
#   - oa_missing_dois.csv         : DOIs OpenAlex did not resolve
#   - pub_openalex.csv            : One row per DOI - citations, year, venue
#   - coauthors_long.csv          : One row per DOI x author x affiliation
#   - coauthor_grad_match.csv     : Coauthors matched to conferred AREC graduates
#   - coauthor_match_review.csv   : Probable (initial-only) matches for review
#   - journal_if_audit.csv        : Which venues matched the IF table and how
#   - publications_enriched.csv   : Publications + citations + IF + student flag
#   - faculty_appointments.csv    : Cleaned appointment table with join key
#   - pubs_analysis.csv           : Publication level + appointment + rank
#   - faculty_year_panel.csv      : Faculty-year panel, active flag, research FTE
#   - dept_year_summary.csv       : Department totals by year, headcount and FTE
#   - appointment_join_audit.csv  : Faculty appearing in only one source
#
# Notes:
#   - Uses oa_fetch(output = "list"). openalexR v2.0.0 raises "Column name `id`
#     must not be duplicated" when building the works tibble; requesting list
#     output skips that conversion and parses the OpenAlex JSON directly, so the
#     script does not depend on openalexR's column naming.
#   - Appointment splits are held constant across 2021-2025 per CODEBOOK.md.
#   - Dollar and IF values remain provisional pending JCR/Scopus and VPR checks.
#
# Dependencies:
#   - Packages: openalexR, dplyr, tidyr, stringr, readr, readxl, purrr, tibble
#
# Execution Time: Moderate (~5 min for ~400 DOIs, network dependent)
# ==============================================================================

library(openalexR)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(tibble)

setwd("C:\Users\lachenar\OneDrive - Colostate\CAS DARE Team-5 year Review - Documents\Research and Creative Artistry - LAUREN\dare_research")

# ==============================================================================
# 0. Configuration
# ==============================================================================

options(openalexR.mailto = "lauren.chenarides@colostate.edu")
# options(openalexR.apikey = "YOUR_KEY_HERE")   # use options(), not Sys.setenv()

in_dir  <- "data"
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

WINDOW <- 2021:2025

# IF_2023 and IF_2026 are empty in the current lookup. TRUE falls back to the
# nearest available year for the same journal, recorded in `if_match_rule`.
# FALSE returns NA for those years instead.
USE_NEAREST_IF_YEAR <- TRUE

# Set TRUE to discard the cached OpenAlex response and re-query.
REFRESH_OPENALEX <- FALSE


# ==============================================================================
# 1. Helper functions
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Normalize person names for matching against the registrar roster.
normalize_name <- function(name) {
  name %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("\\*", " ") %>%
    str_replace_all("[\u00A0\u200B]", " ") %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:][:space:]'\\-\\.]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish() %>%
    str_trim()
}

# Normalize journal titles so naming variants collapse to one key.
normalize_journal <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_remove_all("\\b(the|of|and|for|an|a)\\b") %>%
    str_squish()
}

clean_doi <- function(x) {
  x %>%
    str_remove("^https?://(dx\\.)?doi\\.org/") %>%
    str_trim() %>%
    str_to_lower()
}

# TRUE only for well-formed DOIs. Placeholders such as "nodoi" must become NA:
# a shared non-DOI string acts as a join key and would collapse unrelated
# papers into a single record.
is_valid_doi <- function(x) str_detect(coalesce(x, ""), "^10\\.\\d{4,9}/\\S+$")

# Extract a last-name join key. Handles multi-word surnames ("Anders Van Sandt")
# and inconsistent capitalization ("MARCO Costanigro").
extract_last <- function(full_name) {
  parts <- str_split(str_squish(full_name), " ")
  map_chr(parts, function(p) {
    n <- length(p)
    if (n >= 3 && str_to_lower(p[n - 1]) %in% c("van", "de", "del", "della", "mc", "st")) {
      str_c(p[n - 1], " ", p[n])
    } else {
      p[n]
    }
  }) %>%
    str_to_title() %>%
    str_squish()
}

# Only network-level failures are worth retrying. A schema or parsing error
# fails identically on every attempt, so it stops immediately.
is_transient_error <- function(e) {
  str_detect(
    conditionMessage(e),
    regex("429|too many requests|timeout|timed out|connection|could not resolve|recv failure|handshake|50[0234] ",
          ignore_case = TRUE)
  )
}

fetch_chunk <- function(dois, max_tries = 4) {
  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(
      oa_fetch(entity = "works", doi = dois, output = "list", verbose = FALSE),
      error = function(e) e
    )
    if (!inherits(result, "error")) return(result)
    if (!is_transient_error(result)) {
      stop("Non-transient OpenAlex error (not retried): ", conditionMessage(result))
    }
    wait <- 2^attempt
    message("  transient error, retry ", attempt, " after ", wait, "s")
    Sys.sleep(wait)
  }
  stop("Failed after ", max_tries, " attempts for chunk beginning ", dois[1])
}

# ---- Parsers over the OpenAlex Works schema ----------------------------------
# https://developers.openalex.org/api-reference/works

parse_work_meta <- function(w) {
  tibble(
    oa_id          = w$id %||% NA_character_,
    doi_raw        = w$doi %||% NA_character_,
    oa_title       = w$display_name %||% w$title %||% NA_character_,
    oa_year        = as.integer(w$publication_year %||% NA),
    oa_date        = w$publication_date %||% NA_character_,
    oa_type        = w$type %||% NA_character_,
    cited_by_count = as.integer(w$cited_by_count %||% NA),
    oa_venue       = w$primary_location$source$display_name %||% NA_character_,
    oa_issn_l      = w$primary_location$source$issn_l %||% NA_character_,
    oa_publisher   = w$primary_location$source$host_organization_name %||% NA_character_,
    is_oa          = w$open_access$is_oa %||% NA,
    is_retracted   = w$is_retracted %||% NA,
    n_authors      = length(w$authorships %||% list())
  )
}

parse_work_authors <- function(w) {
  auths <- w$authorships %||% list()
  if (length(auths) == 0) return(NULL)

  map_dfr(auths, function(a) {
    base <- tibble(
      oa_id            = w$id %||% NA_character_,
      doi_raw          = w$doi %||% NA_character_,
      author_position  = a$author_position %||% NA_character_,
      au_id            = a$author$id %||% NA_character_,
      au_display_name  = a$author$display_name %||% NA_character_,
      au_orcid         = a$author$orcid %||% NA_character_,
      is_corresponding = a$is_corresponding %||% NA,
      raw_author_name  = a$raw_author_name %||% NA_character_,
      raw_affiliation  = paste(a$raw_affiliation_strings %||% character(0),
                               collapse = "; ")
    )

    insts <- a$institutions %||% list()
    if (length(insts) == 0) {
      base %>% mutate(institution = NA_character_, ror = NA_character_,
                      country_code = NA_character_, institution_type = NA_character_)
    } else {
      # One row per author-institution pair; multi-affiliated authors repeat.
      map_dfr(insts, function(i) {
        base %>% mutate(
          institution      = i$display_name %||% NA_character_,
          ror              = i$ror %||% NA_character_,
          country_code     = i$country_code %||% NA_character_,
          institution_type = i$type %||% NA_character_
        )
      })
    }
  })
}


# ==============================================================================
# 2. Read inputs
# ==============================================================================

pubs <- read_csv(file.path(in_dir, "publications_faculty_doi_cleaned.csv"),
                 show_col_types = FALSE) %>%
  mutate(
    last      = str_to_title(last),
    doi_clean = clean_doi(doi),
    doi_clean = if_else(is_valid_doi(doi_clean), doi_clean, NA_character_)
  )

nrow(pubs)                                 # N = 406 faculty-publication rows
sum(is.na(pubs$doi_clean))                 # N = 20 rows with no usable DOI
n_distinct(pubs$doi_clean, na.rm = TRUE)   # N = distinct DOIs to query

if_wide <- read_excel(file.path(in_dir, "journal_impact_factors_2021_2026.xlsx"),
                      sheet = "Impact Factors")

conferred <- read_excel(file.path(in_dir, "conferred_degrees.xlsx"),
                        sheet = "conferred")

nrow(conferred)                            # N = 65 conferred AREC graduates

roster <- read_csv(file.path(in_dir, "roster.csv"), show_col_types = FALSE) %>%
  mutate(last = str_to_title(last))

appt_raw <- read_csv(file.path(in_dir, "appointment_splits.csv"),
                     show_col_types = FALSE)


# ==============================================================================
# 3. DOI integrity audit
# ==============================================================================

# A DOI carrying more than one year, title, or venue across faculty rows is
# either a mis-assigned DOI or a duplicated entry. Both need a human decision;
# neither should be silently averaged away.
doi_integrity <- pubs %>%
  filter(!is.na(doi_clean)) %>%
  group_by(doi_clean) %>%
  summarize(
    n_rows   = n(),
    n_years  = n_distinct(year),
    n_titles = n_distinct(title_short),
    n_venues = n_distinct(venue),
    years    = paste(sort(unique(year)), collapse = "; "),
    titles   = paste(unique(title_short), collapse = " | "),
    venues   = paste(unique(venue), collapse = " | "),
    faculty  = paste(sort(unique(last)), collapse = "; "),
    .groups  = "drop"
  ) %>%
  filter(n_years > 1 | n_venues > 1) %>%
  arrange(desc(n_years))

write_csv(doi_integrity, file.path(out_dir, "doi_integrity_audit.csv"))
nrow(doi_integrity)                        # N = DOIs needing review

# OPEN ITEMS AS OF THIS RUN:
#   10.1371/journal.pone.0261833 - a PLOS ONE DOI correctly attached to Suter
#     "Summer Crowds" (2022). Burkhardt "Sustainability of lettuce production"
#     (Journal of Cleaner Production) carries it incorrectly.
#   10.1080/10871209.2024.2414880 - two Hoag rows, different titles and years.
#     Likely one paper entered twice (CV placeholder title plus Scholar title).


# ==============================================================================
# 4. Query OpenAlex by DOI
# ==============================================================================

dois <- pubs %>% filter(!is.na(doi_clean)) %>% pull(doi_clean) %>% unique()
chunks <- split(dois, ceiling(seq_along(dois) / 50))

cache_path <- file.path(out_dir, "oa_works_raw.rds")
if (REFRESH_OPENALEX && file.exists(cache_path)) file.remove(cache_path)

if (file.exists(cache_path)) {
  message("Reading cached OpenAlex response. Set REFRESH_OPENALEX <- TRUE to refresh.")
  works_list <- readRDS(cache_path)
} else {
  works_list <- list()
  for (i in seq_along(chunks)) {
    message("Fetching chunk ", i, " of ", length(chunks))
    works_list <- c(works_list, fetch_chunk(chunks[[i]]))
    Sys.sleep(0.5)
  }
  # fetch_chunk() stops on failure, so reaching this line means every chunk
  # returned. A partial result is never cached.
  saveRDS(works_list, cache_path)
}

length(works_list)                         # N = works returned by OpenAlex


# ==============================================================================
# 5. Work-level table: citations and venue
# ==============================================================================

pub_openalex <- map_dfr(works_list, parse_work_meta) %>%
  mutate(doi_clean = clean_doi(doi_raw)) %>%
  filter(!is.na(doi_clean)) %>%
  distinct(doi_clean, .keep_all = TRUE)

nrow(pub_openalex)                              # N = distinct works resolved
length(setdiff(dois, pub_openalex$doi_clean))   # N = unresolved DOIs

missing_dois <- pubs %>%
  filter(doi_clean %in% setdiff(dois, pub_openalex$doi_clean)) %>%
  distinct(doi_clean, title_short, venue, year)

write_csv(missing_dois, file.path(out_dir, "oa_missing_dois.csv"))


# ==============================================================================
# 6. Coauthor-level table
# ==============================================================================

# One row per DOI x author x affiliation. Multi-affiliated authors repeat, so
# count coauthors with n_distinct(au_id), not nrow().
coauthors_long <- map_dfr(works_list, parse_work_authors) %>%
  mutate(doi_clean = clean_doi(doi_raw)) %>%
  filter(!is.na(au_display_name)) %>%
  mutate(au_name_norm = normalize_name(au_display_name)) %>%
  select(doi_clean, oa_id, au_id, au_display_name, au_name_norm, au_orcid,
         author_position, is_corresponding, institution, ror, country_code,
         institution_type, raw_affiliation) %>%
  distinct()

nrow(coauthors_long)                       # N = author-affiliation rows
n_distinct(coauthors_long$au_id)           # N = distinct coauthors
n_distinct(coauthors_long$institution)     # N = distinct institutions


# ==============================================================================
# 7. Canonical year per DOI
# ==============================================================================

# `pubs` is faculty-DOI level, so a paper coauthored by three DARE faculty has
# three rows. Joins keyed on DOI need one canonical row per DOI. OpenAlex is
# treated as the authority on publication year; the CV year is the fallback.
doi_year <- pubs %>%
  filter(!is.na(doi_clean)) %>%
  left_join(pub_openalex %>% select(doi_clean, oa_year), by = "doi_clean") %>%
  group_by(doi_clean) %>%
  summarize(
    year_cv_min = suppressWarnings(min(year, na.rm = TRUE)),
    n_years_cv  = n_distinct(year),
    oa_year     = first(na.omit(oa_year)),
    .groups     = "drop"
  ) %>%
  mutate(
    year_canonical = coalesce(oa_year, year_cv_min),
    year_conflict  = n_years_cv > 1
  ) %>%
  select(doi_clean, year_canonical, oa_year, year_cv_min, year_conflict)

nrow(doi_year) == n_distinct(doi_year$doi_clean)   # TRUE - safe to join
sum(doi_year$year_conflict)                        # N = DOIs where CVs disagreed


# ==============================================================================
# 8. Match coauthors to conferred graduates
# ==============================================================================

# Banner term codes are YYYYTT. The leading four digits give the academic year.
# CONFIRM the term convention (10 / 60 / 90) with the registrar before relying
# on `grad_year` to separate "student at publication" from "alumni". The name
# match itself does not depend on it.
grads <- conferred %>%
  transmute(
    grad_last  = `Last Name`,
    grad_first = `First Name`,
    program    = Program,
    term_code  = `Academic Period Graduation`,
    grad_year  = suppressWarnings(as.integer(str_sub(as.character(term_code), 1, 4))),
    name_norm  = normalize_name(paste(`First Name`, `Last Name`)),
    last_norm  = normalize_name(`Last Name`),
    first_init = str_sub(normalize_name(`First Name`), 1, 1)
  )

# 8a. Exact match on normalized "first last"
match_exact <- coauthors_long %>%
  distinct(au_id, au_display_name, au_name_norm) %>%
  inner_join(grads, by = c("au_name_norm" = "name_norm")) %>%
  mutate(match_type = "Exact name")

nrow(match_exact)                          # N = exact coauthor-graduate matches

# 8b. Probable match: same last name and first initial. OpenAlex often stores
# "S. Zhou" where the registrar has "Siwei Zhou". NOT auto-accepted - these go
# to a review file so a false positive never becomes a reported statistic.
match_probable <- coauthors_long %>%
  distinct(au_id, au_display_name, au_name_norm) %>%
  mutate(au_last       = word(au_name_norm, -1),
         au_first_init = str_sub(word(au_name_norm, 1), 1, 1)) %>%
  inner_join(grads, by = c("au_last" = "last_norm", "au_first_init" = "first_init")) %>%
  anti_join(match_exact, by = "au_name_norm") %>%
  mutate(match_type = "Last name + first initial - REVIEW")

nrow(match_probable)                       # N = probable matches needing review
write_csv(match_probable, file.path(out_dir, "coauthor_match_review.csv"))

coauthor_grad_match <- coauthors_long %>%
  inner_join(match_exact %>% select(au_name_norm, grad_last, grad_first,
                                    program, grad_year, match_type),
             by = "au_name_norm") %>%
  left_join(doi_year %>% select(doi_clean, year_canonical), by = "doi_clean") %>%
  mutate(
    student_status = case_when(
      is.na(grad_year) | is.na(year_canonical) ~ "Unknown",
      year_canonical <= grad_year              ~ "Student at publication",
      TRUE                                     ~ "Alumni coauthor"
    )
  ) %>%
  arrange(grad_last, year_canonical)

write_csv(coauthor_grad_match, file.path(out_dir, "coauthor_grad_match.csv"))
count(coauthor_grad_match, student_status)

student_flag_oa <- coauthor_grad_match %>%
  group_by(doi_clean) %>%
  summarize(
    n_grad_coauthors   = n_distinct(au_name_norm),
    grad_coauthors     = paste(sort(unique(au_display_name)), collapse = "; "),
    any_student_at_pub = any(student_status == "Student at publication"),
    .groups = "drop"
  )

# CV-coded student flag rolled up to the DOI. The same paper can carry Y on one
# faculty's row and N or U on another's; a Y anywhere makes it a student paper.
cv_student_doi <- pubs %>%
  filter(!is.na(doi_clean)) %>%
  group_by(doi_clean) %>%
  summarize(
    student_cv = case_when(
      any(student_coauthor == "Y", na.rm = TRUE) ~ "Y",
      any(student_coauthor == "N", na.rm = TRUE) ~ "N",
      TRUE                                       ~ "U"
    ),
    student_cv_conflict = n_distinct(na.omit(student_coauthor)) > 1,
    .groups = "drop"
  )

sum(cv_student_doi$student_cv_conflict)    # N = DOIs coded inconsistently


# ==============================================================================
# 9. Map impact factors to journal-year
# ==============================================================================

if_long <- if_wide %>%
  pivot_longer(cols = starts_with("IF_"), names_to = "if_year",
               values_to = "impact_factor") %>%
  mutate(if_year      = as.integer(str_remove(if_year, "^IF_")),
         journal_norm = normalize_journal(Journal)) %>%
  filter(!is.na(impact_factor))

nrow(if_long)                              # N = journal-year IF observations

pubs_norm <- pubs %>% mutate(journal_norm = normalize_journal(venue))

if_strict <- pubs_norm %>%
  left_join(if_long %>% select(journal_norm, if_year, impact_factor),
            by = c("journal_norm", "year" = "if_year")) %>%
  mutate(if_year_used  = if_else(!is.na(impact_factor), year, NA_integer_),
         if_match_rule = if_else(!is.na(impact_factor), "Exact journal-year",
                                 NA_character_))

if (USE_NEAREST_IF_YEAR) {
  nearest_if <- if_strict %>%
    filter(is.na(impact_factor)) %>%
    distinct(journal_norm, year) %>%
    inner_join(if_long %>% select(journal_norm, if_year, impact_factor),
               by = "journal_norm", relationship = "many-to-many") %>%
    mutate(year_gap = abs(if_year - year)) %>%
    group_by(journal_norm, year) %>%
    slice_min(year_gap, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(journal_norm, year,
              impact_factor_fb = impact_factor,
              if_year_used_fb  = if_year,
              if_match_rule_fb = paste0("Nearest year (", if_year, ")"))

  publications_enriched <- if_strict %>%
    left_join(nearest_if, by = c("journal_norm", "year")) %>%
    mutate(
      impact_factor = coalesce(impact_factor, impact_factor_fb),
      if_year_used  = coalesce(if_year_used, if_year_used_fb),
      if_match_rule = coalesce(if_match_rule, if_match_rule_fb)
    ) %>%
    select(-ends_with("_fb"))
} else {
  publications_enriched <- if_strict
}


# ==============================================================================
# 10. Assemble the enriched publication file
# ==============================================================================

publications_enriched <- publications_enriched %>%
  left_join(pub_openalex,    by = "doi_clean") %>%
  left_join(student_flag_oa, by = "doi_clean") %>%
  left_join(cv_student_doi,  by = "doi_clean") %>%
  left_join(doi_year %>% select(doi_clean, year_canonical, year_conflict),
            by = "doi_clean") %>%
  mutate(
    if_match_rule = replace_na(if_match_rule, "No IF match"),
    student_cv    = coalesce(student_cv, student_coauthor, "U"),

    # Union rule: a CV-coded Y is authoritative and is never downgraded. A
    # registrar match adds a Y where the CV did not claim one.
    student_coauthor_final = case_when(
      student_cv == "Y"                               ~ "Y",
      !is.na(n_grad_coauthors) & n_grad_coauthors > 0 ~ "Y",
      student_cv == "N"                               ~ "N",
      TRUE                                            ~ "U"
    ),
    student_evidence = case_when(
      student_cv == "Y" & !is.na(n_grad_coauthors) & n_grad_coauthors > 0 ~ "CV and registrar",
      student_cv == "Y"                                                   ~ "CV only",
      !is.na(n_grad_coauthors) & n_grad_coauthors > 0                     ~ "Registrar only",
      student_cv == "N"                                                   ~ "Neither",
      TRUE                                                                ~ "Undetermined"
    ),
    year_disagrees = !is.na(oa_year) & !is.na(year) & oa_year != year
  )

nrow(publications_enriched)                              # N = 406
sum(is.na(publications_enriched$impact_factor))          # N with no IF
sum(publications_enriched$year_disagrees, na.rm = TRUE)  # N year mismatches
count(publications_enriched, student_coauthor_final, student_evidence)

# "CV only" rows are expected, not errors: the conferred file covers 65 AREC
# graduates, so it cannot match undergraduates, students in other departments,
# students at coauthors' institutions, or current students not yet graduated.

journal_if_audit <- publications_enriched %>%
  count(venue, journal_norm, type, if_match_rule, name = "n_pubs") %>%
  arrange(if_match_rule == "Exact journal-year", desc(n_pubs))

write_csv(journal_if_audit,      file.path(out_dir, "journal_if_audit.csv"))
write_csv(pub_openalex,          file.path(out_dir, "pub_openalex.csv"))
write_csv(coauthors_long,        file.path(out_dir, "coauthors_long.csv"))
write_csv(publications_enriched, file.path(out_dir, "publications_enriched.csv"))


# ==============================================================================
# 11. Faculty appointments
# ==============================================================================

# Column 7 of the appointment file is unnamed and flags departures ("GONE").
gone_col <- names(appt_raw)[7]

faculty_appointments <- appt_raw %>%
  rename(
    candidate_name = `Candidate Name`,
    faculty_type   = `Faculty Type`,
    faculty_rank   = `Faculty Rank`,
    pct_teaching   = `Effort Distribution: Instruction, Advising, and Mentoring`,
    pct_research   = `Effort Distribution: Research, Scholarship, and Creative Activity`,
    pct_service    = `Effort Distribution: University/Professional/Public Service and Outreach`
  ) %>%
  mutate(
    last           = extract_last(candidate_name),
    departed_flag  = !is.na(.data[[gone_col]]),
    is_tt          = faculty_type == "Tenured/Tenure Track",
    split_sum      = pct_teaching + pct_research + pct_service,
    split_sums_100 = split_sum == 100,
    appt_source    = "appointment_splits.csv"
  ) %>%
  select(last, candidate_name, faculty_type, faculty_rank, is_tt,
         pct_teaching, pct_research, pct_service, split_sum, split_sums_100,
         departed_flag, appt_source)

nrow(faculty_appointments)                 # N = 30
sum(faculty_appointments$is_tt)            # N = 24 tenured/tenure track
sum(!faculty_appointments$split_sums_100)  # N = 2 (Thilmany 51, Bennett 90)

# Splits that do not total 100 are left as reported. Weighting uses the research
# share itself, so Thilmany contributes 0.21 FTE either way. No imputation.

# The appointment file is a current snapshot and omits faculty who left during
# the window. Their splits come from roster.csv so 2021-2024 denominators are
# complete. CONFIRM the faculty_type assumption below for each.
roster_only <- roster %>%
  filter(!last %in% faculty_appointments$last) %>%
  transmute(
    last,
    candidate_name = str_c(first, " ", last),
    faculty_type   = "Tenured/Tenure Track",
    faculty_rank   = NA_character_,
    is_tt          = TRUE,
    pct_teaching   = as.numeric(teaching_pct),
    pct_research   = as.numeric(research_pct),
    pct_service    = as.numeric(outreach_pct),
    split_sum      = pct_teaching + pct_research + pct_service,
    split_sums_100 = split_sum == 100,
    departed_flag  = TRUE,
    appt_source    = "roster.csv (departed or absent from appointment file)"
  )

nrow(roster_only)          # N = added from roster (Chouinard, Frasier, Hill,
                           #     Jablonski, Manning)

faculty_appointments <- bind_rows(faculty_appointments, roster_only)
write_csv(faculty_appointments, file.path(out_dir, "faculty_appointments.csv"))

appointment_join_audit <- full_join(
  faculty_appointments %>% distinct(last, appt_source, faculty_type),
  publications_enriched %>% distinct(last) %>% mutate(in_publications = TRUE),
  by = "last"
) %>%
  mutate(
    in_appointments = !is.na(appt_source),
    in_publications = replace_na(in_publications, FALSE),
    audit_note = case_when(
      in_appointments & !in_publications ~ "Appointment record, no publications in window",
      !in_appointments & in_publications ~ "Publications but no appointment record",
      TRUE                               ~ "Matched"
    )
  ) %>%
  arrange(audit_note, last)

write_csv(appointment_join_audit, file.path(out_dir, "appointment_join_audit.csv"))
count(appointment_join_audit, audit_note)


# ==============================================================================
# 12. Publication-level analysis file
# ==============================================================================

pubs_analysis <- publications_enriched %>%
  left_join(
    faculty_appointments %>%
      select(last, faculty_type, faculty_rank, is_tt,
             pct_research, pct_teaching, pct_service, departed_flag),
    by = "last"
  ) %>%
  mutate(
    research_fte = pct_research / 100,
    in_window    = year %in% WINDOW,
    countable    = extension_output == 0 & in_window
  )

nrow(pubs_analysis)                        # N = 406
sum(is.na(pubs_analysis$pct_research))     # N = rows with no appointment match

write_csv(pubs_analysis, file.path(out_dir, "pubs_analysis.csv"))


# ==============================================================================
# 13. Faculty-year panel and department summary
# ==============================================================================

active_long <- roster %>%
  select(last, starts_with("y20")) %>%
  pivot_longer(cols = starts_with("y20"), names_to = "year", values_to = "active") %>%
  mutate(year   = as.integer(str_remove(year, "^y")),
         active = as.integer(active)) %>%
  filter(year %in% WINDOW)

output_counts <- pubs_analysis %>%
  filter(countable) %>%
  count(last, year, name = "n_outputs")

citation_counts <- pubs_analysis %>%
  filter(countable) %>%
  group_by(last, year) %>%
  summarize(total_citations = sum(cited_by_count, na.rm = TRUE),
            mean_if         = round(mean(impact_factor, na.rm = TRUE), 2),
            n_student_pubs  = sum(student_coauthor_final == "Y", na.rm = TRUE),
            .groups = "drop")

faculty_year_panel <- active_long %>%
  left_join(faculty_appointments %>%
              select(last, faculty_type, faculty_rank, is_tt, pct_research),
            by = "last") %>%
  left_join(output_counts,   by = c("last", "year")) %>%
  left_join(citation_counts, by = c("last", "year")) %>%
  mutate(
    n_outputs       = replace_na(n_outputs, 0L),
    total_citations = replace_na(total_citations, 0),
    n_student_pubs  = replace_na(n_student_pubs, 0L),
    research_fte    = if_else(active == 1, pct_research / 100, 0),
    output_per_fte  = if_else(research_fte > 0, n_outputs / research_fte, NA_real_)
  ) %>%
  arrange(last, year)

write_csv(faculty_year_panel, file.path(out_dir, "faculty_year_panel.csv"))

# Two denominators. Headcount is what a reviewer computes by default; research
# FTE is what the department is actually funded to produce. The gap between
# them is the argument.
dept_year_summary <- faculty_year_panel %>%
  filter(active == 1) %>%
  group_by(year) %>%
  summarize(
    headcount_all   = n(),
    headcount_tt    = sum(is_tt, na.rm = TRUE),
    research_fte    = sum(research_fte, na.rm = TRUE),
    n_outputs       = sum(n_outputs),
    total_citations = sum(total_citations),
    n_student_pubs  = sum(n_student_pubs),
    output_per_head = round(n_outputs / headcount_all, 2),
    output_per_tt   = round(n_outputs / headcount_tt, 2),
    output_per_fte  = round(n_outputs / research_fte, 2),
    .groups = "drop"
  )

write_csv(dept_year_summary, file.path(out_dir, "dept_year_summary.csv"))
print(dept_year_summary)

# Rank and appointment-type cuts support the mentoring and trajectory
# discussion in Section 4.
faculty_year_panel %>%
  filter(active == 1, !is.na(faculty_rank)) %>%
  group_by(faculty_rank) %>%
  summarize(n_faculty_years = n(),
            research_fte    = sum(research_fte),
            n_outputs       = sum(n_outputs),
            output_per_fte  = round(n_outputs / sum(research_fte), 2),
            .groups = "drop") %>%
  arrange(desc(output_per_fte)) %>%
  print()

faculty_year_panel %>%
  filter(active == 1) %>%
  group_by(faculty_type) %>%
  summarize(n_faculty_years = n(),
            research_fte    = sum(research_fte, na.rm = TRUE),
            n_outputs       = sum(n_outputs),
            .groups = "drop") %>%
  print()

# ==============================================================================
# End of script
# ==============================================================================
