# ==============================================================================
# Script Name:  02_openalex_enrich.R  (v2)
# Author:       Lauren Chenarides
# Last updated: July 2026
# Description:  Enriches the DARE Section 4 publication list with OpenAlex
#               metadata. For each DOI, retrieves the full authorship list
#               (author names, positions, institutional affiliations, ROR IDs)
#               and the citation count. Coauthor names are then matched against
#               the CSU conferred-degree roster to identify student and alumni
#               coauthorships. Journal impact factors are mapped from a wide
#               lookup table to each journal-year combination.
#
# CHANGE FROM v1:
#   openalexR v2.0.0 raises "Column name `id` must not be duplicated" when
#   converting works to a tibble (a collision inside oa2df between the work's
#   own `id` and a nested entity `id`). This version requests
#   output = "list", which skips oa2df entirely, and parses the JSON directly.
#   Field names below come from the OpenAlex Works schema, not from openalexR,
#   so the script no longer breaks when the package renames columns.
#
#   The retry wrapper now distinguishes transient failures (rate limit,
#   timeout, connection reset) from deterministic ones. Deterministic errors
#   fail immediately instead of being retried five times.
#
# Inputs:
#   - `publications_faculty_doi_cleaned.csv` : Faculty-DOI publication list
#   - `journal_impact_factors_2021_2026.xlsx`: Wide IF table, sheet "Impact Factors"
#   - `conferred_degrees.xlsx`               : CSU conferred degrees, sheet "conferred"
#
# Outputs:
#   - `oa_works_raw.rds`         : Cached raw OpenAlex response (list form)
#   - `pub_openalex.csv`         : One row per DOI - citations, year, venue
#   - `coauthors_long.csv`       : One row per DOI x author x affiliation
#   - `coauthor_grad_match.csv`  : Coauthors matched to conferred AREC graduates
#   - `coauthor_match_review.csv`: Probable (initial-only) matches for review
#   - `publications_enriched.csv`: Publication list + citations + impact factor
#   - `journal_if_audit.csv`     : Which venues matched the IF table and how
#   - `oa_missing_dois.csv`      : DOIs OpenAlex did not resolve
#
# Dependencies:
#   - Packages: openalexR, dplyr, tidyr, stringr, readr, readxl, purrr, tibble
#
# Execution Time: Moderate (~5 min for ~400 DOIs, network dependent)
# ==============================================================================

rm(list=ls())

library(openalexR)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(tibble)

# -------------------------
# 0. Configuration
# -------------------------

options(openalexR.mailto = "lauren.chenarides@colostate.edu")
# options(openalexR.apikey = "YOUR_KEY_HERE")

in_dir  <- "data"
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# IF_2023 and IF_2026 are empty in the current lookup. TRUE falls back to the
# nearest available year for the same journal, recorded in `if_match_rule`.
USE_NEAREST_IF_YEAR <- TRUE

# DELETE ANY CACHE WRITTEN BY v1 BEFORE RUNNING - it may hold a partial or
# empty result from the failed fetch.
#   file.remove(file.path(out_dir, "oa_works_raw.rds"))

# -------------------------
# 1. Helper functions
# -------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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

# Only network-level failures are worth retrying. A schema or parsing error
# will fail identically on every attempt.
is_transient_error <- function(e) {
  msg <- conditionMessage(e)
  str_detect(
    msg,
    regex("429|too many requests|timeout|timed out|connection|could not resolve|recv failure|handshake|50[0234] ",
          ignore_case = TRUE)
  )
}

# Fetch one chunk as raw list. Retries only on transient errors.
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

# ---- Parsers over the OpenAlex Works schema -------------------------------
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

# -------------------------
# 2. Read inputs
# -------------------------

pubs <- read_csv(file.path(in_dir, "publications_faculty_doi_cleaned.csv"),
                 show_col_types = FALSE) %>%
  mutate(
    doi_clean = clean_doi(doi),
    # "nodoi" and other placeholders must not act as a shared key. A single
    # non-DOI string would otherwise collapse ~20 unrelated papers into one
    # record and produce spurious many-to-many joins downstream.
    doi_clean = if_else(str_detect(doi_clean %||% "", "^10\\.\\d{4,9}/\\S+$"),
                        doi_clean, NA_character_)
  ) %>% 
  filter(type == "JA") %>%
  select(-index_class, -extension_output, -source_file)

sum(is.na(pubs$doi_clean))   # N = 9 rows with no usable DOI

nrow(pubs)                       # N = 373 faculty-publication rows
length(unique(pubs$doi_clean))   # N = distinct DOIs to query

# A DOI carrying more than one title or year across faculty rows is either a
# mis-assigned DOI or a duplicated entry. Both need a human decision; neither
# should be silently averaged away.
doi_integrity <- pubs %>%
  filter(!is.na(doi_clean)) %>%
  group_by(doi_clean) %>%
  summarize(
    n_rows       = n(),
    n_years      = n_distinct(year),
    n_titles     = n_distinct(title_short),
    n_venues     = n_distinct(venue),
    years        = paste(sort(unique(year)), collapse = "; "),
    titles       = paste(unique(title_short), collapse = " | "),
    venues       = paste(unique(venue), collapse = " | "),
    faculty      = paste(sort(unique(last)), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_years > 1 | n_venues > 1) %>%
  arrange(desc(n_years))

nrow(doi_integrity)               # N = 0 DOIs needing review
write_csv(doi_integrity, file.path(out_dir, "doi_integrity_audit.csv"))

if_wide <- read_excel(file.path(in_dir, "journal_impact_factors_2021_2026.xlsx"),
                      sheet = "Impact Factors")

conferred <- read_excel(file.path(in_dir, "conferred_degrees.xlsx"),
                        sheet = "conferred")

nrow(conferred)                  # N = 65 conferred AREC graduates

# -------------------------
# 3. Query OpenAlex by DOI
# -------------------------

dois <- unique(na.omit(pubs$doi_clean))

# A malformed DOI poisons the whole chunk it lands in. Screen first.
valid_doi <- str_detect(dois, "^10\\.\\d{4,9}/\\S+$")
if (any(!valid_doi)) {
  message("Malformed DOIs skipped: ", sum(!valid_doi))
  print(dois[!valid_doi])
}
dois <- dois[valid_doi]

chunks <- split(dois, ceiling(seq_along(dois) / 50))
cache_path <- file.path(out_dir, "oa_works_raw.rds")

if (file.exists(cache_path)) {
  message("Reading cached OpenAlex response. Delete ", cache_path, " to refresh.")
  works_list <- readRDS(cache_path)
} else {
  works_list <- list()
  for (i in seq_along(chunks)) {
    message("Fetching chunk ", i, " of ", length(chunks))
    works_list <- c(works_list, fetch_chunk(chunks[[i]]))
    Sys.sleep(0.5)
  }
  # Only cache a complete run. fetch_chunk() stops on failure, so reaching
  # this line means every chunk returned.
  saveRDS(works_list, cache_path)
}

length(works_list)               # N = works returned by OpenAlex

# -------------------------
# 4. Work-level table: citations and venue
# -------------------------

pub_openalex <- map_dfr(works_list, parse_work_meta) %>%
  mutate(doi_clean = clean_doi(doi_raw)) %>%
  filter(!is.na(doi_clean)) %>%
  distinct(doi_clean, .keep_all = TRUE)

nrow(pub_openalex)                                  # N = distinct works
length(setdiff(dois, pub_openalex$doi_clean))       # N = unresolved DOIs

missing_dois <- pubs %>%
  filter(doi_clean %in% setdiff(dois, pub_openalex$doi_clean)) %>%
  distinct(doi_clean, title_short, venue, year)

write_csv(missing_dois, file.path(out_dir, "oa_missing_dois.csv"))

# OpenAlex year is preferred as the authority; the CV year is the fallback.
# `year_conflict` marks DOIs where faculty CVs disagreed so the resolution is
# visible rather than assumed.
doi_year <- pubs %>%
  filter(!is.na(doi_clean)) %>%
  left_join(pub_openalex %>% select(doi_clean, oa_year), by = "doi_clean") %>%
  group_by(doi_clean) %>%
  summarize(
    year_cv_min   = suppressWarnings(min(year, na.rm = TRUE)),
    n_years_cv    = n_distinct(year),
    oa_year       = first(na.omit(oa_year)),
    .groups = "drop"
  ) %>%
  mutate(
    year_canonical = coalesce(oa_year, year_cv_min),
    year_conflict  = n_years_cv > 1
  ) %>%
  select(doi_clean, year_canonical, oa_year, year_cv_min, year_conflict)

nrow(doi_year)                                   # N = one row per DOI
length(unique(doi_year$doi_clean)) == nrow(doi_year)   # TRUE - safe to join

# -------------------------
# 5. Coauthor-level table
# -------------------------

# `student_coauthor` is coded per faculty-publication row, so the same paper can
# carry Y on one faculty's row and N or U on another's. Per Lauren's rule, a Y
# anywhere makes the paper a student coauthorship.
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

count(cv_student_doi, student_cv)                # distribution after roll-up
sum(cv_student_doi$student_cv_conflict)          # N = 40 DOIs coded inconsistently

coauthors_long <- map_dfr(works_list, parse_work_authors) %>%
  mutate(doi_clean = clean_doi(doi_raw)) %>%
  filter(!is.na(au_display_name)) %>%
  mutate(au_name_norm = normalize_name(au_display_name)) %>%
  select(doi_clean, oa_id, au_id, au_display_name, au_name_norm, au_orcid,
         author_position, is_corresponding, institution, ror, country_code,
         institution_type, raw_affiliation) %>%
  distinct()

nrow(coauthors_long)                          # N = 1828 author-affiliation rows
length(unique(coauthors_long$au_name_norm))   # N = 884 distinct coauthors
n_distinct(coauthors_long$institution)        # N = 345 distinct institutions

# -------------------------
# 6. Match coauthors to conferred graduates
# -------------------------

# Banner term codes are YYYYTT. The leading four digits give the academic year.
# CONFIRM the term convention (10 / 60 / 90) with the registrar before using
# `grad_year` to separate "student at publication" from "alumni". The name
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

# 6a. Exact match on normalized "first last"
match_exact <- coauthors_long %>%
  distinct(au_id, au_display_name, au_name_norm) %>%
  inner_join(grads, by = c("au_name_norm" = "name_norm")) %>%
  mutate(match_type = "Exact name")

nrow(match_exact)                # N = exact coauthor-graduate matches

# 6b. Probable: same last name and first initial. OpenAlex often stores
# "S. Zhou" where the registrar has "Siwei Zhou". NOT auto-accepted.
match_probable <- coauthors_long %>%
  distinct(au_id, au_display_name, au_name_norm) %>%
  mutate(au_last = word(au_name_norm, -1),
         au_first_init = str_sub(word(au_name_norm, 1), 1, 1)) %>%
  inner_join(grads, by = c("au_last" = "last_norm", "au_first_init" = "first_init")) %>%
  anti_join(match_exact, by = "au_name_norm") %>%
  mutate(match_type = "Last name + first initial - REVIEW")

nrow(match_probable)             # N = probable matches needing review

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
write_csv(match_probable,      file.path(out_dir, "coauthor_match_review.csv"))

student_flag_oa <- coauthor_grad_match %>%
  group_by(doi_clean) %>%
  summarize(
    n_grad_coauthors   = n_distinct(au_name_norm),
    grad_coauthors     = paste(sort(unique(au_display_name)), collapse = "; "),
    any_student_at_pub = any(student_status == "Student at publication"),
    .groups = "drop"
  )

# -------------------------
# 7. Map impact factors to journal-year
# -------------------------

# Convert the wide impact-factor file to one row per journal-year.
# Keep the raw version first so duplicate normalized journal titles can be audited.
if_long_raw <- if_wide %>%
  pivot_longer(
    cols = starts_with("IF_"),
    names_to = "if_year",
    values_to = "impact_factor"
  ) %>%
  mutate(
    if_year = as.integer(str_remove(if_year, "^IF_")),
    journal_norm = normalize_journal(Journal)
  ) %>%
  filter(!is.na(impact_factor))

nrow(if_long_raw)   # N = raw journal-year IF observations


# Identify cases where multiple lookup-table rows collapse to the same
# normalized journal-year combination.
if_duplicates <- if_long_raw %>%
  group_by(journal_norm, if_year) %>%
  filter(n() > 1) %>%
  arrange(journal_norm, if_year, Journal) %>%
  ungroup()

if (nrow(if_duplicates) > 0) {
  message(
    "Found ", nrow(if_duplicates),
    " duplicate impact-factor rows after journal normalization."
  )
  
  print(
    if_duplicates %>%
      select(
        Journal,
        journal_norm,
        if_year,
        impact_factor
      ),
    n = Inf
  )
}


# Summarize duplicate normalized journal-year combinations and determine
# whether alternate titles carry the same or conflicting IF values.
if_duplicate_summary <- if_long_raw %>%
  group_by(journal_norm, if_year) %>%
  summarize(
    n_rows = n(),
    n_values = n_distinct(impact_factor),
    lookup_titles = paste(
      sort(unique(Journal)),
      collapse = " | "
    ),
    impact_factor_values = paste(
      sort(unique(impact_factor)),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  filter(n_rows > 1)

if (nrow(if_duplicate_summary) > 0) {
  print(if_duplicate_summary, n = Inf)
}


# Stop rather than arbitrarily choosing among conflicting IF values.
if_conflicts <- if_duplicate_summary %>%
  filter(n_values > 1)

if (nrow(if_conflicts) > 0) {
  print(if_conflicts, n = Inf)
  
  stop(
    "The impact-factor lookup contains conflicting values for one or more ",
    "normalized journal-year combinations. Review the printed rows in ",
    "`if_conflicts` before continuing.",
    call. = FALSE
  )
}


# Deduplicate alternate journal-title variants that normalize to the same
# journal-year. At this point, duplicate rows are known to have the same IF.
if_long <- if_long_raw %>%
  group_by(journal_norm, if_year) %>%
  summarize(
    impact_factor = first(impact_factor),
    if_lookup_titles = paste(
      sort(unique(Journal)),
      collapse = " | "
    ),
    .groups = "drop"
  )

nrow(if_long)   # N = unique normalized journal-year IF observations


# Confirm that the lookup now has exactly one row per normalized journal-year.
if_lookup_check <- if_long %>%
  count(journal_norm, if_year, name = "n") %>%
  filter(n > 1)

if (nrow(if_lookup_check) > 0) {
  stop(
    "`if_long` still contains duplicate journal-year keys.",
    call. = FALSE
  )
}


# Normalize publication journal titles using the same function applied to the
# impact-factor lookup.
pubs_norm <- pubs %>%
  mutate(
    journal_norm = normalize_journal(venue)
  )


# 7a. Strict join on normalized journal title and publication year.
#
# Many publication rows may correspond to one journal-year IF row, but each
# journal-year in the lookup must have only one impact factor.
if_strict <- pubs_norm %>%
  left_join(
    if_long %>%
      select(
        journal_norm,
        if_year,
        impact_factor
      ),
    by = c(
      "journal_norm",
      "year" = "if_year"
    ),
    relationship = "many-to-one"
  ) %>%
  mutate(
    if_year_used = if_else(
      !is.na(impact_factor),
      as.integer(year),
      NA_integer_
    ),
    if_match_rule = if_else(
      !is.na(impact_factor),
      "Exact journal-year",
      NA_character_
    )
  )


# 7b. Nearest-year fallback within the same normalized journal.
#
# This is used when the journal exists in the IF lookup but the publication
# year itself has no impact-factor value. For example, the current lookup has
# no IF_2023 or IF_2026 values.
if (USE_NEAREST_IF_YEAR) {
  
  # Create one row per unmatched normalized journal-publication year.
  unmatched_journal_years <- if_strict %>%
    filter(
      is.na(impact_factor),
      !is.na(journal_norm),
      journal_norm != "",
      !is.na(year)
    ) %>%
    distinct(
      journal_norm,
      year
    )
  
  # Compare each unmatched publication year with every available IF year for
  # the same journal, then retain the closest year.
  #
  # This join is intentionally many-to-many because one publication year is
  # being compared with multiple available IF years for the same journal.
  nearest_if <- unmatched_journal_years %>%
    inner_join(
      if_long %>%
        select(
          journal_norm,
          if_year,
          impact_factor
        ),
      by = "journal_norm",
      relationship = "many-to-many"
    ) %>%
    mutate(
      year_gap = abs(if_year - year)
    ) %>%
    arrange(
      journal_norm,
      year,
      year_gap,
      desc(if_year)
    ) %>%
    group_by(
      journal_norm,
      year
    ) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      journal_norm,
      year,
      impact_factor_fb = impact_factor,
      if_year_used_fb = if_year,
      if_match_rule_fb = paste0(
        "Nearest year (",
        if_year,
        ")"
      )
    )
  
  
  # Join fallback IF values back to the original publication rows.
  publications_enriched <- if_strict %>%
    left_join(
      nearest_if,
      by = c(
        "journal_norm",
        "year"
      ),
      relationship = "many-to-one"
    ) %>%
    mutate(
      impact_factor = coalesce(
        impact_factor,
        impact_factor_fb
      ),
      if_year_used = coalesce(
        if_year_used,
        if_year_used_fb
      ),
      if_match_rule = coalesce(
        if_match_rule,
        if_match_rule_fb
      )
    ) %>%
    select(
      -impact_factor_fb,
      -if_year_used_fb,
      -if_match_rule_fb
    )
  
} else {
  
  publications_enriched <- if_strict
}


# Label publications that still have no IF match after the optional fallback.
publications_enriched <- publications_enriched %>%
  mutate(
    if_match_rule = replace_na(
      if_match_rule,
      "No IF match"
    )
  )


# 7c. Attach OpenAlex citations and the OpenAlex-derived student flag.
publications_enriched <- publications_enriched %>%
  left_join(
    pub_openalex,
    by = "doi_clean",
    relationship = "many-to-one"
  ) %>%
  left_join(
    student_flag_oa,
    by = "doi_clean",
    relationship = "many-to-one"
  ) %>%
  mutate(
    # Flag where the hand-coded CV student indicator and OpenAlex/registrar
    # match disagree.
    student_flag_disagrees = case_when(
      is.na(student_coauthor) ~ NA,
      student_coauthor == "Y" &
        (is.na(n_grad_coauthors) | n_grad_coauthors == 0) ~ TRUE,
      student_coauthor == "N" &
        !is.na(n_grad_coauthors) &
        n_grad_coauthors > 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Compare the publication year recorded in the CV data with the OpenAlex
    # publication year.
    year_disagrees = (
      !is.na(oa_year) &
        !is.na(year) &
        oa_year != year
    )
  )


# Diagnostic counts
nrow(publications_enriched)   # Expected N = 406 faculty-publication rows

sum(
  is.na(publications_enriched$impact_factor)
)   # N with no IF after optional fallback

sum(
  publications_enriched$if_match_rule == "Exact journal-year",
  na.rm = TRUE
)

sum(
  str_detect(
    publications_enriched$if_match_rule,
    "^Nearest year"
  ),
  na.rm = TRUE
)

sum(
  publications_enriched$if_match_rule == "No IF match",
  na.rm = TRUE
)

sum(
  publications_enriched$year_disagrees,
  na.rm = TRUE
)   # N with CV/OpenAlex year mismatch

# -------------------------
# 8. Journal match audit
# -------------------------

journal_if_audit <- publications_enriched %>%
  count(venue, journal_norm, type, if_match_rule, name = "n_pubs") %>%
  arrange(if_match_rule == "Exact journal-year", desc(n_pubs))

write_csv(journal_if_audit, file.path(out_dir, "journal_if_audit.csv"))

# -------------------------
# 9. Write outputs
# -------------------------

write_csv(pub_openalex,          file.path(out_dir, "pub_openalex.csv"))
write_csv(coauthors_long,        file.path(out_dir, "coauthors_long.csv"))
write_csv(publications_enriched, file.path(out_dir, "publications_enriched.csv"))

publications_enriched %>%
  filter(year >= 2021, year <= 2026) %>%
  group_by(year) %>%
  summarize(
    n_pubs          = n(),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    mean_citations  = round(mean(cited_by_count, na.rm = TRUE), 1),
    mean_if         = round(mean(impact_factor, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

coauthor_grad_match %>% count(student_status) %>% print()
