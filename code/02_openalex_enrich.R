# ==============================================================================
# Script Name:  02_openalex_enrich.R
# Author:       Lauren Chenarides
# Purpose:      Discover missing publications for rostered faculty, append them
#               to the curated publication data in memory, and enrich the
#               resulting faculty-publication file with OpenAlex metadata,
#               graduate-coauthor matches, and journal impact factors.
#
# Run this script from anywhere inside the dare_research repository.
#
# Required roster columns:
#   last, first, area, orcid, openalex_author_id
#
# Publication windows:
#   - 2021-2025: program-review reporting window
#   - 2026: collected and flagged as edge_2026, but not part of the window
#
# Important design rules:
#   - data/publications_faculty_doi_cleaned.csv remains the curated baseline.
#   - OpenAlex discovery uses verified ORCID/OpenAlex author identifiers only.
#   - Faculty-year activity flags in roster.csv determine CSU affiliation at
#     publication. Newly discovered works outside an active year are audited
#     but not appended.
#   - New faculty-publication rows are appended automatically and written to
#     data/publications_faculty_doi_updated.csv.
#   - OpenAlex's exact work type is retained in openalex_type. The narrower DARE
#     type is assigned only where a defensible mapping exists. All other types
#     receive OA_OTHER and type_requires_review = 1.
#   - DOI is the preferred publication key. OpenAlex work ID is second. A
#     normalized title-year key is the final fallback.
#   - CV/curated publication year remains authoritative. OpenAlex year is an
#     audit field and never silently replaces the curated year.
#   - This script ends with publications_enriched.csv. Appointment joins,
#     faculty-year panels, and department summaries belong in stage 03.
#
# Main inputs:
#   data/publications_faculty_doi_cleaned.csv
#   data/roster.csv
#   data/journal_impact_factors_2021_2026.xlsx
#   data/conferred_degrees.xlsx
#
# Main outputs:
#   data/publications_faculty_doi_updated.csv
#   output/publications_enriched.csv
#   output/pub_openalex.csv
#   output/coauthors_long.csv
#   output/coauthor_grad_match.csv
#   output/openalex_discovery_audit.csv
#   output/openalex_affiliation_exclusions.csv
#   output/publication_affiliation_audit.csv
#   output/openalex_type_mapping_audit.csv
#   output/roster_identifier_audit.csv
#   output/doi_integrity_audit.csv
#   output/oa_missing_dois.csv
#   output/journal_if_audit.csv
#   output/if_key_audit.csv
#
# Packages:
#   openalexR, dplyr, tidyr, stringr, readr, readxl, purrr, tibble
# ==============================================================================

suppressPackageStartupMessages({
  library(openalexR)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(readxl)
  library(purrr)
  library(tibble)
})


# ==============================================================================
# 0. Configuration
# ==============================================================================

REPORT_YEARS    <- 2021:2025
DISCOVERY_YEARS <- 2021:2026

# Discovery is refreshed by default because its purpose is to find new works.
REFRESH_DISCOVERY <- TRUE

# DOI enrichment uses a structured cache. Set TRUE to discard it and re-query.
REFRESH_DOI_CACHE <- FALSE

# When an exact journal-year impact factor is unavailable, use the closest
# available year for that journal. Equal-distance ties use the earlier year.
USE_NEAREST_IF_YEAR <- TRUE

options(openalexR.mailto = "lauren.chenarides@colostate.edu")
# Store the API key outside this script, for example in ~/.Rprofile:
# options(openalexR.apikey = "YOUR_API_KEY")


# ==============================================================================
# 1. General helpers
# ==============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    has_readme  <- file.exists(file.path(current, "README.md"))
    has_codebook <- file.exists(file.path(current, "CODEBOOK.md"))

    if (has_readme && has_codebook) return(current)

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not locate the dare_research project root. ",
        "Run the script from inside the repository.",
        call. = FALSE
      )
    }
    current <- parent
  }
}

PROJECT_ROOT <- find_project_root()
in_dir       <- file.path(PROJECT_ROOT, "data")
out_dir      <- file.path(PROJECT_ROOT, "output")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_publications_path <- file.path(
  in_dir,
  "publications_faculty_doi_cleaned.csv"
)
updated_publications_path <- file.path(
  in_dir,
  "publications_faculty_doi_updated.csv"
)
roster_path <- file.path(in_dir, "roster.csv")


normalize_name <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_replace_all("\\*", " ") %>%
    str_replace_all("[\u00A0\u200B]", " ") %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:][:space:]'\\-\\.]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish() %>%
    str_trim()
}

normalize_text <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

normalize_journal <- function(x) {
  x %>%
    normalize_text() %>%
    str_remove_all("\\b(the|of|and|for|an|a)\\b") %>%
    str_squish()
}

clean_doi <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_remove(regex("^doi:\\s*", ignore_case = TRUE)) %>%
    str_remove(regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE)) %>%
    str_to_lower() %>%
    na_if("")
}

is_valid_doi <- function(x) {
  str_detect(coalesce(x, ""), "^10\\.\\d{4,9}/\\S+$")
}

clean_orcid <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_remove(regex("^https?://orcid\\.org/", ignore_case = TRUE)) %>%
    str_to_upper() %>%
    na_if("")
}

is_valid_orcid <- function(x) {
  str_detect(coalesce(x, ""), "^\\d{4}-\\d{4}-\\d{4}-[\\dX]{4}$")
}

clean_openalex_author_id <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_remove(regex("^https?://openalex\\.org/", ignore_case = TRUE)) %>%
    str_to_upper() %>%
    na_if("")
}

is_valid_openalex_author_id <- function(x) {
  str_detect(coalesce(x, ""), "^A\\d+$")
}

clean_openalex_work_id <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_remove(regex("^https?://openalex\\.org/", ignore_case = TRUE)) %>%
    str_to_upper() %>%
    na_if("")
}

make_title_year_key <- function(title, year) {
  title_key <- normalize_text(title)
  title_key <- if_else(
    is.na(title_key) | title_key == "",
    NA_character_,
    str_sub(title_key, 1, 60)
  )

  if_else(
    is.na(title_key) | is.na(year),
    NA_character_,
    str_c("titleyear:", title_key, "|", year)
  )
}

make_publication_key <- function(doi, title, year, openalex_work_id = NA_character_) {
  doi_clean <- clean_doi(doi)
  oa_id     <- clean_openalex_work_id(openalex_work_id)
  title_key <- make_title_year_key(title, year)

  case_when(
    is_valid_doi(doi_clean) ~ str_c("doi:", doi_clean),
    !is.na(oa_id)           ~ str_c("oa:", oa_id),
    !is.na(title_key)       ~ title_key,
    TRUE                    ~ NA_character_
  )
}

work_identity <- function(w) {
  oa_id <- clean_openalex_work_id(w$id %||% NA_character_)
  doi   <- clean_doi(w$doi %||% NA_character_)
  title <- w$display_name %||% w$title %||% NA_character_
  year  <- suppressWarnings(as.integer(w$publication_year %||% NA))

  make_publication_key(doi, title, year, oa_id)
}

unique_works <- function(works) {
  if (length(works) == 0) return(list())

  keys <- map_chr(works, work_identity)
  keep <- !is.na(keys) & !duplicated(keys)
  works[keep]
}

map_openalex_type <- function(oa_type) {
  case_when(
    oa_type %in% c("article", "review", "data-paper", "software-paper") ~ "JA",
    oa_type == "book"                                                     ~ "BK",
    oa_type == "book-chapter"                                             ~ "BC",
    oa_type == "conference-paper"                                         ~ "CP",
    oa_type == "report"                                                   ~ "RP",
    TRUE                                                                   ~ "OA_OTHER"
  )
}

type_mapping_rule <- function(oa_type) {
  mapped <- map_openalex_type(oa_type)
  case_when(
    oa_type == "report" ~
      "Mapped to RP; research, extension, and refereed status require review",
    mapped == "OA_OTHER" ~
      "No direct DARE codebook mapping; review OpenAlex type",
    TRUE ~ "Direct OpenAlex-to-DARE mapping"
  )
}

openalex_type_requires_review <- function(oa_type) {
  as.integer(oa_type == "report" | map_openalex_type(oa_type) == "OA_OTHER")
}

default_extension_output <- function(oa_type) {
  if_else(
    oa_type == "report" | map_openalex_type(oa_type) == "OA_OTHER",
    NA_integer_,
    0L
  )
}

is_transient_error <- function(e) {
  str_detect(
    conditionMessage(e),
    regex(
      paste(
        "429|too many requests|timeout|timed out|connection|",
        "could not resolve|recv failure|handshake|50[0234]"
      ),
      ignore_case = TRUE
    )
  )
}

fetch_oa <- function(args, max_tries = 4) {
  for (attempt in seq_len(max_tries)) {
    result <- tryCatch(
      do.call(openalexR::oa_fetch, args),
      error = function(e) e
    )

    if (!inherits(result, "error")) return(result)

    if (!is_transient_error(result)) {
      stop(
        "Non-transient OpenAlex error: ",
        conditionMessage(result),
        call. = FALSE
      )
    }

    wait_seconds <- 2^attempt
    message(
      "Transient OpenAlex error. Retry ",
      attempt,
      " of ",
      max_tries,
      " after ",
      wait_seconds,
      " seconds."
    )
    Sys.sleep(wait_seconds)
  }

  stop("OpenAlex request failed after ", max_tries, " attempts.", call. = FALSE)
}

bind_missing_columns <- function(df, defaults) {
  for (column_name in names(defaults)) {
    if (!column_name %in% names(df)) {
      df[[column_name]] <- rep(defaults[[column_name]], nrow(df))
    }
  }
  df
}


# ==============================================================================
# 2. OpenAlex parsers
# ==============================================================================

parse_work_meta <- function(w) {
  oa_id     <- clean_openalex_work_id(w$id %||% NA_character_)
  doi_raw   <- w$doi %||% NA_character_
  doi_clean <- clean_doi(doi_raw)
  doi_clean <- if_else(is_valid_doi(doi_clean), doi_clean, NA_character_)
  oa_title  <- w$display_name %||% w$title %||% NA_character_
  oa_year   <- suppressWarnings(as.integer(w$publication_year %||% NA))

  tibble(
    publication_key = make_publication_key(doi_clean, oa_title, oa_year, oa_id),
    oa_id            = oa_id,
    doi_raw          = doi_raw,
    doi_clean        = doi_clean,
    oa_title         = oa_title,
    oa_year          = oa_year,
    oa_date          = w$publication_date %||% NA_character_,
    oa_type          = w$type %||% NA_character_,
    cited_by_count   = suppressWarnings(as.integer(w$cited_by_count %||% NA)),
    oa_venue         = w$primary_location$source$display_name %||% NA_character_,
    oa_issn_l        = w$primary_location$source$issn_l %||% NA_character_,
    oa_publisher     = w$primary_location$source$host_organization_name %||% NA_character_,
    is_oa            = w$open_access$is_oa %||% NA,
    is_retracted     = w$is_retracted %||% NA,
    n_authors        = length(w$authorships %||% list())
  )
}

parse_work_authors <- function(w) {
  auths <- w$authorships %||% list()
  if (length(auths) == 0) return(tibble())

  work_meta <- parse_work_meta(w)

  map_dfr(auths, function(a) {
    author_id    <- clean_openalex_author_id(a$author$id %||% NA_character_)
    author_orcid <- clean_orcid(a$author$orcid %||% a$raw_orcid %||% NA_character_)
    author_name  <- a$author$display_name %||% a$raw_author_name %||% NA_character_
    author_norm  <- normalize_name(author_name)
    author_key   <- coalesce(
      author_id,
      if_else(!is.na(author_norm), str_c("name:", author_norm), NA_character_)
    )

    base <- tibble(
      publication_key = work_meta$publication_key,
      oa_id            = work_meta$oa_id,
      doi_clean        = work_meta$doi_clean,
      au_key           = author_key,
      au_id            = author_id,
      au_orcid         = author_orcid,
      au_display_name  = author_name,
      au_name_norm     = author_norm,
      author_position  = a$author_position %||% NA_character_,
      is_corresponding = a$is_corresponding %||% NA,
      raw_author_name  = a$raw_author_name %||% NA_character_,
      raw_affiliation  = paste(
        a$raw_affiliation_strings %||% character(0),
        collapse = "; "
      )
    )

    institutions <- a$institutions %||% list()

    if (length(institutions) == 0) {
      return(
        base %>%
          mutate(
            institution      = NA_character_,
            ror              = NA_character_,
            country_code     = NA_character_,
            institution_type = NA_character_
          )
      )
    }

    map_dfr(institutions, function(i) {
      base %>%
        mutate(
          institution      = i$display_name %||% NA_character_,
          ror              = i$ror %||% NA_character_,
          country_code     = i$country_code %||% NA_character_,
          institution_type = i$type %||% NA_character_
        )
    })
  })
}


# ==============================================================================
# 3. Read and validate the curated inputs
# ==============================================================================

pubs <- read_csv(base_publications_path, show_col_types = FALSE)
roster <- read_csv(roster_path, show_col_types = FALSE)

if (!"orcid" %in% names(roster)) {
  roster$orcid <- NA_character_
}

if (!"openalex_author_id" %in% names(roster)) {
  roster$openalex_author_id <- NA_character_
}

# write_csv(roster, "data/roster.csv", na = "")

required_publication_columns <- c(
  "last", "first", "area", "year", "type", "index_class",
  "student_coauthor", "dare_coauthors", "dare_coauthors_names",
  "title_short", "venue", "doi", "source_file"
)

missing_publication_columns <- setdiff(
  required_publication_columns,
  names(pubs)
)

if (length(missing_publication_columns) > 0) {
  stop(
    "The curated publication file is missing: ",
    paste(missing_publication_columns, collapse = ", "),
    call. = FALSE
  )
}

active_year_columns <- str_c("y", DISCOVERY_YEARS)

required_roster_columns <- c(
  "last", "first", "area", "orcid", "openalex_author_id",
  active_year_columns
)

missing_roster_columns <- setdiff(required_roster_columns, names(roster))

if (length(missing_roster_columns) > 0) {
  stop(
    "roster.csv is missing: ",
    paste(missing_roster_columns, collapse = ", "),
    ". Add `orcid` and `openalex_author_id` columns and rerun.",
    call. = FALSE
  )
}

publication_defaults <- list(
  openalex_work_id    = NA_character_,
  openalex_type       = NA_character_,
  type_mapping_rule   = NA_character_,
  type_requires_review = 0L,
  extension_output    = NA_integer_,
  affiliated_at_publication = NA_integer_,
  affiliation_status  = NA_character_,
  added_from_openalex = 0L,
  added_from_coauthor = 0L,
  author_match_rule   = NA_character_,
  discovery_date      = NA_character_,
  edge_2026           = 0L
)

pubs <- bind_missing_columns(pubs, publication_defaults) %>%
  mutate(
    last                 = str_to_title(str_squish(last)),
    first                = str_to_title(str_squish(first)),
    year                 = suppressWarnings(as.integer(year)),
    doi_clean            = clean_doi(doi),
    doi_clean            = if_else(
      is_valid_doi(doi_clean),
      doi_clean,
      NA_character_
    ),
    openalex_work_id     = clean_openalex_work_id(openalex_work_id),
    faculty_key          = normalize_name(str_c(first, last, sep = " ")),
    title_year_key       = make_title_year_key(title_short, year),
    publication_key      = make_publication_key(
      doi_clean,
      title_short,
      year,
      openalex_work_id
    ),
    extension_output     = case_when(
      !is.na(extension_output) ~ as.integer(extension_output),
      type == "EX"             ~ 1L,
      type %in% c("JA", "BC", "BK", "RP", "CP") ~ 0L,
      TRUE                     ~ NA_integer_
    ),
    edge_2026             = as.integer(year == 2026),
    added_from_openalex   = replace_na(as.integer(added_from_openalex), 0L),
    added_from_coauthor   = replace_na(as.integer(added_from_coauthor), 0L),
    type_requires_review  = replace_na(as.integer(type_requires_review), 0L)
  )

if (any(is.na(pubs$year))) {
  stop(
    "The curated publication file contains missing or invalid years. ",
    "The CODEBOOK requires undated works to remain excluded.",
    call. = FALSE
  )
}

roster <- roster %>%
  mutate(
    last               = str_to_title(str_squish(last)),
    first              = str_to_title(str_squish(first)),
    faculty_key        = normalize_name(str_c(first, last, sep = " ")),
    orcid              = clean_orcid(orcid),
    openalex_author_id = clean_openalex_author_id(openalex_author_id),
    orcid_valid        = is.na(orcid) | is_valid_orcid(orcid),
    openalex_id_valid  = is.na(openalex_author_id) |
      is_valid_openalex_author_id(openalex_author_id),
    has_identifier     = !is.na(orcid) | !is.na(openalex_author_id),
    identifier_status  = case_when(
      !orcid_valid | !openalex_id_valid ~ "Invalid identifier",
      !has_identifier                   ~ "No identifier; discovery skipped",
      !is.na(orcid) & !is.na(openalex_author_id) ~ "ORCID and OpenAlex ID",
      !is.na(openalex_author_id)         ~ "OpenAlex ID only",
      TRUE                               ~ "ORCID only"
    )
  )

roster_active_year <- roster %>%
  select(faculty_key, all_of(active_year_columns)) %>%
  pivot_longer(
    cols = all_of(active_year_columns),
    names_to = "active_year_column",
    values_to = "active_value"
  ) %>%
  mutate(
    year = as.integer(str_remove(active_year_column, "^y")),
    affiliated_at_publication = suppressWarnings(as.integer(active_value))
  ) %>%
  select(faculty_key, year, affiliated_at_publication)

invalid_active_flags <- roster_active_year %>%
  filter(
    is.na(affiliated_at_publication) |
      !affiliated_at_publication %in% c(0L, 1L)
  )

if (nrow(invalid_active_flags) > 0) {
  stop(
    "Roster activity columns y2021 through y2026 must contain only 0 or 1. ",
    "Correct the activity flags before discovery.",
    call. = FALSE
  )
}

identifier_audit <- roster %>%
  select(
    last, first, area, orcid, openalex_author_id,
    identifier_status, orcid_valid, openalex_id_valid
  )

write_csv(
  identifier_audit,
  file.path(out_dir, "roster_identifier_audit.csv")
)

if (any(identifier_audit$identifier_status == "Invalid identifier")) {
  stop(
    "One or more roster identifiers are invalid. ",
    "Review output/roster_identifier_audit.csv.",
    call. = FALSE
  )
}

duplicate_faculty_keys <- roster %>%
  count(faculty_key, name = "n") %>%
  filter(n > 1)

duplicate_orcids <- roster %>%
  filter(!is.na(orcid)) %>%
  count(orcid, name = "n") %>%
  filter(n > 1)

duplicate_openalex_ids <- roster %>%
  filter(!is.na(openalex_author_id)) %>%
  count(openalex_author_id, name = "n") %>%
  filter(n > 1)

if (
  nrow(duplicate_faculty_keys) > 0 ||
    nrow(duplicate_orcids) > 0 ||
    nrow(duplicate_openalex_ids) > 0
) {
  stop(
    "Roster names or identifiers are assigned to multiple faculty. ",
    "Resolve the duplicates before discovery.",
    call. = FALSE
  )
}

# Apply the roster's annual activity flags to the curated publication rows.
# Rows outside an active CSU year remain in the data for traceability, but the
# flag makes them ineligible for reporting in stages 03 and 04.
pubs <- pubs %>%
  select(-affiliated_at_publication, -affiliation_status) %>%
  left_join(
    roster_active_year,
    by = c("faculty_key", "year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    affiliation_match_missing = is.na(affiliated_at_publication),
    affiliation_status = case_when(
      affiliation_match_missing ~ "No matching roster faculty-year",
      affiliated_at_publication == 1L ~ "Active CSU affiliation",
      TRUE ~ "Outside active CSU affiliation"
    ),
    affiliated_at_publication = coalesce(
      affiliated_at_publication,
      0L
    )
  )

publication_affiliation_audit <- pubs %>%
  filter(affiliated_at_publication != 1L) %>%
  select(
    last, first, area, year, title_short, venue, doi,
    publication_key, affiliation_status, affiliation_match_missing
  ) %>%
  arrange(last, year, title_short)

write_csv(
  publication_affiliation_audit,
  file.path(out_dir, "publication_affiliation_audit.csv")
)

if (any(pubs$affiliation_match_missing)) {
  stop(
    "At least one curated publication has no matching roster faculty-year. ",
    "Review output/publication_affiliation_audit.csv.",
    call. = FALSE
  )
}

pubs <- pubs %>% select(-affiliation_match_missing)


# ==============================================================================
# 4. Audit the curated DOI keys before making API requests
# ==============================================================================

audit_doi_integrity <- function(df) {
  df %>%
    filter(!is.na(doi_clean)) %>%
    group_by(doi_clean) %>%
    summarize(
      n_rows   = n(),
      n_years  = n_distinct(year, na.rm = TRUE),
      n_titles = n_distinct(title_short, na.rm = TRUE),
      n_venues = n_distinct(venue, na.rm = TRUE),
      years    = paste(sort(unique(na.omit(year))), collapse = "; "),
      titles   = paste(unique(na.omit(title_short)), collapse = " | "),
      venues   = paste(unique(na.omit(venue)), collapse = " | "),
      faculty  = paste(sort(unique(last)), collapse = "; "),
      .groups  = "drop"
    ) %>%
    filter(n_years > 1 | n_titles > 1 | n_venues > 1) %>%
    arrange(desc(n_years), desc(n_titles), desc(n_venues))
}

doi_integrity_curated <- audit_doi_integrity(pubs)

write_csv(
  doi_integrity_curated,
  file.path(out_dir, "doi_integrity_audit.csv")
)

if (nrow(doi_integrity_curated) > 0) {
  stop(
    nrow(doi_integrity_curated),
    " DOI keys have conflicting years, titles, or venues. ",
    "Review output/doi_integrity_audit.csv before continuing.",
    call. = FALSE
  )
}


# ==============================================================================
# 5. Discover works by verified roster identifiers
# ==============================================================================

discovery_cache_path <- file.path(out_dir, "oa_discovery_raw.rds")

discovery_signature <- roster %>%
  arrange(faculty_key) %>%
  transmute(
    signature_piece = str_c(
      faculty_key,
      coalesce(orcid, ""),
      coalesce(openalex_author_id, ""),
      sep = "|"
    )
  ) %>%
  pull(signature_piece) %>%
  paste(collapse = "||") %>%
  str_c(
    paste(range(DISCOVERY_YEARS), collapse = "-"),
    .,
    sep = "||"
  )

use_discovery_cache <- FALSE

if (!REFRESH_DISCOVERY && file.exists(discovery_cache_path)) {
  discovery_cache <- readRDS(discovery_cache_path)
  use_discovery_cache <- is.list(discovery_cache) &&
    identical(discovery_cache$signature, discovery_signature)
}

if (use_discovery_cache) {
  message("Reading cached faculty discovery response.")
  works_by_query <- discovery_cache$works_by_query
  query_meta     <- discovery_cache$query_meta
} else {
  works_by_query <- list()
  query_meta     <- tibble(
    query_key = character(),
    faculty_key = character(),
    query_basis = character()
  )

  roster_to_query <- roster %>% filter(has_identifier)

  for (i in seq_len(nrow(roster_to_query))) {
    person <- roster_to_query[i, ]

    if (!is.na(person$openalex_author_id)) {
      query_key <- str_c(person$faculty_key, "__openalex_id")
      message(
        "Discovering works for ",
        person$first,
        " ",
        person$last,
        " by OpenAlex ID."
      )

      works_by_query[[query_key]] <- fetch_oa(list(
        entity = "works",
        author.id = person$openalex_author_id,
        from_publication_date = str_c(min(DISCOVERY_YEARS), "-01-01"),
        to_publication_date = str_c(max(DISCOVERY_YEARS), "-12-31"),
        output = "list",
        verbose = FALSE
      ))

      query_meta <- bind_rows(
        query_meta,
        tibble(
          query_key = query_key,
          faculty_key = person$faculty_key,
          query_basis = "OpenAlex author ID query"
        )
      )
    }

    if (!is.na(person$orcid)) {
      query_key <- str_c(person$faculty_key, "__orcid")
      message(
        "Discovering works for ",
        person$first,
        " ",
        person$last,
        " by ORCID."
      )

      works_by_query[[query_key]] <- fetch_oa(list(
        entity = "works",
        author.orcid = person$orcid,
        from_publication_date = str_c(min(DISCOVERY_YEARS), "-01-01"),
        to_publication_date = str_c(max(DISCOVERY_YEARS), "-12-31"),
        output = "list",
        verbose = FALSE
      ))

      query_meta <- bind_rows(
        query_meta,
        tibble(
          query_key = query_key,
          faculty_key = person$faculty_key,
          query_basis = "ORCID query"
        )
      )
    }
  }

  saveRDS(
    list(
      signature = discovery_signature,
      works_by_query = works_by_query,
      query_meta = query_meta,
      cached_at = Sys.time()
    ),
    discovery_cache_path
  )
}

works_by_query <- map(works_by_query, unique_works)

query_index_raw <- imap_dfr(works_by_query, function(works, query_key) {
  if (length(works) == 0) return(tibble())

  tibble(
    query_key = query_key,
    oa_id = map_chr(
      works,
      ~ clean_openalex_work_id(.x$id %||% NA_character_)
    )
  ) %>%
    filter(!is.na(oa_id))
})

if (nrow(query_index_raw) == 0) {
  query_index <- tibble(
    oa_id = character(),
    faculty_key = character(),
    query_basis = character()
  )
} else {
  query_index <- query_index_raw %>%
    left_join(query_meta, by = "query_key", relationship = "many-to-one") %>%
    distinct(oa_id, faculty_key, query_basis)
}

discovery_works <- unique_works(
  purrr::flatten(unname(works_by_query))
)

if (length(discovery_works) > 0) {
  discovery_meta <- map_dfr(discovery_works, parse_work_meta) %>%
    filter(oa_year %in% DISCOVERY_YEARS) %>%
    distinct(oa_id, .keep_all = TRUE)

  discovery_authors <- map_dfr(discovery_works, parse_work_authors) %>%
    filter(publication_key %in% discovery_meta$publication_key) %>%
    distinct()
} else {
  discovery_meta <- tibble()
  discovery_authors <- tibble()
}


# ==============================================================================
# 6. Match discovered works to rostered faculty and append new rows
# ==============================================================================

new_publication_rows <- pubs[0, ]
discovery_append_audit <- tibble()
discovery_affiliation_exclusions <- tibble()

if (nrow(discovery_meta) > 0) {
  query_pairs <- query_index %>%
    inner_join(
      discovery_meta %>% select(oa_id, publication_key),
      by = "oa_id",
      relationship = "many-to-one"
    ) %>%
    transmute(
      publication_key,
      faculty_key,
      author_match_rule = query_basis,
      match_priority = if_else(
        query_basis == "OpenAlex author ID query",
        1L,
        2L
      ),
      direct_query_match = 1L
    )

  author_rows <- discovery_authors %>%
    distinct(
      publication_key, au_key, au_id, au_orcid,
      au_display_name, au_name_norm
    )

  matches_by_openalex_id <- author_rows %>%
    filter(!is.na(au_id)) %>%
    inner_join(
      roster %>%
        filter(!is.na(openalex_author_id)) %>%
        select(faculty_key, openalex_author_id),
      by = c("au_id" = "openalex_author_id"),
      relationship = "many-to-one"
    ) %>%
    transmute(
      publication_key,
      faculty_key,
      author_match_rule = "OpenAlex author ID in authorship",
      match_priority = 1L,
      direct_query_match = 0L
    )

  matches_by_orcid <- author_rows %>%
    filter(!is.na(au_orcid)) %>%
    inner_join(
      roster %>%
        filter(!is.na(orcid)) %>%
        select(faculty_key, orcid),
      by = c("au_orcid" = "orcid"),
      relationship = "many-to-one"
    ) %>%
    transmute(
      publication_key,
      faculty_key,
      author_match_rule = "ORCID in authorship",
      match_priority = 2L,
      direct_query_match = 0L
    )

  matches_by_exact_name <- author_rows %>%
    filter(!is.na(au_name_norm)) %>%
    inner_join(
      roster %>% select(faculty_key),
      by = c("au_name_norm" = "faculty_key"),
      relationship = "many-to-one"
    ) %>%
    transmute(
      publication_key,
      faculty_key,
      author_match_rule = "Exact normalized roster name in authorship",
      match_priority = 4L,
      direct_query_match = 0L
    )

  roster_work_matches <- bind_rows(
    query_pairs,
    matches_by_openalex_id,
    matches_by_orcid,
    matches_by_exact_name
  ) %>%
    group_by(publication_key, faculty_key) %>%
    arrange(match_priority, desc(direct_query_match), .by_group = TRUE) %>%
    summarize(
      author_match_rule = first(author_match_rule),
      direct_query_match = max(direct_query_match),
      .groups = "drop"
    )

  dare_author_summary <- roster_work_matches %>%
    left_join(
      roster %>% select(faculty_key, last),
      by = "faculty_key",
      relationship = "many-to-one"
    ) %>%
    group_by(publication_key) %>%
    summarize(
      n_dare_authors = n_distinct(faculty_key),
      dare_coauthors_names = paste(sort(unique(last)), collapse = ";"),
      .groups = "drop"
    )

  discovered_rows <- roster_work_matches %>%
    left_join(
      roster %>% select(faculty_key, last, first, area),
      by = "faculty_key",
      relationship = "many-to-one"
    ) %>%
    left_join(
      discovery_meta,
      by = "publication_key",
      relationship = "many-to-one"
    ) %>%
    left_join(
      dare_author_summary,
      by = "publication_key",
      relationship = "many-to-one"
    ) %>%
    mutate(
      year                  = oa_year,
      type                  = map_openalex_type(oa_type),
      index_class           = NA_character_,
      student_coauthor      = "U",
      dare_coauthors        = as.integer(n_dare_authors > 1),
      title_short           = str_squish(oa_title),
      venue                 = oa_venue,
      doi                   = doi_clean,
      source_file           = "OpenAlex discovery",
      openalex_work_id      = oa_id,
      openalex_type         = oa_type,
      type_mapping_rule     = type_mapping_rule(oa_type),
      type_requires_review  = openalex_type_requires_review(oa_type),
      extension_output      = default_extension_output(oa_type),
      added_from_openalex   = 1L,
      added_from_coauthor   = as.integer(direct_query_match == 0L),
      discovery_date        = as.character(Sys.Date()),
      edge_2026             = as.integer(year == 2026),
      doi_clean             = clean_doi(doi),
      title_year_key        = make_title_year_key(title_short, year)
    )

  # When the department already has the publication for another faculty member,
  # inherit the curated metadata before creating the missing faculty row.
  existing_meta_by_doi <- pubs %>%
    filter(!is.na(doi_clean)) %>%
    group_by(doi_clean) %>%
    summarize(
      meta_publication_key = first(publication_key),
      meta_year            = first(year),
      meta_type            = first(type),
      meta_index_class     = first(index_class),
      meta_title_short     = first(title_short),
      meta_venue           = first(venue),
      meta_source_file     = first(source_file),
      meta_extension       = first(extension_output),
      .groups = "drop"
    )

  existing_meta_by_title <- pubs %>%
    filter(!is.na(title_year_key)) %>%
    group_by(title_year_key) %>%
    filter(n_distinct(publication_key) == 1) %>%
    summarize(
      title_meta_publication_key = first(publication_key),
      title_meta_year            = first(year),
      title_meta_type            = first(type),
      title_meta_index_class     = first(index_class),
      title_meta_title_short     = first(title_short),
      title_meta_venue           = first(venue),
      title_meta_source_file     = first(source_file),
      title_meta_extension       = first(extension_output),
      .groups = "drop"
    )

  discovered_rows <- discovered_rows %>%
    left_join(
      existing_meta_by_doi,
      by = "doi_clean",
      relationship = "many-to-one"
    ) %>%
    left_join(
      existing_meta_by_title,
      by = "title_year_key",
      relationship = "many-to-one"
    ) %>%
    mutate(
      inherited_existing_record = !is.na(meta_publication_key) |
        !is.na(title_meta_publication_key),
      publication_key = coalesce(
        meta_publication_key,
        title_meta_publication_key,
        publication_key
      ),
      year = coalesce(meta_year, title_meta_year, year),
      type = coalesce(meta_type, title_meta_type, type),
      index_class = coalesce(
        meta_index_class,
        title_meta_index_class,
        index_class
      ),
      title_short = coalesce(
        meta_title_short,
        title_meta_title_short,
        title_short
      ),
      venue = coalesce(meta_venue, title_meta_venue, venue),
      source_file = coalesce(
        meta_source_file,
        title_meta_source_file,
        source_file
      ),
      extension_output = coalesce(
        meta_extension,
        title_meta_extension,
        extension_output
      ),
      type_mapping_rule = if_else(
        inherited_existing_record,
        "Inherited from existing curated DARE publication",
        type_mapping_rule
      ),
      type_requires_review = if_else(
        inherited_existing_record,
        0L,
        type_requires_review
      ),
      added_from_coauthor = if_else(
        inherited_existing_record,
        1L,
        added_from_coauthor
      ),
      edge_2026 = as.integer(year == 2026),
      title_year_key = make_title_year_key(title_short, year)
    ) %>%
    left_join(
      roster_active_year,
      by = c("faculty_key", "year"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      affiliation_status = case_when(
        is.na(affiliated_at_publication) ~
          "No matching roster faculty-year",
        affiliated_at_publication == 1L ~
          "Active CSU affiliation",
        TRUE ~ "Outside active CSU affiliation"
      ),
      affiliated_at_publication = coalesce(
        affiliated_at_publication,
        0L
      )
    )

  existing_doi_pairs <- pubs %>%
    filter(!is.na(doi_clean)) %>%
    transmute(pair = str_c(faculty_key, "|doi:", doi_clean)) %>%
    pull(pair)

  existing_oa_pairs <- pubs %>%
    filter(!is.na(openalex_work_id)) %>%
    transmute(pair = str_c(faculty_key, "|oa:", openalex_work_id)) %>%
    pull(pair)

  existing_title_pairs <- pubs %>%
    filter(!is.na(title_year_key)) %>%
    transmute(pair = str_c(faculty_key, "|", title_year_key)) %>%
    pull(pair)

  discovered_rows <- discovered_rows %>%
    mutate(
      doi_pair = if_else(
        !is.na(doi_clean),
        str_c(faculty_key, "|doi:", doi_clean),
        NA_character_
      ),
      oa_pair = if_else(
        !is.na(openalex_work_id),
        str_c(faculty_key, "|oa:", openalex_work_id),
        NA_character_
      ),
      title_pair = if_else(
        !is.na(title_year_key),
        str_c(faculty_key, "|", title_year_key),
        NA_character_
      ),
      already_in_curated = doi_pair %in% existing_doi_pairs |
        oa_pair %in% existing_oa_pairs |
        title_pair %in% existing_title_pairs
    )

  discovery_append_audit <- discovered_rows %>%
    transmute(
      last, first, area, year, title_short, venue, doi,
      openalex_work_id, openalex_type, type,
      type_mapping_rule, type_requires_review,
      author_match_rule, affiliated_at_publication,
      affiliation_status, already_in_curated,
      action = case_when(
        affiliated_at_publication != 1L ~
          "Outside active CSU affiliation; not appended",
        already_in_curated ~ "Already present; not appended",
        TRUE ~ "Appended to updated publication file"
      )
    ) %>%
    arrange(last, year, title_short)

  discovery_affiliation_exclusions <- discovery_append_audit %>%
    filter(affiliated_at_publication != 1L)

  new_publication_rows <- discovered_rows %>%
    filter(
      !already_in_curated,
      affiliated_at_publication == 1L
    ) %>%
    select(any_of(names(pubs)))
}

write_csv(
  discovery_append_audit,
  file.path(out_dir, "openalex_discovery_audit.csv")
)

write_csv(
  discovery_affiliation_exclusions,
  file.path(out_dir, "openalex_affiliation_exclusions.csv")
)

updated_pubs <- bind_rows(pubs, new_publication_rows) %>%
  mutate(
    faculty_key = normalize_name(str_c(first, last, sep = " ")),
    doi_clean = clean_doi(doi),
    doi_clean = if_else(is_valid_doi(doi_clean), doi_clean, NA_character_),
    openalex_work_id = clean_openalex_work_id(openalex_work_id),
    title_year_key = make_title_year_key(title_short, year),
    publication_key = coalesce(
      publication_key,
      make_publication_key(
        doi_clean,
        title_short,
        year,
        openalex_work_id
      )
    )
  )

# Recompute internal-collaboration fields for both existing and appended rows.
internal_collaboration <- updated_pubs %>%
  filter(
    affiliated_at_publication == 1L,
    !is.na(publication_key)
  ) %>%
  group_by(publication_key) %>%
  summarize(
    n_dare_authors = n_distinct(faculty_key),
    dare_names = paste(sort(unique(last)), collapse = ";"),
    .groups = "drop"
  )

updated_pubs <- updated_pubs %>%
  left_join(
    internal_collaboration,
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    dare_coauthors = if_else(
      !is.na(n_dare_authors),
      as.integer(n_dare_authors > 1),
      dare_coauthors
    ),
    dare_coauthors_names = if_else(
      !is.na(n_dare_authors) & n_dare_authors > 1,
      dare_names,
      NA_character_
    ),
    edge_2026 = as.integer(year == 2026)
  ) %>%
  select(-n_dare_authors, -dare_names)

doi_integrity_updated <- audit_doi_integrity(updated_pubs)

write_csv(
  doi_integrity_updated,
  file.path(out_dir, "doi_integrity_audit.csv")
)

if (nrow(doi_integrity_updated) > 0) {
  stop(
    nrow(doi_integrity_updated),
    " DOI keys conflict after discovery. ",
    "Review output/doi_integrity_audit.csv.",
    call. = FALSE
  )
}

# ==============================================================================
# 7. Enrich every valid DOI using a structured cache
# ==============================================================================

pubs <- updated_pubs
dois <- pubs %>%
  filter(!is.na(doi_clean)) %>%
  pull(doi_clean) %>%
  unique()

doi_cache_path <- file.path(out_dir, "oa_works_raw.rds")

if (REFRESH_DOI_CACHE && file.exists(doi_cache_path)) {
  file.remove(doi_cache_path)
}

doi_cache <- list(
  requested_dois = character(),
  works = list()
)

if (file.exists(doi_cache_path)) {
  cached_object <- readRDS(doi_cache_path)

  if (
    is.list(cached_object) &&
      all(c("requested_dois", "works") %in% names(cached_object))
  ) {
    doi_cache <- cached_object
  } else {
    # One-time migration from the old cache, which stored only the work list.
    migrated_works <- cached_object
    migrated_dois <- map_chr(
      migrated_works,
      ~ clean_doi(.x$doi %||% NA_character_)
    )
    migrated_dois <- unique(migrated_dois[is_valid_doi(migrated_dois)])

    doi_cache <- list(
      requested_dois = migrated_dois,
      works = migrated_works
    )
  }
}

cached_work_dois <- map_chr(
  doi_cache$works,
  ~ clean_doi(.x$doi %||% NA_character_)
)

keep_cached_work <- is_valid_doi(cached_work_dois) & cached_work_dois %in% dois
doi_works <- doi_cache$works[keep_cached_work]

dois_to_fetch <- if (REFRESH_DOI_CACHE) {
  dois
} else {
  setdiff(dois, doi_cache$requested_dois)
}

doi_chunks <- split(
  dois_to_fetch,
  ceiling(seq_along(dois_to_fetch) / 50)
)

if (length(doi_chunks) > 0) {
  for (i in seq_along(doi_chunks)) {
    message("Fetching DOI chunk ", i, " of ", length(doi_chunks), ".")

    fetched <- fetch_oa(list(
      entity = "works",
      doi = doi_chunks[[i]],
      output = "list",
      verbose = FALSE
    ))

    doi_works <- c(doi_works, fetched)
    Sys.sleep(0.5)
  }
}

doi_works <- unique_works(doi_works)

saveRDS(
  list(
    requested_dois = dois,
    works = doi_works,
    cached_at = Sys.time()
  ),
  doi_cache_path
)


# ==============================================================================
# 8. Build work-level and coauthor-level OpenAlex tables
# ==============================================================================

# Discovery may contain works without DOIs. Combine both sources before parsing.
all_works <- unique_works(c(doi_works, discovery_works))

# A discovered work can inherit the publication key of an existing curated row
# (for example, when the DOI is missing but the normalized title and year match).
# Carry that canonical key into the parsed OpenAlex tables so metadata and
# authorships stay linked to every faculty-publication row.
oa_key_map <- pubs %>%
  filter(!is.na(openalex_work_id), !is.na(publication_key)) %>%
  distinct(oa_id = openalex_work_id, publication_key)

oa_key_map_conflicts <- oa_key_map %>%
  count(oa_id, name = "n_publication_keys") %>%
  filter(n_publication_keys > 1)

write_csv(
  oa_key_map_conflicts,
  file.path(out_dir, "openalex_id_key_conflicts.csv")
)

if (nrow(oa_key_map_conflicts) > 0) {
  stop(
    "An OpenAlex work ID maps to more than one canonical publication key. ",
    "Review output/openalex_id_key_conflicts.csv.",
    call. = FALSE
  )
}

oa_key_map <- oa_key_map %>%
  rename(canonical_publication_key = publication_key)

pub_openalex_raw <- map_dfr(all_works, parse_work_meta) %>%
  left_join(oa_key_map, by = "oa_id", relationship = "many-to-one") %>%
  mutate(
    publication_key = coalesce(
      canonical_publication_key,
      publication_key
    )
  ) %>%
  select(-canonical_publication_key) %>%
  filter(publication_key %in% pubs$publication_key)

oa_key_audit <- pub_openalex_raw %>%
  count(publication_key, name = "n_openalex_records") %>%
  filter(n_openalex_records > 1)

write_csv(
  oa_key_audit,
  file.path(out_dir, "openalex_work_key_audit.csv")
)

if (nrow(oa_key_audit) > 0) {
  stop(
    "Multiple OpenAlex works map to the same publication key. ",
    "Review output/openalex_work_key_audit.csv.",
    call. = FALSE
  )
}

pub_openalex <- pub_openalex_raw %>%
  distinct(publication_key, .keep_all = TRUE)

unresolved_dois <- setdiff(dois, pub_openalex$doi_clean)

missing_dois <- pubs %>%
  filter(doi_clean %in% unresolved_dois) %>%
  distinct(doi_clean, title_short, venue, year)

write_csv(missing_dois, file.path(out_dir, "oa_missing_dois.csv"))

coauthors_long <- map_dfr(all_works, parse_work_authors) %>%
  left_join(oa_key_map, by = "oa_id", relationship = "many-to-one") %>%
  mutate(
    publication_key = coalesce(
      canonical_publication_key,
      publication_key
    )
  ) %>%
  select(-canonical_publication_key) %>%
  filter(publication_key %in% pubs$publication_key) %>%
  select(
    publication_key, oa_id, doi_clean, au_key, au_id, au_display_name,
    au_name_norm, au_orcid, author_position, is_corresponding,
    institution, ror, country_code, institution_type, raw_affiliation
  ) %>%
  distinct()


# ==============================================================================
# 9. Canonical publication year and graduate-coauthor matching
# ==============================================================================

# The curated/input year remains authoritative. OpenAlex year is retained only
# to identify disagreements.
publication_year <- pubs %>%
  filter(!is.na(publication_key)) %>%
  left_join(
    pub_openalex %>% select(publication_key, oa_year),
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  group_by(publication_key) %>%
  summarize(
    year_canonical = first(year),
    n_years_input  = n_distinct(year, na.rm = TRUE),
    oa_year        = first(oa_year),
    year_conflict  = n_years_input > 1,
    year_disagrees = !is.na(oa_year) &
      !is.na(year_canonical) &
      oa_year != year_canonical,
    .groups = "drop"
  )

if (any(publication_year$year_conflict)) {
  stop(
    "A publication key maps to multiple input years. ",
    "Review output/doi_integrity_audit.csv.",
    call. = FALSE
  )
}

conferred <- read_excel(
  file.path(in_dir, "conferred_degrees.xlsx"),
  sheet = "conferred"
)

grads <- conferred %>%
  transmute(
    grad_last  = `Last Name`,
    grad_first = `First Name`,
    program    = Program,
    term_code  = `Academic Period Graduation`,
    grad_year  = suppressWarnings(
      as.integer(str_sub(as.character(term_code), 1, 4))
    ),
    name_norm  = normalize_name(str_c(`First Name`, `Last Name`, sep = " ")),
    last_norm  = normalize_name(`Last Name`),
    first_init = str_sub(normalize_name(`First Name`), 1, 1)
  )

graduate_name_audit <- grads %>%
  count(name_norm, name = "n_degree_records") %>%
  filter(n_degree_records > 1)

write_csv(
  graduate_name_audit,
  file.path(out_dir, "graduate_name_audit.csv")
)

# Multiple records for one normalized graduate name are collapsed. The latest
# conferred year is used because student status is supported when at least one
# recorded completion year is at or after the publication year.
grads_by_name <- grads %>%
  group_by(name_norm) %>%
  summarize(
    grad_last  = first(grad_last),
    grad_first = first(grad_first),
    program    = paste(sort(unique(na.omit(program))), collapse = "; "),
    grad_year  = {
      if (all(is.na(grad_year))) NA_integer_ else max(grad_year, na.rm = TRUE)
    },
    last_norm  = first(last_norm),
    first_init = first(first_init),
    n_degree_records = n(),
    .groups = "drop"
  )

coauthor_people <- coauthors_long %>%
  distinct(au_key, au_id, au_display_name, au_name_norm)

match_exact <- coauthor_people %>%
  inner_join(
    grads_by_name,
    by = c("au_name_norm" = "name_norm"),
    relationship = "many-to-one"
  ) %>%
  mutate(match_type = "Exact normalized name")

match_probable <- coauthor_people %>%
  mutate(
    au_last = word(au_name_norm, -1),
    au_first_init = str_sub(word(au_name_norm, 1), 1, 1)
  ) %>%
  inner_join(
    grads_by_name,
    by = c(
      "au_last" = "last_norm",
      "au_first_init" = "first_init"
    ),
    relationship = "many-to-many"
  ) %>%
  anti_join(match_exact %>% distinct(au_key), by = "au_key") %>%
  mutate(match_type = "Last name + first initial; review required")

write_csv(
  match_probable,
  file.path(out_dir, "coauthor_match_review.csv")
)

coauthor_grad_match <- coauthors_long %>%
  inner_join(
    match_exact %>%
      select(
        au_key, au_name_norm, grad_last, grad_first,
        program, grad_year, n_degree_records, match_type
      ),
    by = c("au_key", "au_name_norm"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    publication_year %>% select(publication_key, year_canonical),
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    student_status = case_when(
      is.na(grad_year) | is.na(year_canonical) ~ "Unknown",
      year_canonical <= grad_year              ~ "Student at publication",
      TRUE                                     ~ "Alumni coauthor"
    )
  ) %>%
  arrange(grad_last, year_canonical)

write_csv(
  coauthor_grad_match,
  file.path(out_dir, "coauthor_grad_match.csv")
)

student_flag_oa <- coauthor_grad_match %>%
  group_by(publication_key) %>%
  summarize(
    n_grad_coauthors = n_distinct(au_key),
    grad_coauthors = paste(
      sort(unique(na.omit(au_display_name))),
      collapse = "; "
    ),
    any_student_at_pub = any(student_status == "Student at publication"),
    .groups = "drop"
  )

cv_student_publication <- pubs %>%
  filter(!is.na(publication_key)) %>%
  group_by(publication_key) %>%
  summarize(
    student_cv = case_when(
      any(student_coauthor == "Y", na.rm = TRUE) ~ "Y",
      any(student_coauthor == "N", na.rm = TRUE) ~ "N",
      TRUE                                        ~ "U"
    ),
    student_cv_conflict = n_distinct(
      na.omit(student_coauthor)
    ) > 1,
    .groups = "drop"
  )


# ==============================================================================
# 10. Map journal impact factors without multiplying publication rows
# ==============================================================================

if_wide <- read_excel(
  file.path(in_dir, "journal_impact_factors_2021_2026.xlsx"),
  sheet = "Impact Factors"
)

if (!"Journal" %in% names(if_wide)) {
  stop("The impact-factor sheet has no `Journal` column.", call. = FALSE)
}

if_columns <- names(if_wide)[str_detect(names(if_wide), "^IF_\\d{4}$")]

if (length(if_columns) == 0) {
  stop("The impact-factor sheet has no IF_YYYY columns.", call. = FALSE)
}

if_long_raw <- if_wide %>%
  pivot_longer(
    cols = all_of(if_columns),
    names_to = "if_year",
    values_to = "impact_factor"
  ) %>%
  mutate(
    if_year = as.integer(str_remove(if_year, "^IF_")),
    journal_norm = normalize_journal(Journal)
  ) %>%
  filter(!is.na(impact_factor), !is.na(journal_norm), journal_norm != "") %>%
  distinct(Journal, journal_norm, if_year, impact_factor)

if_key_audit <- if_long_raw %>%
  group_by(journal_norm, if_year) %>%
  summarize(
    n_source_rows = n(),
    n_journals = n_distinct(Journal),
    n_values = n_distinct(impact_factor),
    journals = paste(sort(unique(Journal)), collapse = " | "),
    impact_factors = paste(sort(unique(impact_factor)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_source_rows > 1) %>%
  mutate(
    resolution = if_else(
      n_values == 1,
      "Collapsed to one identical journal-year value",
      "Conflicting values; execution stopped"
    )
  )

write_csv(if_key_audit, file.path(out_dir, "if_key_audit.csv"))

if (any(if_key_audit$n_values > 1)) {
  stop(
    "The impact-factor lookup has conflicting values for at least one ",
    "normalized journal-year key. Review output/if_key_audit.csv.",
    call. = FALSE
  )
}

# Collapse exact duplicates and journal-title variants only after confirming
# that each normalized journal-year key has one impact-factor value. This makes
# the downstream lookup explicitly many-to-one and prevents row multiplication.
if_long <- if_long_raw %>%
  group_by(journal_norm, if_year) %>%
  summarize(
    Journal = paste(sort(unique(Journal)), collapse = " | "),
    impact_factor = first(impact_factor),
    .groups = "drop"
  )

pubs_norm <- pubs %>%
  mutate(journal_norm = normalize_journal(venue))

n_rows_before_if <- nrow(pubs_norm)

if_strict <- pubs_norm %>%
  left_join(
    if_long %>% select(journal_norm, if_year, impact_factor),
    by = c("journal_norm", "year" = "if_year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    impact_factor = if_else(type == "JA", impact_factor, NA_real_),
    if_year_used = if_else(!is.na(impact_factor), year, NA_integer_),
    if_match_rule = if_else(
      !is.na(impact_factor),
      "Exact journal-year",
      NA_character_
    )
  )

if (nrow(if_strict) != n_rows_before_if) {
  stop("The exact impact-factor join changed the publication row count.")
}

if (USE_NEAREST_IF_YEAR) {
  nearest_if <- if_strict %>%
    filter(type == "JA", is.na(impact_factor), !is.na(journal_norm)) %>%
    distinct(journal_norm, year) %>%
    inner_join(
      if_long %>% select(journal_norm, if_year, impact_factor),
      by = "journal_norm",
      relationship = "many-to-many"
    ) %>%
    mutate(year_gap = abs(if_year - year)) %>%
    group_by(journal_norm, year) %>%
    arrange(year_gap, if_year, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(
      journal_norm,
      year,
      impact_factor_fb = impact_factor,
      if_year_used_fb  = if_year,
      if_match_rule_fb = str_c("Nearest year (", if_year, ")")
    )

  publications_if <- if_strict %>%
    left_join(
      nearest_if,
      by = c("journal_norm", "year"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      impact_factor = coalesce(impact_factor, impact_factor_fb),
      if_year_used  = coalesce(if_year_used, if_year_used_fb),
      if_match_rule = coalesce(if_match_rule, if_match_rule_fb)
    ) %>%
    select(-ends_with("_fb"))
} else {
  publications_if <- if_strict
}

if (nrow(publications_if) != nrow(pubs)) {
  stop("Impact-factor mapping changed the publication row count.")
}

# A journal article with a matched impact factor is verified as index class a.
# No-match rows remain blank because class b requires separate peer-review and
# indexing verification.
publications_if <- publications_if %>%
  mutate(
    index_class_missing = is.na(index_class) |
      str_squish(index_class) == "",
    index_class = case_when(
      !index_class_missing ~ index_class,
      type == "JA" & !is.na(impact_factor) ~ "a",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-index_class_missing)

# The impact-factor joins preserve row order and have already been checked for
# multiplication, so copy the verified index class back to the updated
# publication data before writing its single final version.
updated_pubs$index_class <- publications_if$index_class
pubs$index_class <- publications_if$index_class

write_csv(
  updated_pubs %>% select(-doi_clean, -faculty_key, -title_year_key),
  updated_publications_path
)

message(
  "Appended ",
  nrow(new_publication_rows),
  " faculty-publication rows and wrote ",
  updated_publications_path,
  "."
)


# ==============================================================================
# 11. Assemble and write the enriched publication file
# ==============================================================================

publications_enriched <- publications_if %>%
  left_join(
    pub_openalex %>% select(-doi_clean),
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    student_flag_oa,
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    cv_student_publication,
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    publication_year %>%
      select(
        publication_key, year_canonical,
        year_conflict, year_disagrees
      ),
    by = "publication_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    if_match_rule = case_when(
      type != "JA"                 ~ "Not applicable",
      is.na(if_match_rule)          ~ "No IF match",
      TRUE                          ~ if_match_rule
    ),
    student_cv = coalesce(student_cv, student_coauthor, "U"),
    student_coauthor_final = case_when(
      student_cv == "Y" ~ "Y",
      any_student_at_pub %in% TRUE ~ "Y",
      student_cv == "N" ~ "N",
      TRUE              ~ "U"
    ),
    student_evidence = case_when(
      student_cv == "Y" &
        any_student_at_pub %in% TRUE ~
        "CV and registrar",
      student_cv == "Y" ~ "CV only",
      any_student_at_pub %in% TRUE ~
        "Registrar only",
      student_cv == "N" ~ "Neither",
      TRUE              ~ "Undetermined"
    ),
    in_reporting_window = year %in% REPORT_YEARS,
    edge_2026 = as.integer(year == 2026)
  )

if (nrow(publications_enriched) != nrow(pubs)) {
  stop(
    "Publication enrichment changed the row count from ",
    nrow(pubs),
    " to ",
    nrow(publications_enriched),
    ". Check the preceding joins.",
    call. = FALSE
  )
}

journal_if_audit <- publications_enriched %>%
  count(
    venue, journal_norm, type, if_match_rule,
    name = "n_faculty_publication_rows"
  ) %>%
  arrange(if_match_rule, desc(n_faculty_publication_rows))

openalex_type_mapping_audit <- publications_enriched %>%
  filter(added_from_openalex == 1L) %>%
  count(
    openalex_type,
    type,
    type_mapping_rule,
    type_requires_review,
    name = "n_faculty_publication_rows"
  ) %>%
  arrange(desc(type_requires_review), openalex_type)

write_csv(
  journal_if_audit,
  file.path(out_dir, "journal_if_audit.csv")
)
write_csv(
  openalex_type_mapping_audit,
  file.path(out_dir, "openalex_type_mapping_audit.csv")
)
write_csv(
  pub_openalex,
  file.path(out_dir, "pub_openalex.csv")
)
write_csv(
  coauthors_long,
  file.path(out_dir, "coauthors_long.csv")
)
write_csv(
  publications_enriched %>%
    select(-faculty_key, -title_year_key),
  file.path(out_dir, "publications_enriched.csv")
)

message(
  "Stage 02 complete. Input faculty-publication rows: ",
  nrow(pubs) - nrow(new_publication_rows),
  ". Appended rows: ",
  nrow(new_publication_rows),
  ". Enriched rows: ",
  nrow(publications_enriched),
  "."
)

# ==============================================================================
# End of script
# ==============================================================================
