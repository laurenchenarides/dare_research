# ==============================================================================
# Script Name:  03_build_analysis_file.R
# Author:       Lauren Chenarides
# Purpose:      Join appointment information to the stage 02 publication file
#               and build analysis-ready faculty-publication, publication,
#               faculty-year, and department-year files for Section 4.
#
# Run this script from anywhere inside the dare_research repository.
#
# Counting rules implemented here:
#   - Collection window: 2021-2026.
#   - Complete-year reporting window: 2021-2025.
#   - Faculty-publication rows enter analysis only when the roster marks the
#     faculty member active in the publication year.
#   - Countable research outputs are JA, BC, BK, RP, and CP rows that are not
#     extension outputs and do not require review.
#   - A JA row is countable only when index_class is a or b.
#   - Department publication totals deduplicate on publication_key.
#   - Faculty-publication totals give each active DARE author one credit.
#   - Appointment shares are held constant across the collection window.
#   - Unknown appointment shares remain unknown. They are never converted to
#     zero in the research-FTE denominator.
#   - The primary research-FTE denominator includes active TT faculty only.
#   - John Ritten remains Continuing as listed. Parallel sensitivity fields
#     show the result if he were classified as TT.
#
# Roster-only TT faculty retained during their active roster years:
#   Hayley Chouinard, Marshall Frasier, Alexandra Hill, Becca Jablonski,
#   Dale Manning, and Greg Perry.
#
# Main inputs:
#   output/publications_enriched.csv
#   data/appointment_splits.csv
#   data/roster.csv
#
# Main outputs:
#   output/faculty_appointments.csv
#   output/pubs_analysis.csv
#   output/publication_level_analysis.csv
#   output/faculty_year_panel.csv
#   output/dept_year_summary.csv
#   output/dept_reporting_summary.csv
#   output/appointment_join_audit.csv
#   output/appointment_split_audit.csv
#   output/publication_affiliation_audit_stage03.csv
#   output/publication_classification_audit.csv
#   output/publication_count_audit.csv
#   output/publication_exclusion_audit.csv
#   output/research_fte_completeness_audit.csv
#
# Packages:
#   dplyr, tidyr, stringr, readr, purrr, tibble
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})


# ==============================================================================
# 0. Configuration
# ==============================================================================

REPORT_YEARS     <- 2021:2025
COLLECTION_YEARS <- 2021:2026

COUNTABLE_TYPES <- c("JA", "BC", "BK", "RP", "CP")

ROSTER_ONLY_TT_NAMES <- c(
  "Hayley Chouinard",
  "Marshall Frasier",
  "Alexandra Hill",
  "Becca Jablonski",
  "Dale Manning",
  "Greg Perry"
)


# ==============================================================================
# 1. General helpers and project paths
# ==============================================================================

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

normalize_name <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:][:space:]]", " ") %>%
    str_squish() %>%
    str_trim()
}

clean_character <- function(x) {
  value <- str_squish(as.character(x))
  value[value == ""] <- NA_character_
  value
}

assert_columns <- function(df, required, file_label) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      file_label,
      " is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

safe_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

first_character <- function(x) {
  value <- clean_character(x)
  value <- value[!is.na(value)]
  if (length(value) == 0) NA_character_ else value[[1]]
}

first_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  value <- value[!is.na(value)]
  if (length(value) == 0) NA_real_ else value[[1]]
}

PROJECT_ROOT <- find_project_root()
in_dir       <- file.path(PROJECT_ROOT, "data")
out_dir      <- file.path(PROJECT_ROOT, "output")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

publications_path <- file.path(out_dir, "publications_enriched.csv")
appointments_path <- file.path(in_dir, "appointment_splits.csv")
roster_path       <- file.path(in_dir, "roster.csv")


# ==============================================================================
# 2. Read and validate the roster
# ==============================================================================

roster <- read_csv(roster_path, show_col_types = FALSE)

active_year_columns <- str_c("y", COLLECTION_YEARS)

required_roster_columns <- c(
  "last", "first", "area",
  "research_pct", "teaching_pct", "outreach_pct",
  active_year_columns
)

assert_columns(roster, required_roster_columns, "data/roster.csv")

roster <- roster %>%
  mutate(
    last          = str_to_title(str_squish(last)),
    first         = str_to_title(str_squish(first)),
    faculty_key   = normalize_name(str_c(first, last, sep = " ")),
    research_pct  = suppressWarnings(as.numeric(research_pct)),
    teaching_pct  = suppressWarnings(as.numeric(teaching_pct)),
    outreach_pct  = suppressWarnings(as.numeric(outreach_pct))
  )

duplicate_roster_keys <- roster %>%
  count(faculty_key, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_roster_keys) > 0) {
  stop(
    "roster.csv contains duplicate normalized faculty names.",
    call. = FALSE
  )
}

roster_long <- roster %>%
  select(
    faculty_key, last, first, area,
    research_pct, teaching_pct, outreach_pct,
    all_of(active_year_columns)
  ) %>%
  pivot_longer(
    cols = all_of(active_year_columns),
    names_to = "active_year_column",
    values_to = "active"
  ) %>%
  mutate(
    year   = as.integer(str_remove(active_year_column, "^y")),
    active = suppressWarnings(as.integer(active))
  ) %>%
  select(-active_year_column)

invalid_activity_flags <- roster_long %>%
  filter(is.na(active) | !active %in% c(0L, 1L))

if (nrow(invalid_activity_flags) > 0) {
  stop(
    "Roster activity columns y2021 through y2026 must contain only 0 or 1.",
    call. = FALSE
  )
}


# ==============================================================================
# 3. Clean appointment records and add known roster-only TT faculty
# ==============================================================================

appt_raw <- read_csv(appointments_path, show_col_types = FALSE)

appointment_source_columns <- c(
  "Candidate Name",
  "Faculty Type",
  "Faculty Rank",
  "Effort Distribution: Instruction, Advising, and Mentoring",
  "Effort Distribution: Research, Scholarship, and Creative Activity",
  "Effort Distribution: University/Professional/Public Service and Outreach"
)

assert_columns(
  appt_raw,
  appointment_source_columns,
  "data/appointment_splits.csv"
)

extra_appointment_columns <- setdiff(
  names(appt_raw),
  appointment_source_columns
)

if (length(extra_appointment_columns) > 1) {
  stop(
    "appointment_splits.csv has more than one unrecognized column: ",
    paste(extra_appointment_columns, collapse = ", "),
    call. = FALSE
  )
}

if (length(extra_appointment_columns) == 1) {
  appt_raw <- appt_raw %>%
    mutate(departed_raw = .data[[extra_appointment_columns[[1]]]])
} else {
  appt_raw <- appt_raw %>% mutate(departed_raw = NA_character_)
}

appointment_file <- appt_raw %>%
  transmute(
    candidate_name = clean_character(`Candidate Name`),
    faculty_key = normalize_name(candidate_name),
    faculty_type = clean_character(`Faculty Type`),
    faculty_rank = clean_character(`Faculty Rank`),
    pct_teaching = suppressWarnings(as.numeric(
      `Effort Distribution: Instruction, Advising, and Mentoring`
    )),
    pct_research = suppressWarnings(as.numeric(
      `Effort Distribution: Research, Scholarship, and Creative Activity`
    )),
    pct_service = suppressWarnings(as.numeric(
      `Effort Distribution: University/Professional/Public Service and Outreach`
    )),
    departed_flag = !is.na(clean_character(departed_raw)),
    is_tt_as_listed = faculty_type == "Tenured/Tenure Track",
    appointment_source = "appointment_splits.csv",
    classification_source = "appointment_splits.csv"
  ) %>%
  mutate(
    split_complete = !is.na(pct_teaching) &
      !is.na(pct_research) &
      !is.na(pct_service),
    split_sum = if_else(
      split_complete,
      pct_teaching + pct_research + pct_service,
      NA_real_
    ),
    split_sums_100 = if_else(
      split_complete,
      near(split_sum, 100),
      NA
    )
  )

duplicate_appointment_keys <- appointment_file %>%
  count(faculty_key, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_appointment_keys) > 0) {
  stop(
    "appointment_splits.csv contains duplicate normalized faculty names.",
    call. = FALSE
  )
}

expected_roster_only_keys <- normalize_name(ROSTER_ONLY_TT_NAMES)

roster_missing_appointment <- roster %>%
  anti_join(
    appointment_file %>% select(faculty_key),
    by = "faculty_key"
  )

unexpected_roster_only <- roster_missing_appointment %>%
  filter(!faculty_key %in% expected_roster_only_keys)

if (nrow(unexpected_roster_only) > 0) {
  stop(
    "The roster contains faculty missing from appointment_splits.csv who are ",
    "not in ROSTER_ONLY_TT_NAMES: ",
    paste(
      str_c(unexpected_roster_only$first, unexpected_roster_only$last),
      collapse = ", "
    ),
    call. = FALSE
  )
}

roster_only_appointments <- roster_missing_appointment %>%
  transmute(
    candidate_name = str_c(first, last, sep = " "),
    faculty_key,
    faculty_type = "Tenured/Tenure Track",
    faculty_rank = NA_character_,
    pct_teaching = teaching_pct,
    pct_research = research_pct,
    pct_service = outreach_pct,
    departed_flag = y2026 == 0L,
    is_tt_as_listed = TRUE,
    appointment_source = "roster.csv",
    classification_source = "Roster-only TT list approved for active years",
    split_complete = !is.na(pct_teaching) &
      !is.na(pct_research) &
      !is.na(pct_service),
    split_sum = if_else(
      split_complete,
      pct_teaching + pct_research + pct_service,
      NA_real_
    ),
    split_sums_100 = if_else(
      split_complete,
      near(split_sum, 100),
      NA
    )
  )

faculty_appointments <- bind_rows(
  appointment_file,
  roster_only_appointments
) %>%
  mutate(
    is_tt_if_ritten_tt = is_tt_as_listed |
      faculty_key == normalize_name("John Ritten")
  ) %>%
  arrange(candidate_name)

if (anyDuplicated(faculty_appointments$faculty_key) > 0) {
  stop("The combined appointment table is not unique by faculty_key.")
}

write_csv(
  faculty_appointments,
  file.path(out_dir, "faculty_appointments.csv")
)


# ==============================================================================
# 4. Audit appointment coverage and effort splits
# ==============================================================================

appointment_join_audit <- full_join(
  roster %>%
    select(faculty_key, roster_last = last, roster_first = first, area) %>%
    mutate(in_roster = TRUE),
  faculty_appointments %>%
    select(
      faculty_key, candidate_name, faculty_type,
      appointment_source, classification_source
    ) %>%
    mutate(in_appointments = TRUE),
  by = "faculty_key",
  relationship = "one-to-one"
) %>%
  mutate(
    in_roster = replace_na(in_roster, FALSE),
    in_appointments = replace_na(in_appointments, FALSE),
    audit_note = case_when(
      in_roster & in_appointments ~ "Matched",
      in_roster ~ "Roster faculty missing appointment record",
      TRUE ~ "Appointment record outside analysis roster"
    )
  ) %>%
  arrange(audit_note, roster_last, candidate_name)

write_csv(
  appointment_join_audit,
  file.path(out_dir, "appointment_join_audit.csv")
)

if (any(appointment_join_audit$audit_note ==
        "Roster faculty missing appointment record")) {
  stop(
    "At least one roster faculty member has no appointment record. ",
    "Review output/appointment_join_audit.csv.",
    call. = FALSE
  )
}

roster_appointments <- roster %>%
  left_join(
    faculty_appointments,
    by = "faculty_key",
    relationship = "many-to-one"
  )

appointment_split_audit <- roster_appointments %>%
  transmute(
    last, first, faculty_key,
    roster_research_pct = research_pct,
    appointment_research_pct = pct_research,
    roster_teaching_pct = teaching_pct,
    appointment_teaching_pct = pct_teaching,
    roster_outreach_pct = outreach_pct,
    appointment_service_pct = pct_service,
    research_conflict = !is.na(roster_research_pct) &
      !is.na(appointment_research_pct) &
      roster_research_pct != appointment_research_pct,
    teaching_conflict = !is.na(roster_teaching_pct) &
      !is.na(appointment_teaching_pct) &
      roster_teaching_pct != appointment_teaching_pct,
    service_conflict = !is.na(roster_outreach_pct) &
      !is.na(appointment_service_pct) &
      roster_outreach_pct != appointment_service_pct,
    research_share_missing = is.na(appointment_research_pct),
    split_complete,
    split_sum,
    split_sums_100,
    appointment_source
  ) %>%
  mutate(
    any_conflict = research_conflict |
      teaching_conflict |
      service_conflict
  ) %>%
  filter(
    any_conflict |
      research_share_missing |
      !replace_na(split_complete, FALSE) |
      split_sums_100 %in% FALSE
  ) %>%
  arrange(desc(any_conflict), last)

write_csv(
  appointment_split_audit,
  file.path(out_dir, "appointment_split_audit.csv")
)

if (any(appointment_split_audit$any_conflict)) {
  stop(
    "Roster and appointment files contain conflicting effort shares. ",
    "Review output/appointment_split_audit.csv.",
    call. = FALSE
  )
}


# ==============================================================================
# 5. Read stage 02 publications and verify faculty-year affiliation
# ==============================================================================

pubs_stage02 <- read_csv(publications_path, show_col_types = FALSE)

required_publication_columns <- c(
  "last", "first", "area", "year", "type", "index_class",
  "student_coauthor_final", "dare_coauthors", "dare_coauthors_names",
  "title_short", "venue", "doi", "source_file", "publication_key",
  "affiliated_at_publication", "affiliation_status",
  "extension_output", "type_requires_review", "edge_2026",
  "cited_by_count", "impact_factor", "added_from_coauthor"
)

assert_columns(
  pubs_stage02,
  required_publication_columns,
  "output/publications_enriched.csv"
)

pubs_stage02 <- pubs_stage02 %>%
  mutate(
    last = str_to_title(str_squish(last)),
    first = str_to_title(str_squish(first)),
    faculty_key = normalize_name(str_c(first, last, sep = " ")),
    year = suppressWarnings(as.integer(year)),
    type = str_to_upper(clean_character(type)),
    index_class = str_to_lower(clean_character(index_class)),
    publication_key = clean_character(publication_key),
    affiliated_at_publication = suppressWarnings(
      as.integer(affiliated_at_publication)
    ),
    extension_output = suppressWarnings(as.integer(extension_output)),
    type_requires_review = suppressWarnings(
      as.integer(type_requires_review)
    ),
    edge_2026 = as.integer(year == 2026),
    cited_by_count = suppressWarnings(as.numeric(cited_by_count)),
    impact_factor = suppressWarnings(as.numeric(impact_factor)),
    student_coauthor_final = str_to_upper(
      clean_character(student_coauthor_final)
    ),
    added_from_coauthor = suppressWarnings(as.integer(added_from_coauthor))
  )

if (any(!pubs_stage02$year %in% COLLECTION_YEARS)) {
  stop(
    "publications_enriched.csv contains years outside 2021-2026.",
    call. = FALSE
  )
}

if (any(is.na(pubs_stage02$publication_key))) {
  stop(
    "publications_enriched.csv contains missing publication_key values.",
    call. = FALSE
  )
}

duplicate_faculty_publications <- pubs_stage02 %>%
  count(faculty_key, publication_key, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_faculty_publications) > 0) {
  write_csv(
    duplicate_faculty_publications,
    file.path(out_dir, "duplicate_faculty_publication_audit.csv")
  )
  stop(
    "Duplicate faculty-publication pairs found. Review ",
    "output/duplicate_faculty_publication_audit.csv.",
    call. = FALSE
  )
}

pubs_with_activity <- pubs_stage02 %>%
  left_join(
    roster_long %>%
      select(faculty_key, year, roster_active = active),
    by = c("faculty_key", "year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    affiliation_match_missing = is.na(roster_active),
    stage02_affiliation_missing = is.na(affiliated_at_publication),
    affiliation_mismatch = !affiliation_match_missing &
      !stage02_affiliation_missing &
      affiliated_at_publication != roster_active
  )

publication_affiliation_audit <- pubs_with_activity %>%
  filter(
    affiliation_match_missing |
      stage02_affiliation_missing |
      affiliation_mismatch |
      affiliated_at_publication != 1L
  ) %>%
  select(
    last, first, area, year, title_short, venue, doi,
    publication_key, affiliated_at_publication,
    roster_active, affiliation_status,
    affiliation_match_missing, stage02_affiliation_missing,
    affiliation_mismatch
  ) %>%
  arrange(last, year, title_short)

write_csv(
  publication_affiliation_audit,
  file.path(out_dir, "publication_affiliation_audit_stage03.csv")
)

if (
  any(pubs_with_activity$affiliation_match_missing, na.rm = TRUE) ||
    any(pubs_with_activity$stage02_affiliation_missing, na.rm = TRUE) ||
    any(pubs_with_activity$affiliation_mismatch, na.rm = TRUE)
) {
  stop(
    "Stage 02 affiliation fields do not match roster.csv. Review ",
    "output/publication_affiliation_audit_stage03.csv.",
    call. = FALSE
  )
}


# ==============================================================================
# 6. Apply explicit publication counting rules
# ==============================================================================

pubs_classified <- pubs_with_activity %>%
  mutate(
    in_collection_window = year %in% COLLECTION_YEARS,
    in_reporting_window = year %in% REPORT_YEARS,
    publication_count_status = case_when(
      affiliated_at_publication != 1L ~
        "Outside active CSU affiliation",
      !in_collection_window ~ "Outside collection window",
      is.na(type_requires_review) | type_requires_review == 1L ~
        "Publication type requires review",
      !type %in% COUNTABLE_TYPES ~ "Publication type not countable",
      extension_output == 1L ~ "Extension or non-refereed output",
      is.na(extension_output) ~ "Extension status unverified",
      type == "JA" & !index_class %in% c("a", "b") ~
        "Journal index class unverified",
      TRUE ~ "Countable research output"
    ),
    research_countable = publication_count_status ==
      "Countable research output",
    journal_article_countable = research_countable & type == "JA",
    verified_peer_reviewed = research_countable &
      type %in% c("JA", "CP")
  )

publication_count_audit <- pubs_classified %>%
  count(
    year, type, index_class, publication_count_status,
    name = "n_faculty_publication_rows"
  ) %>%
  arrange(year, publication_count_status, type, index_class)

publication_exclusion_audit <- pubs_classified %>%
  filter(!research_countable) %>%
  select(
    last, first, area, year, type, index_class,
    extension_output, type_requires_review,
    title_short, venue, doi, publication_key,
    publication_count_status
  ) %>%
  arrange(publication_count_status, last, year, title_short)

write_csv(
  publication_count_audit,
  file.path(out_dir, "publication_count_audit.csv")
)
write_csv(
  publication_exclusion_audit,
  file.path(out_dir, "publication_exclusion_audit.csv")
)

# Only active-year rows enter the analysis files. Excluded publication types
# remain in pubs_analysis with a count-status flag so stage 04 can report other
# scholarly output without treating it as research-countable output.
pubs_analysis <- pubs_classified %>%
  filter(affiliated_at_publication == 1L) %>%
  left_join(
    roster_appointments %>%
      select(
        faculty_key, faculty_type, faculty_rank,
        is_tt_as_listed, is_tt_if_ritten_tt,
        pct_research, pct_teaching, pct_service,
        split_complete, split_sum, split_sums_100,
        departed_flag, appointment_source,
        classification_source
      ),
    by = "faculty_key",
    relationship = "many-to-one"
  ) %>%
  mutate(
    individual_research_fte = pct_research / 100,
    is_tt = is_tt_as_listed,
    research_fte = individual_research_fte
  )

if (any(is.na(pubs_analysis$faculty_type))) {
  stop(
    "At least one active publication has no appointment classification.",
    call. = FALSE
  )
}


# ==============================================================================
# 7. Validate publication-level classifications and collapse unique works
# ==============================================================================

publication_classification_audit <- pubs_analysis %>%
  group_by(publication_key) %>%
  summarize(
    n_years = n_distinct(year, na.rm = TRUE),
    n_types = n_distinct(type, na.rm = TRUE),
    n_index_classes = n_distinct(index_class, na.rm = TRUE),
    n_extension_values = n_distinct(extension_output, na.rm = TRUE),
    n_review_values = n_distinct(type_requires_review, na.rm = TRUE),
    years = paste(sort(unique(na.omit(year))), collapse = "; "),
    types = paste(sort(unique(na.omit(type))), collapse = "; "),
    index_classes = paste(
      sort(unique(na.omit(index_class))),
      collapse = "; "
    ),
    faculty = paste(sort(unique(last)), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(
    n_years > 1 |
      n_types > 1 |
      n_index_classes > 1 |
      n_extension_values > 1 |
      n_review_values > 1
  ) %>%
  arrange(desc(n_years), desc(n_types), publication_key)

write_csv(
  publication_classification_audit,
  file.path(out_dir, "publication_classification_audit.csv")
)

if (nrow(publication_classification_audit) > 0) {
  stop(
    "Faculty rows disagree on a publication classification. Review ",
    "output/publication_classification_audit.csv.",
    call. = FALSE
  )
}

publication_level_analysis <- pubs_analysis %>%
  filter(research_countable) %>%
  group_by(publication_key) %>%
  summarize(
    year = first(year),
    type = first(type),
    index_class = first_character(index_class),
    title_short = first_character(title_short),
    venue = first_character(venue),
    doi = first_character(doi),
    cited_by_count = first_numeric(cited_by_count),
    impact_factor = first_numeric(impact_factor),
    n_dare_authors = n_distinct(faculty_key),
    faculty_names = paste(
      sort(unique(str_c(first, last, sep = " "))),
      collapse = "; "
    ),
    faculty_last_names = paste(sort(unique(last)), collapse = "; "),
    research_areas = paste(sort(unique(area)), collapse = "; "),
    cross_area_publication = n_distinct(area) > 1,
    internal_collaboration = n_dare_authors > 1,
    student_coauthored = any(student_coauthor_final == "Y", na.rm = TRUE),
    added_from_coauthor_any = any(added_from_coauthor == 1L, na.rm = TRUE),
    journal_article_countable = first(journal_article_countable),
    verified_peer_reviewed = first(verified_peer_reviewed),
    in_reporting_window = year %in% REPORT_YEARS,
    edge_2026 = as.integer(year == 2026),
    .groups = "drop"
  ) %>%
  arrange(year, type, title_short)

write_csv(pubs_analysis, file.path(out_dir, "pubs_analysis.csv"))
write_csv(
  publication_level_analysis,
  file.path(out_dir, "publication_level_analysis.csv")
)


# ==============================================================================
# 8. Build the active faculty-year panel
# ==============================================================================

faculty_output_counts <- pubs_analysis %>%
  filter(research_countable) %>%
  group_by(faculty_key, year) %>%
  summarize(
    n_faculty_publication_rows = n(),
    n_journal_articles = sum(journal_article_countable),
    n_verified_peer_reviewed = sum(verified_peer_reviewed),
    n_index_a = sum(type == "JA" & index_class == "a", na.rm = TRUE),
    n_index_b = sum(type == "JA" & index_class == "b", na.rm = TRUE),
    n_books = sum(type == "BK", na.rm = TRUE),
    n_book_chapters = sum(type == "BC", na.rm = TRUE),
    n_research_reports = sum(type == "RP", na.rm = TRUE),
    n_conference_proceedings = sum(type == "CP", na.rm = TRUE),
    n_student_coauthored = sum(
      student_coauthor_final == "Y",
      na.rm = TRUE
    ),
    n_internal_collaboration_rows = sum(
      dare_coauthors == 1L,
      na.rm = TRUE
    ),
    n_outputs_with_citation_data = sum(!is.na(cited_by_count)),
    total_citations = safe_sum(cited_by_count),
    mean_impact_factor = safe_mean(impact_factor),
    .groups = "drop"
  )

faculty_year_panel <- roster_long %>%
  filter(active == 1L) %>%
  left_join(
    roster_appointments %>%
      select(
        faculty_key, faculty_type, faculty_rank,
        is_tt_as_listed, is_tt_if_ritten_tt,
        pct_research, pct_teaching, pct_service,
        split_complete, split_sum, split_sums_100,
        departed_flag, appointment_source,
        classification_source
      ),
    by = "faculty_key",
    relationship = "many-to-one"
  ) %>%
  left_join(
    faculty_output_counts,
    by = c("faculty_key", "year"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(
      c(
        n_faculty_publication_rows,
        n_journal_articles,
        n_verified_peer_reviewed,
        n_index_a,
        n_index_b,
        n_books,
        n_book_chapters,
        n_research_reports,
        n_conference_proceedings,
        n_student_coauthored,
        n_internal_collaboration_rows,
        n_outputs_with_citation_data
      ),
      ~ replace_na(as.integer(.x), 0L)
    ),
    total_citations = if_else(
      n_faculty_publication_rows == 0L,
      0,
      total_citations
    ),
    individual_research_fte = pct_research / 100,
    is_tt = is_tt_as_listed,
    research_fte = individual_research_fte,
    tt_research_fte_as_listed = case_when(
      !is_tt_as_listed ~ 0,
      is.na(pct_research) ~ NA_real_,
      TRUE ~ pct_research / 100
    ),
    tt_research_fte_if_ritten_tt = case_when(
      !is_tt_if_ritten_tt ~ 0,
      is.na(pct_research) ~ NA_real_,
      TRUE ~ pct_research / 100
    ),
    output_per_individual_research_fte = if_else(
      !is.na(individual_research_fte) & individual_research_fte > 0,
      n_faculty_publication_rows / individual_research_fte,
      NA_real_
    ),
    complete_reporting_year = year %in% REPORT_YEARS,
    edge_2026 = as.integer(year == 2026)
  ) %>%
  arrange(last, first, year)

write_csv(
  faculty_year_panel,
  file.path(out_dir, "faculty_year_panel.csv")
)


# ==============================================================================
# 9. Build department-year publication and capacity summaries
# ==============================================================================

department_publications_by_year <- publication_level_analysis %>%
  group_by(year) %>%
  summarize(
    n_distinct_publications = n(),
    n_faculty_publication_rows = sum(n_dare_authors),
    internal_collaboration_gap =
      n_faculty_publication_rows - n_distinct_publications,
    n_internal_collaboration_publications = sum(
      internal_collaboration,
      na.rm = TRUE
    ),
    n_cross_area_publications = sum(
      cross_area_publication,
      na.rm = TRUE
    ),
    n_student_coauthored_publications = sum(
      student_coauthored,
      na.rm = TRUE
    ),
    n_distinct_journal_articles = sum(
      journal_article_countable,
      na.rm = TRUE
    ),
    n_distinct_verified_peer_reviewed = sum(
      verified_peer_reviewed,
      na.rm = TRUE
    ),
    n_distinct_index_a = sum(
      type == "JA" & index_class == "a",
      na.rm = TRUE
    ),
    n_distinct_index_b = sum(
      type == "JA" & index_class == "b",
      na.rm = TRUE
    ),
    n_distinct_books = sum(type == "BK", na.rm = TRUE),
    n_distinct_book_chapters = sum(type == "BC", na.rm = TRUE),
    n_distinct_research_reports = sum(type == "RP", na.rm = TRUE),
    n_distinct_conference_proceedings = sum(
      type == "CP",
      na.rm = TRUE
    ),
    n_publications_with_citation_data = sum(!is.na(cited_by_count)),
    distinct_publication_citations = safe_sum(cited_by_count),
    mean_publication_impact_factor = safe_mean(impact_factor),
    .groups = "drop"
  )

department_capacity_by_year <- faculty_year_panel %>%
  group_by(year) %>%
  summarize(
    headcount_all = n(),
    headcount_tt_as_listed = sum(is_tt_as_listed, na.rm = TRUE),
    headcount_tt_if_ritten_tt = sum(
      is_tt_if_ritten_tt,
      na.rm = TRUE
    ),
    n_all_missing_research_share = sum(is.na(pct_research)),
    n_tt_missing_research_share = sum(
      is_tt_as_listed & is.na(pct_research)
    ),
    n_tt_if_ritten_missing_research_share = sum(
      is_tt_if_ritten_tt & is.na(pct_research)
    ),
    research_fte_all_known = sum(
      replace_na(individual_research_fte, 0),
      na.rm = TRUE
    ),
    research_fte_tt_known_as_listed = sum(
      replace_na(tt_research_fte_as_listed, 0),
      na.rm = TRUE
    ),
    research_fte_tt_known_if_ritten_tt = sum(
      replace_na(tt_research_fte_if_ritten_tt, 0),
      na.rm = TRUE
    ),
    research_fte_tt_complete_as_listed = if_else(
      n_tt_missing_research_share == 0L,
      research_fte_tt_known_as_listed,
      NA_real_
    ),
    research_fte_tt_complete_if_ritten_tt = if_else(
      n_tt_if_ritten_missing_research_share == 0L,
      research_fte_tt_known_if_ritten_tt,
      NA_real_
    ),
    .groups = "drop"
  )

dept_year_summary <- tibble(year = COLLECTION_YEARS) %>%
  left_join(
    department_capacity_by_year,
    by = "year",
    relationship = "one-to-one"
  ) %>%
  left_join(
    department_publications_by_year,
    by = "year",
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(
      c(
        n_distinct_publications,
        n_faculty_publication_rows,
        internal_collaboration_gap,
        n_internal_collaboration_publications,
        n_cross_area_publications,
        n_student_coauthored_publications,
        n_distinct_journal_articles,
        n_distinct_verified_peer_reviewed,
        n_distinct_index_a,
        n_distinct_index_b,
        n_distinct_books,
        n_distinct_book_chapters,
        n_distinct_research_reports,
        n_distinct_conference_proceedings,
        n_publications_with_citation_data
      ),
      ~ replace_na(as.integer(.x), 0L)
    ),
    complete_reporting_year = year %in% REPORT_YEARS,
    edge_2026 = as.integer(year == 2026),
    headcount_tt = headcount_tt_as_listed,
    research_fte_tt_known = research_fte_tt_known_as_listed,
    research_fte_tt = research_fte_tt_complete_as_listed,
    research_fte_denominator_complete =
      n_tt_missing_research_share == 0L,
    unique_publications_per_tt = if_else(
      complete_reporting_year & headcount_tt_as_listed > 0,
      n_distinct_publications / headcount_tt_as_listed,
      NA_real_
    ),
    verified_peer_reviewed_per_tt = if_else(
      complete_reporting_year & headcount_tt_as_listed > 0,
      n_distinct_verified_peer_reviewed / headcount_tt_as_listed,
      NA_real_
    ),
    unique_publications_per_research_fte = if_else(
      complete_reporting_year &
        !is.na(research_fte_tt_complete_as_listed) &
        research_fte_tt_complete_as_listed > 0,
      n_distinct_publications /
        research_fte_tt_complete_as_listed,
      NA_real_
    ),
    verified_peer_reviewed_per_research_fte = if_else(
      complete_reporting_year &
        !is.na(research_fte_tt_complete_as_listed) &
        research_fte_tt_complete_as_listed > 0,
      n_distinct_verified_peer_reviewed /
        research_fte_tt_complete_as_listed,
      NA_real_
    ),
    unique_publications_per_tt_if_ritten_tt = if_else(
      complete_reporting_year & headcount_tt_if_ritten_tt > 0,
      n_distinct_publications / headcount_tt_if_ritten_tt,
      NA_real_
    ),
    unique_publications_per_research_fte_if_ritten_tt = if_else(
      complete_reporting_year &
        !is.na(research_fte_tt_complete_if_ritten_tt) &
        research_fte_tt_complete_if_ritten_tt > 0,
      n_distinct_publications /
        research_fte_tt_complete_if_ritten_tt,
      NA_real_
    )
  ) %>%
  arrange(year)

dept_reporting_summary <- dept_year_summary %>%
  filter(complete_reporting_year)

write_csv(
  dept_year_summary,
  file.path(out_dir, "dept_year_summary.csv")
)
write_csv(
  dept_reporting_summary,
  file.path(out_dir, "dept_reporting_summary.csv")
)


# ==============================================================================
# 10. Audit incomplete research-FTE denominators
# ==============================================================================

research_fte_completeness_audit <- faculty_year_panel %>%
  filter(is_tt_as_listed, is.na(pct_research)) %>%
  select(
    year, last, first, area, faculty_type,
    pct_research, appointment_source,
    classification_source, complete_reporting_year
  ) %>%
  arrange(year, last)

write_csv(
  research_fte_completeness_audit,
  file.path(out_dir, "research_fte_completeness_audit.csv")
)

message(
  "Stage 03 complete. Active faculty-publication rows: ",
  nrow(pubs_analysis),
  ". Countable faculty-publication rows: ",
  sum(pubs_analysis$research_countable),
  ". Distinct countable publications: ",
  nrow(publication_level_analysis),
  ". Complete reporting years with missing TT research shares: ",
  paste(
    dept_reporting_summary$year[
      !dept_reporting_summary$research_fte_denominator_complete
    ],
    collapse = ", "
  ),
  "."
)

# ==============================================================================
# End of script
# ==============================================================================
