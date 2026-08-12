# ==============================================================================
# Script Name:  04_publication_tables.R
# Author:       Lauren Chenarides
# Last updated: July 2026
#
# Description:
#   Produces the publication and disciplinary-influence tables for Section 4:
#
#   Table D1. Peer-reviewed publication output by year and publication category
#   Table D2. Peer-reviewed publications by research area
#   Table D3. Other scholarly outputs by year
#   Table D4. Citation and publication-impact indicators
#
# Counting rules:
#   1. Unique department publications count each publication once, even when
#      multiple DARE faculty members are coauthors.
#   2. Faculty-publication count gives each DARE faculty author one count.
#   3. Peer-reviewed publication totals exclude extension publications,
#      reports, policy briefs, working papers, media contributions, and other
#      non-peer-reviewed outputs.
#   4. OpenAlex indexing is used as an operational indexing indicator:
#        indexed     = a valid OpenAlex work ID was returned
#        non-indexed = no valid OpenAlex work ID was returned
#
# Inputs:
#   output/pubs_analysis.csv
#   output/faculty_year_panel.csv
#   data/publications_faculty_doi_cleaned.csv
#
# Optional input:
#   data/publication_peer_benchmarks.csv
#
# Outputs:
#   output/table_D1_publications_by_year.csv
#   output/table_D2_publications_by_area.csv
#   output/table_D2_publications_by_area_long.csv
#   output/table_D3_other_scholarly_outputs.csv
#   output/table_D4_impact_indicators.csv
#   output/publication_count_audit.csv
#   output/publication_tables_D1_D4.xlsx
# ==============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(openxlsx)

in_dir  <- "data"
out_dir <- "output"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

WINDOW <- 2021:2025


# ------------------------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------------------------

clean_doi <- function(x) {
  x %>%
    str_remove(regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE)) %>%
    str_trim() %>%
    str_to_lower()
}


normalize_text <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}


# Use DOI as the preferred publication identifier.
#
# For publications without a usable DOI, construct a reproducible fallback from
# title, venue, and year. This prevents all missing-DOI records from collapsing
# into one publication.
make_publication_id <- function(doi_clean, title, venue, year) {
  doi_is_valid <- !is.na(doi_clean) &
    str_detect(doi_clean, "^10\\.\\d{4,9}/\\S+$")
  
  fallback_id <- str_c(
    "fallback:",
    normalize_text(title),
    "|",
    normalize_text(venue),
    "|",
    year
  )
  
  if_else(
    doi_is_valid,
    str_c("doi:", doi_clean),
    fallback_id
  )
}


# Create one label for the three department research areas.
standardize_area <- function(x) {
  case_when(
    str_to_lower(str_squish(x)) %in%
      c("enre", "environmental and resource economics") ~
      "ENRE",
    
    str_to_lower(str_squish(x)) %in%
      c(
        "ag and food",
        "agricultural and food economics",
        "agricultural economics",
        "food economics"
      ) ~
      "Agricultural and Food Economics",
    
    str_to_lower(str_squish(x)) %in%
      c(
        "ag ed",
        "agricultural education",
        "ag education"
      ) ~
      "Agricultural Education",
    
    TRUE ~ NA_character_
  )
}


# Return a scalar sum that remains NA when every input value is missing.
sum_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    sum(x, na.rm = TRUE)
  }
}


mean_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}


# ------------------------------------------------------------------------------
# 2. Read inputs
# ------------------------------------------------------------------------------

pubs_analysis <- read_csv(
  file.path(out_dir, "pubs_analysis.csv"),
  show_col_types = FALSE
)

faculty_year_panel <- read_csv(
  file.path(out_dir, "faculty_year_panel.csv"),
  show_col_types = FALSE
)

# This unfiltered source is needed for Table D3 because the OpenAlex enrichment
# workflow retained journal articles only.
all_outputs <- read_csv(
  file.path(in_dir, "publications_faculty_doi_cleaned.csv"),
  show_col_types = FALSE
)


# ------------------------------------------------------------------------------
# 3. Prepare publication-level and faculty-publication files
# ------------------------------------------------------------------------------

# `pubs_analysis` contains one row per DARE faculty member x publication.
faculty_publications <- pubs_analysis %>%
  mutate(
    doi_clean = clean_doi(doi),
    
    publication_id = make_publication_id(
      doi_clean = doi_clean,
      title     = title_short,
      venue     = venue,
      year      = year
    ),
    
    area_standard = standardize_area(area),
    
    # A returned OpenAlex work ID is used as the indexing indicator.
    indexed_openalex = !is.na(oa_id) & str_squish(oa_id) != "",
    
    # All rows in pubs_analysis came from the JA subset of the curated CV file.
    peer_reviewed = type == "JA"
  ) %>%
  filter(
    year %in% WINDOW,
    peer_reviewed
  )


# Publication-level table: one row per distinct department publication.
#
# A publication can have multiple department areas when faculty from different
# areas coauthor the same paper.
publication_level <- faculty_publications %>%
  group_by(publication_id) %>%
  summarize(
    year = first(year),
    
    doi_clean = first(na.omit(doi_clean)),
    
    title_short = first(title_short),
    
    venue = first(venue),
    
    indexed_openalex = any(indexed_openalex),
    
    cited_by_count = {
      x <- cited_by_count[!is.na(cited_by_count)]
      if (length(x) == 0) NA_real_ else first(x)
    },
    
    impact_factor = {
      x <- impact_factor[!is.na(impact_factor)]
      if (length(x) == 0) NA_real_ else first(x)
    },
    
    if_year_used = {
      x <- if_year_used[!is.na(if_year_used)]
      if (length(x) == 0) NA_integer_ else first(as.integer(x))
    },
    
    n_dare_faculty_authors = n_distinct(last),
    
    dare_faculty_authors = paste(
      sort(unique(str_c(first, last, sep = " "))),
      collapse = "; "
    ),
    
    n_areas = n_distinct(area_standard, na.rm = TRUE),
    
    area_tags = paste(
      sort(unique(na.omit(area_standard))),
      collapse = "; "
    ),
    
    area_category = case_when(
      n_areas > 1 ~ "Cross-area publications",
      n_areas == 1 ~ first(na.omit(area_standard)),
      TRUE ~ "Unclassified"
    ),
    
    .groups = "drop"
  )


# Ensure that each publication identifier has only one year.
publication_id_audit <- faculty_publications %>%
  group_by(publication_id) %>%
  summarize(
    n_rows   = n(),
    n_years  = n_distinct(year),
    n_titles = n_distinct(title_short),
    n_venues = n_distinct(venue),
    years    = paste(sort(unique(year)), collapse = "; "),
    titles   = paste(unique(title_short), collapse = " | "),
    venues   = paste(unique(venue), collapse = " | "),
    .groups  = "drop"
  ) %>%
  filter(
    n_years > 1 |
      n_titles > 1 |
      n_venues > 1
  )

publication_id_audit %>%
  print(n = Inf)

if (nrow(publication_id_audit) > 0) {
  warning(
    nrow(publication_id_audit),
    " publication identifiers map to inconsistent titles, venues, or years. ",
    "Review `publication_count_audit.csv`."
  )
}

write_csv(
  publication_id_audit,
  file.path(out_dir, "publication_count_audit.csv")
)


# ------------------------------------------------------------------------------
# 4. Active TT faculty denominators
# ------------------------------------------------------------------------------

active_tt_by_year <- faculty_year_panel %>%
  filter(
    year %in% WINDOW,
    active == 1,
    is_tt %in% TRUE
  ) %>%
  group_by(year) %>%
  summarize(
    active_tt_faculty = n_distinct(last),
    .groups = "drop"
  )


# Make sure every year appears even if one source has no rows for that year.
year_frame <- tibble(year = WINDOW)

# ------------------------------------------------------------------------------
# Active TT faculty and research-FTE denominators
# ------------------------------------------------------------------------------

tt_denominators <- faculty_year_panel %>%
  filter(
    year %in% WINDOW,
    active == 1,
    is_tt %in% TRUE
  ) %>%
  group_by(year) %>%
  summarize(
    active_tt_faculty = n_distinct(last),
    
    active_tt_research_fte = sum(
      research_fte,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 5. Table D1
# Peer-reviewed publication output by year and publication category
# ------------------------------------------------------------------------------

unique_counts_by_year <- publication_level %>%
  group_by(year) %>%
  summarize(
    indexed_and_peer_reviewed_publications =
      sum(indexed_openalex, na.rm = TRUE),
    
    peer_reviewed_but_nonindexed_publications =
      sum(!indexed_openalex, na.rm = TRUE),
    
    total_qualifying_publications = n(),
    
    unique_department_publications = n(),
    
    .groups = "drop"
  )


faculty_counts_by_year <- faculty_publications %>%
  group_by(year) %>%
  summarize(
    faculty_publication_count = n(),
    
    .groups = "drop"
  )


table_D1 <- tibble(year = WINDOW) %>%
  left_join(
    tt_denominators,
    by = "year",
    relationship = "one-to-one"
  ) %>%
  left_join(
    unique_counts_by_year,
    by = "year",
    relationship = "one-to-one"
  ) %>%
  left_join(
    faculty_counts_by_year,
    by = "year",
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(
      c(
        active_tt_faculty,
        active_tt_research_fte,
        indexed_and_peer_reviewed_publications,
        peer_reviewed_but_nonindexed_publications,
        total_qualifying_publications,
        unique_department_publications,
        faculty_publication_count
      ),
      ~ replace_na(.x, 0)
    ),
    
    # Unweighted per-capita measures
    unique_publications_per_active_tt_faculty = if_else(
      active_tt_faculty > 0,
      unique_department_publications / active_tt_faculty,
      NA_real_
    ),
    
    faculty_publications_per_active_tt_faculty = if_else(
      active_tt_faculty > 0,
      faculty_publication_count / active_tt_faculty,
      NA_real_
    ),
    
    # Research-effort-adjusted measures
    unique_publications_per_tt_research_fte = if_else(
      active_tt_research_fte > 0,
      unique_department_publications / active_tt_research_fte,
      NA_real_
    ),
    
    faculty_publications_per_tt_research_fte = if_else(
      active_tt_research_fte > 0,
      faculty_publication_count / active_tt_research_fte,
      NA_real_
    )
  ) %>%
  mutate(
    across(
      c(
        active_tt_research_fte,
        unique_publications_per_active_tt_faculty,
        faculty_publications_per_active_tt_faculty,
        unique_publications_per_tt_research_fte,
        faculty_publications_per_tt_research_fte
      ),
      ~ round(.x, 2)
    )
  ) %>%
  select(
    year,
    active_tt_faculty,
    active_tt_research_fte,
    indexed_and_peer_reviewed_publications,
    peer_reviewed_but_nonindexed_publications,
    total_qualifying_publications,
    unique_department_publications,
    faculty_publication_count,
    unique_publications_per_active_tt_faculty,
    faculty_publications_per_active_tt_faculty,
    unique_publications_per_tt_research_fte,
    faculty_publications_per_tt_research_fte
  )


write_csv(
  table_D1,
  file.path(out_dir, "table_D1_publications_by_year.csv")
)


# ------------------------------------------------------------------------------
# 6. Table D2
# Peer-reviewed publications by research area
# ------------------------------------------------------------------------------

# Each unique publication is assigned to exactly one mutually exclusive category:
#   - ENRE
#   - Agricultural and Food Economics
#   - Agricultural Education
#   - Cross-area publications
#   - Unclassified
#
# This guarantees that the area columns sum to the department total.
table_D2_long <- publication_level %>%
  count(
    year,
    area_category,
    name = "n_publications"
  ) %>%
  complete(
    year = WINDOW,
    area_category = c(
      "ENRE",
      "Agricultural and Food Economics",
      "Agricultural Education",
      "Cross-area publications",
      "Unclassified"
    ),
    fill = list(n_publications = 0)
  )


table_D2 <- table_D2_long %>%
  pivot_wider(
    names_from = area_category,
    values_from = n_publications,
    values_fill = 0
  ) %>%
  rename(
    enre = ENRE,
    agricultural_and_food_economics =
      `Agricultural and Food Economics`,
    agricultural_education =
      `Agricultural Education`,
    cross_area_publications =
      `Cross-area publications`,
    unclassified_publications =
      Unclassified
  ) %>%
  mutate(
    department_total =
      enre +
      agricultural_and_food_economics +
      agricultural_education +
      cross_area_publications +
      unclassified_publications
  ) %>%
  arrange(year)


write_csv(
  table_D2,
  file.path(out_dir, "table_D2_publications_by_area.csv")
)

write_csv(
  table_D2_long,
  file.path(out_dir, "table_D2_publications_by_area_long.csv")
)


# Optional five-year-period version.
table_D2_five_year <- table_D2 %>%
  summarize(
    period = str_c(min(WINDOW), "-", max(WINDOW)),
    enre = sum(enre),
    agricultural_and_food_economics =
      sum(agricultural_and_food_economics),
    agricultural_education =
      sum(agricultural_education),
    cross_area_publications =
      sum(cross_area_publications),
    unclassified_publications =
      sum(unclassified_publications),
    department_total =
      sum(department_total)
  ) %>%
  select(
    period,
    enre,
    agricultural_and_food_economics,
    agricultural_education,
    cross_area_publications,
    unclassified_publications,
    department_total
  )


# ------------------------------------------------------------------------------
# 7. Table D3
# Other scholarly outputs by year
# ------------------------------------------------------------------------------

# This table must use the original curated file rather than pubs_analysis,
# because the enrichment script filtered to journal articles before OpenAlex.
all_outputs_prepared <- all_outputs %>%
  mutate(
    doi_clean = clean_doi(doi),
    
    publication_id = make_publication_id(
      doi_clean = doi_clean,
      title     = title_short,
      venue     = venue,
      year      = year
    ),
    
    type_clean = str_to_upper(str_squish(type)),
    
    output_category = case_when(
      type_clean %in% c("BK", "BOOK") ~
        "Scholarly books",
      
      type_clean %in% c(
        "EB",
        "EDITED BOOK",
        "EDITED VOLUME",
        "ED"
      ) ~
        "Edited volumes",
      
      type_clean %in% c(
        "BC",
        "CH",
        "BOOK CHAPTER",
        "PEER-REVIEWED BOOK CHAPTER"
      ) ~
        "Peer-reviewed book chapters",
      
      type_clean %in% c(
        "CP",
        "PROC",
        "CONFERENCE PROCEEDING",
        "CONFERENCE PROCEEDINGS"
      ) ~
        "Conference proceedings",
      
      type_clean %in% c(
        "JR",
        "RP",
        "JURIED",
        "OTHER PEER REVIEWED",
        "OTHER JURIED",
        "PR"
      ) ~
        "Other juried or peer-reviewed scholarly works",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    year %in% WINDOW,
    !is.na(output_category)
  )


# Count each scholarly output once at the department level.
table_D3_long <- all_outputs_prepared %>%
  distinct(
    publication_id,
    year,
    output_category
  ) %>%
  count(
    output_category,
    year,
    name = "n_outputs"
  ) %>%
  complete(
    output_category = c(
      "Scholarly books",
      "Edited volumes",
      "Peer-reviewed book chapters",
      "Conference proceedings",
      "Other juried or peer-reviewed scholarly works"
    ),
    year = WINDOW,
    fill = list(n_outputs = 0)
  )


# Rows are output categories; columns are years.
table_D3 <- table_D3_long %>%
  mutate(
    year = str_c("Year_", year)
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = n_outputs,
    values_fill = 0
  ) %>%
  mutate(
    five_year_total = rowSums(
      across(starts_with("Year_")),
      na.rm = TRUE
    )
  )


write_csv(
  table_D3,
  file.path(out_dir, "table_D3_other_scholarly_outputs.csv")
)


# Audit the type codes that were not assigned to Table D3.
type_code_audit <- all_outputs %>%
  mutate(
    type_clean = str_to_upper(str_squish(type))
  ) %>%
  count(
    type_clean,
    sort = TRUE,
    name = "n_rows"
  )

write_csv(
  type_code_audit,
  file.path(out_dir, "publication_type_code_audit.csv")
)


# ------------------------------------------------------------------------------
# 8. Table D4
# Citation and publication-impact indicators
# ------------------------------------------------------------------------------

# Department citation measures are based on unique department publications so a
# paper coauthored by multiple DARE faculty is not counted multiple times.
impact_base <- publication_level %>%
  filter(year %in% WINDOW)


n_unique_publications <- nrow(impact_base)

total_citations_openalex <- sum_or_na(
  impact_base$cited_by_count
)

citations_per_publication <- if_else(
  n_unique_publications > 0,
  total_citations_openalex / n_unique_publications,
  NA_real_
)

percentage_publications_cited <- if_else(
  n_unique_publications > 0,
  100 * mean(
    !is.na(impact_base$cited_by_count) &
      impact_base$cited_by_count > 0
  ),
  NA_real_
)


# Define "highly cited" using the top 10% of the department's own citation
# distribution. This is not a field-normalized benchmark; it is included only
# as a transparent internal descriptive indicator.
citation_90th_percentile <- if (
  all(is.na(impact_base$cited_by_count))
) {
  NA_real_
} else {
  as.numeric(
    quantile(
      impact_base$cited_by_count,
      probs = 0.90,
      na.rm = TRUE,
      names = FALSE
    )
  )
}


n_highly_cited_internal <- if_else(
  is.na(citation_90th_percentile),
  NA_real_,
  sum(
    impact_base$cited_by_count >= citation_90th_percentile,
    na.rm = TRUE
  )
)


percentage_with_impact_factor <- if_else(
  n_unique_publications > 0,
  100 * mean(!is.na(impact_base$impact_factor)),
  NA_real_
)


mean_journal_impact_factor <- mean_or_na(
  impact_base$impact_factor
)


median_journal_impact_factor <- if (
  all(is.na(impact_base$impact_factor))
) {
  NA_real_
} else {
  median(
    impact_base$impact_factor,
    na.rm = TRUE
  )
}


table_D4_department <- tibble(
  indicator = c(
    "Unique peer-reviewed department publications",
    "Total OpenAlex citations",
    "OpenAlex citations per publication",
    "Percentage of publications cited",
    "Field-weighted citation impact",
    "Highly cited publications: internal top 10 percent",
    "Google Scholar citations",
    "Percentage with matched journal impact factor",
    "Mean journal impact factor",
    "Median journal impact factor"
  ),
  
  department_value = c(
    n_unique_publications,
    total_citations_openalex,
    citations_per_publication,
    percentage_publications_cited,
    NA_real_,
    n_highly_cited_internal,
    NA_real_,
    percentage_with_impact_factor,
    mean_journal_impact_factor,
    median_journal_impact_factor
  ),
  
  data_source = c(
    "Curated CV publication dataset",
    "OpenAlex",
    "OpenAlex and curated CV publication dataset",
    "OpenAlex and curated CV publication dataset",
    "Not available in current dataset",
    "OpenAlex; department-internal citation distribution",
    "Not yet supplied",
    "Curated journal impact-factor lookup",
    "Curated journal impact-factor lookup",
    "Curated journal impact-factor lookup"
  ),
  
  coverage_period = str_c(
    min(WINDOW),
    "-",
    max(WINDOW)
  ),
  
  notes = c(
    "Each publication counted once across DARE faculty authors.",
    "Current cumulative citation count at date of OpenAlex retrieval.",
    "Total citations divided by unique department publications.",
    "Share of unique publications with at least one OpenAlex citation.",
    "Requires a field-normalized bibliometric source.",
    str_c(
      "Count at or above the department's 90th percentile citation threshold: ",
      round(citation_90th_percentile, 1),
      ". This is not a peer or field-normalized highly cited indicator."
    ),
    "Populate after faculty-level Google Scholar data are curated.",
    "Share of unique publications with a matched impact factor.",
    "Mean across unique publications with a matched impact factor.",
    "Median across unique publications with a matched impact factor."
  )
)


# ------------------------------------------------------------------------------
# 9. Optional peer benchmarks for Table D4
# ------------------------------------------------------------------------------

# Optional file format:
#
# indicator,peer_median,peer_75th_percentile,department_percentile
#
# The `indicator` text must match the values used in table_D4_department.
peer_path <- file.path(
  in_dir,
  "publication_peer_benchmarks.csv"
)

if (file.exists(peer_path)) {
  
  peer_benchmarks <- read_csv(
    peer_path,
    show_col_types = FALSE
  )
  
  required_peer_columns <- c(
    "indicator",
    "peer_median",
    "peer_75th_percentile",
    "department_percentile"
  )
  
  missing_peer_columns <- setdiff(
    required_peer_columns,
    names(peer_benchmarks)
  )
  
  if (length(missing_peer_columns) > 0) {
    stop(
      "`publication_peer_benchmarks.csv` is missing: ",
      paste(missing_peer_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  table_D4 <- table_D4_department %>%
    left_join(
      peer_benchmarks %>%
        select(all_of(required_peer_columns)),
      by = "indicator",
      relationship = "one-to-one"
    )
  
} else {
  
  table_D4 <- table_D4_department %>%
    mutate(
      peer_median = NA_real_,
      peer_75th_percentile = NA_real_,
      department_percentile = NA_real_
    )
}


table_D4 <- table_D4 %>%
  select(
    indicator,
    department_value,
    peer_median,
    peer_75th_percentile,
    department_percentile,
    data_source,
    coverage_period,
    notes
  ) %>%
  mutate(
    department_value = round(department_value, 2),
    peer_median = round(peer_median, 2),
    peer_75th_percentile = round(peer_75th_percentile, 2),
    department_percentile = round(department_percentile, 1)
  )


write_csv(
  table_D4,
  file.path(out_dir, "table_D4_impact_indicators.csv")
)


# ------------------------------------------------------------------------------
# 10. Validation checks
# ------------------------------------------------------------------------------

# D1 total qualifying publications should equal unique department publications.
stopifnot(
  all(
    table_D1$total_qualifying_publications ==
      table_D1$unique_department_publications
  )
)


# D2 mutually exclusive area categories must sum to the department total.
stopifnot(
  all(
    table_D2$department_total ==
      table_D2$enre +
      table_D2$agricultural_and_food_economics +
      table_D2$agricultural_education +
      table_D2$cross_area_publications +
      table_D2$unclassified_publications
  )
)


# Annual unique publication counts in D1 and D2 should agree.
D1_D2_check <- table_D1 %>%
  select(
    year,
    d1_total = unique_department_publications
  ) %>%
  left_join(
    table_D2 %>%
      select(
        year,
        d2_total = department_total
      ),
    by = "year",
    relationship = "one-to-one"
  ) %>%
  mutate(
    agrees = d1_total == d2_total
  )

stopifnot(
  all(D1_D2_check$agrees)
)


# The faculty-publication count cannot be below the unique publication count.
stopifnot(
  all(
    table_D1$faculty_publication_count >=
      table_D1$unique_department_publications
  )
)


# ------------------------------------------------------------------------------
# 11. Write a single Excel workbook
# ------------------------------------------------------------------------------

workbook_path <- file.path(
  out_dir,
  "publication_tables_D1_D4.xlsx"
)

wb <- createWorkbook()


addWorksheet(wb, "Table D1")
writeDataTable(
  wb,
  "Table D1",
  table_D1,
  tableStyle = "TableStyleMedium2"
)
freezePane(
  wb,
  "Table D1",
  firstRow = TRUE
)
setColWidths(
  wb,
  "Table D1",
  cols = 1:ncol(table_D1),
  widths = "auto"
)


addWorksheet(wb, "Table D2 Annual")
writeDataTable(
  wb,
  "Table D2 Annual",
  table_D2,
  tableStyle = "TableStyleMedium2"
)
freezePane(
  wb,
  "Table D2 Annual",
  firstRow = TRUE
)
setColWidths(
  wb,
  "Table D2 Annual",
  cols = 1:ncol(table_D2),
  widths = "auto"
)


addWorksheet(wb, "Table D2 Five Year")
writeDataTable(
  wb,
  "Table D2 Five Year",
  table_D2_five_year,
  tableStyle = "TableStyleMedium2"
)
setColWidths(
  wb,
  "Table D2 Five Year",
  cols = 1:ncol(table_D2_five_year),
  widths = "auto"
)


addWorksheet(wb, "Table D3")
writeDataTable(
  wb,
  "Table D3",
  table_D3,
  tableStyle = "TableStyleMedium2"
)
freezePane(
  wb,
  "Table D3",
  firstRow = TRUE
)
setColWidths(
  wb,
  "Table D3",
  cols = 1:ncol(table_D3),
  widths = "auto"
)


addWorksheet(wb, "Table D4")
writeDataTable(
  wb,
  "Table D4",
  table_D4,
  tableStyle = "TableStyleMedium2"
)
freezePane(
  wb,
  "Table D4",
  firstRow = TRUE
)
setColWidths(
  wb,
  "Table D4",
  cols = 1:ncol(table_D4),
  widths = c(
    46,
    18,
    16,
    20,
    21,
    38,
    16,
    65
  )
)
setRowHeights(
  wb,
  "Table D4",
  rows = 1:(nrow(table_D4) + 1),
  heights = "auto"
)
addStyle(
  wb,
  "Table D4",
  style = createStyle(wrapText = TRUE, valign = "top"),
  rows = 1:(nrow(table_D4) + 1),
  cols = 1:ncol(table_D4),
  gridExpand = TRUE
)


addWorksheet(wb, "Publication Detail")
writeDataTable(
  wb,
  "Publication Detail",
  publication_level,
  tableStyle = "TableStyleMedium2"
)
freezePane(
  wb,
  "Publication Detail",
  firstRow = TRUE
)
setColWidths(
  wb,
  "Publication Detail",
  cols = 1:ncol(publication_level),
  widths = "auto"
)


addWorksheet(wb, "Validation")
writeDataTable(
  wb,
  "Validation",
  D1_D2_check,
  tableStyle = "TableStyleMedium2"
)
setColWidths(
  wb,
  "Validation",
  cols = 1:ncol(D1_D2_check),
  widths = "auto"
)


saveWorkbook(
  wb,
  workbook_path,
  overwrite = TRUE
)


# ------------------------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------------------------

cat("\nTABLE D1\n")
print(table_D1, n = Inf)

cat("\nTABLE D2: ANNUAL\n")
print(table_D2, n = Inf)

cat("\nTABLE D2: FIVE-YEAR TOTAL\n")
print(table_D2_five_year, n = Inf)

cat("\nTABLE D3\n")
print(table_D3, n = Inf)

cat("\nTABLE D4\n")
print(table_D4, n = Inf)

cat(
  "\nFiles written to: ",
  normalizePath(out_dir),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# Figures
# ------------------------------------------------------------------------------

library(ggplot2)
library(scales)

figure_dir <- file.path(out_dir, "figures")

dir.create(
  figure_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------------------------
# Figure D1. Unique publications versus faculty-publication count
# ------------------------------------------------------------------------------

figure_D1_data <- table_D1 %>%
  select(
    year,
    unique_department_publications,
    faculty_publication_count
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "measure",
    values_to = "publications"
  ) %>%
  mutate(
    measure = recode(
      measure,
      unique_department_publications =
        "Unique department publications",
      faculty_publication_count =
        "Faculty-publication count"
    )
  )


figure_D1 <- ggplot(
  figure_D1_data,
  aes(
    x = factor(year),
    y = publications,
    fill = measure
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.72
  ) +
  labs(
    title = "Peer-reviewed publication output",
    subtitle = paste0(
      min(WINDOW),
      "–",
      max(WINDOW)
    ),
    x = "Year",
    y = "Number of publications",
    fill = NULL,
    caption = paste(
      "Faculty collaboration within the department is common.",
      "\nFaculty-publication count gives each department faculty author credit.",
      "\nUnique department publications count each paper once."
    )
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )


ggsave(
  filename = file.path(
    figure_dir,
    "figure_D1_publication_counts.png"
  ),
  plot = figure_D1,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Figure D2. Unadjusted and research-FTE-adjusted productivity
# ------------------------------------------------------------------------------

figure_D2_data <- table_D1 %>%
  select(
    year,
    unique_publications_per_active_tt_faculty,
    unique_publications_per_tt_research_fte
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "measure",
    values_to = "publications"
  ) %>%
  mutate(
    measure = recode(
      measure,
      unique_publications_per_active_tt_faculty =
        "Per active TT faculty member",
      unique_publications_per_tt_research_fte =
        "Per TT research FTE"
    )
  )


figure_D2 <- ggplot(
  figure_D2_data,
  aes(
    x = factor(year),
    y = publications,
    fill = measure
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.72
  ) +
  labs(
    title = "Unique peer-reviewed publications relative to faculty capacity",
    x = "Year",
    y = "Unique publications",
    fill = NULL,
    caption = paste(
      "The per-faculty measure uses active TT headcount.",
      "\nThe research-FTE measure adjusts for the share of faculty appointments",
      "assigned to research."
    )
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )


ggsave(
  filename = file.path(
    figure_dir,
    "figure_D2_publications_per_faculty_and_fte.png"
  ),
  plot = figure_D2,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Figure D3. Publications by research area
# ------------------------------------------------------------------------------

figure_D3_data <- table_D2_long %>%
  filter(
    area_category != "Unclassified"
  ) %>%
  mutate(
    area_category = factor(
      area_category,
      levels = c(
        "ENRE",
        "Agricultural and Food Economics",
        "Agricultural Education",
        "Cross-area publications"
      )
    )
  )


figure_D3 <- ggplot(
  figure_D3_data,
  aes(
    x = factor(year),
    y = n_publications,
    fill = area_category
  )
) +
  geom_col(
    width = 0.72
  ) +
  labs(
    title = "Unique peer-reviewed publications by research area",
    x = "Year",
    y = "Number of publications",
    fill = "Research area",
    caption = paste(
      "Each publication is counted once.",
      "A publication authored by faculty in more than one area",
      "is classified as cross-area."
    )
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )


ggsave(
  filename = file.path(
    figure_dir,
    "figure_D3_publications_by_area.png"
  ),
  plot = figure_D3,
  width = 8,
  height = 5.5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Figure D4. Indexed versus non-indexed publications
# ------------------------------------------------------------------------------

figure_D4_data <- table_D1 %>%
  select(
    year,
    indexed_and_peer_reviewed_publications,
    peer_reviewed_but_nonindexed_publications
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "index_status",
    values_to = "publications"
  ) %>%
  mutate(
    index_status = recode(
      index_status,
      indexed_and_peer_reviewed_publications =
        "Indexed in OpenAlex",
      peer_reviewed_but_nonindexed_publications =
        "Not indexed in OpenAlex"
    )
  )


figure_D4 <- ggplot(
  figure_D4_data,
  aes(
    x = factor(year),
    y = publications,
    fill = index_status
  )
) +
  geom_col(
    width = 0.72
  ) +
  labs(
    title = "Peer-reviewed publications by OpenAlex indexing status",
    x = "Year",
    y = "Number of publications",
    fill = NULL,
    caption = paste(
      "Indexing is defined operationally as having a matched OpenAlex work ID.",
      "\nAbsence from OpenAlex does not necessarily mean that a publication",
      "is absent from every bibliographic index."
    )
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )


ggsave(
  filename = file.path(
    figure_dir,
    "figure_D4_indexing_status.png"
  ),
  plot = figure_D4,
  width = 8,
  height = 5,
  dpi = 300
)
