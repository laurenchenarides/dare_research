# ==============================================================================
# Script Name:  05_grant_tables.R
# Purpose:      Clean the CAS proposal/award export, parse investigators, and
#               produce reproducible DARE grant tables and audits.
#
# Source:
#   data/grants_cas.xlsx, sheet `grants_cas`
#
# Core rules:
#   - Exclude any nonproject/formula rows with missing Key ID. The current
#     workbook has no total row, but the guard prevents future double-counting.
#   - Reporting/submission cohort is Date Sent year, 2021-2026.
#   - Status == Funded identifies awards. Blank status means not funded or
#     still in progress and is not counted as an award.
#   - Amount* is award amount only on Funded rows. It is not treated as an
#     award on blank-status rows and is not an expenditure measure.
#   - Each Key ID is unique and counted once. Full multi-year award amounts are
#     never repeated in subsequent years.
#   - DARE award/proposal totals use lead unit 1172.
#   - DARE faculty PI/co-PI participation uses investigator unit 1172 across
#     all CAS-led records, thereby retaining cross-department participation.
#   - Funding-source category uses the prime sponsor/type when present,
#     otherwise the direct sponsor/type.
#   - F&A analysis uses the direct sponsor because it issued the award to CSU.
#
# Supported outputs:
#   output/table_C1_sponsored_projects_by_year.csv
#   output/table_C2_awards_by_source_year_long.csv
#   output/table_C2_awards_by_source_year.csv
#   output/table_C3_awards_by_sponsor.csv
#   output/appendix_CAS_awards_by_department_year.csv
#   output/appendix_CAS_awards_department_summary.csv
#   output/appendix_DARE_cross_unit_collaboration.csv
#   output/appendix_DARE_cross_unit_projects.csv
#   output/appendix_DARE_on_other_lead_units_summary.csv
#   output/appendix_DARE_on_other_lead_units_projects.csv
#   output/grant_sponsor_concentration.csv
#   output/grant_fa_sponsor_summary.csv
#   output/grant_fa_rate_distribution.csv
#   output/figure_grant_fa_rate_by_sponsor_type.png
#   output/grant_data_availability_audit.csv
#   output/grant_investigator_parse_audit.csv
#   output/grant_faculty_match_audit.csv
#   output/grant_date_audit.csv
#   output/grant_status_audit.csv
#   output/grants_projects_clean.csv
#   output/grants_investigators_long.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(readxl)
  library(purrr)
  library(tibble)
  library(ggplot2)
})


# ==============================================================================
# 0. Configuration and helpers
# ==============================================================================

REPORT_YEARS <- 2021:2026
DARE_UNIT <- "1172"
LOW_FA_THRESHOLD <- 10

# Approved grant-export-to-roster name aliases. Keys use normalized last name
# plus first initial. Rebecca Jablonski is listed as Becca in roster.csv.
FACULTY_PERSON_KEY_ALIASES <- c(
  "jablonski|r" = "jablonski|b"
)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(current, "README.md")) &&
      file.exists(file.path(current, "CODEBOOK.md"))
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the dare_research project root.", call. = FALSE)
    }
    current <- parent
  }
}

clean_character <- function(x) {
  x <- str_squish(as.character(x))
  if_else(is.na(x) | x == "", NA_character_, x)
}

normalize_text <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

person_key_from_parts <- function(last, first) {
  last_norm <- normalize_text(last)
  first_initial <- str_sub(normalize_text(first), 1, 1)
  if_else(
    is.na(last_norm) | last_norm == "" |
      is.na(first_initial) | first_initial == "",
    NA_character_,
    str_c(last_norm, "|", first_initial)
  )
}

read_excel_with_onedrive_fallback <- function(path, sheet) {
  direct <- tryCatch(
    read_excel(path, sheet = sheet),
    error = function(e) e
  )

  if (!inherits(direct, "error")) return(direct)

  local_copy <- tempfile(fileext = ".xlsx")
  copied <- file.copy(path, local_copy, overwrite = TRUE)

  if (!copied && identical(.Platform$OS.type, "windows")) {
    escape_ps <- function(x) str_replace_all(x, "'", "''")
    ps_command <- str_c(
      "Copy-Item -LiteralPath '", escape_ps(normalizePath(
        path, winslash = "\\", mustWork = TRUE
      )), "' -Destination '", escape_ps(local_copy), "' -Force"
    )
    status <- system2(
      "powershell.exe",
      c("-NoProfile", "-Command", shQuote(ps_command)),
      stdout = FALSE,
      stderr = FALSE
    )
    copied <- identical(status, 0L) && file.exists(local_copy)
  }

  if (!copied) {
    stop(
      "Could not read or create a local copy of ", path, ". Direct error: ",
      conditionMessage(direct),
      call. = FALSE
    )
  }

  on.exit(unlink(local_copy), add = TRUE)
  read_excel(local_copy, sheet = sheet)
}

classify_funding_source <- function(
  originating_sponsor,
  originating_type,
  proposal_type
) {
  sponsor_norm <- normalize_text(originating_sponsor)
  type_clean <- coalesce(originating_type, "")

  case_when(
    proposal_type == "Internal RFP" |
      str_detect(sponsor_norm, "colorado state university") ~
      "CSU/Internal",
    type_clean == "Federal" ~ "Federal",
    str_detect(
      type_clean,
      "State of Colorado|Other Domestic State Government|Local Government"
    ) ~ "State",
    str_detect(type_clean, "Foundation|Non-Profit") ~
      "Foundation/nonprofit",
    str_detect(type_clean, "Commercial") ~ "Industry",
    TRUE ~ "Other"
  )
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else first(x)
}

safe_divide <- function(numerator, denominator) {
  if_else(
    !is.na(denominator) & denominator > 0,
    numerator / denominator,
    NA_real_
  )
}


PROJECT_ROOT <- find_project_root()
in_dir <- file.path(PROJECT_ROOT, "data")
out_dir <- file.path(PROJECT_ROOT, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# 1. Read and validate source records
# ==============================================================================

source_path <- file.path(in_dir, "grants_cas.xlsx")
grants_raw <- read_excel_with_onedrive_fallback(
  source_path,
  sheet = "grants_cas"
)

required_columns <- c(
  "Key ID", "IP Nbr", "PD Nbr", "Proposal Type", "Primary PI",
  "Investigators", "Full Project Title", "Lead Unit",
  "Lead College/Division", "Start Date", "End Date", "F&A Rate",
  "F&A Type", "Direct Sponsor", "Prime Sponsor",
  "Direct Sponsor_Type", "Prime Sponsor Type", "Date Sent", "Status",
  "Amount*"
)

missing_columns <- setdiff(required_columns, names(grants_raw))
if (length(missing_columns) > 0) {
  stop(
    "grants_cas.xlsx is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

source_nonproject_rows <- grants_raw %>% filter(is.na(`Key ID`))

projects <- grants_raw %>%
  filter(!is.na(`Key ID`)) %>%
  transmute(
    key_id = clean_character(`Key ID`),
    ip_number = as.character(as.integer(`IP Nbr`)),
    pd_number = as.character(as.integer(`PD Nbr`)),
    proposal_type = clean_character(`Proposal Type`),
    primary_pi = clean_character(`Primary PI`),
    investigators = clean_character(Investigators),
    project_title = clean_character(`Full Project Title`),
    lead_unit = clean_character(`Lead Unit`),
    lead_unit_code = str_extract(lead_unit, "^\\d+"),
    lead_unit_name = str_remove(lead_unit, "^\\d+-"),
    lead_college = clean_character(`Lead College/Division`),
    start_date = as.Date(`Start Date`),
    end_date = as.Date(`End Date`),
    end_date_century_error = !is.na(end_date) & end_date < as.Date("2000-01-01"),
    fa_rate = suppressWarnings(as.numeric(`F&A Rate`)),
    fa_type = clean_character(`F&A Type`),
    direct_sponsor = clean_character(`Direct Sponsor`),
    prime_sponsor = clean_character(`Prime Sponsor`),
    direct_sponsor_type = clean_character(`Direct Sponsor_Type`),
    prime_sponsor_type = clean_character(`Prime Sponsor Type`),
    date_sent = as.Date(`Date Sent`),
    year = as.integer(format(date_sent, "%Y")),
    status = clean_character(Status),
    funded = coalesce(status == "Funded", FALSE),
    amount = suppressWarnings(as.numeric(`Amount*`)),
    originating_sponsor = coalesce(prime_sponsor, direct_sponsor),
    originating_sponsor_type = coalesce(
      prime_sponsor_type,
      direct_sponsor_type
    ),
    sponsor_selection_rule = if_else(
      !is.na(prime_sponsor),
      "Prime sponsor reported; direct sponsor is pass-through",
      "Direct sponsor reported; prime sponsor unavailable"
    ),
    university_direct_with_prime = !is.na(prime_sponsor) &
      str_detect(coalesce(direct_sponsor_type, ""), "Higher Education") &
      !str_detect(normalize_text(direct_sponsor), "colorado state university"),
    funding_source_type = classify_funding_source(
      originating_sponsor,
      originating_sponsor_type,
      proposal_type
    ),
    dare_lead = lead_unit_code == DARE_UNIT,
    partial_2026 = year == 2026
  )

if (anyDuplicated(projects$key_id)) {
  stop("Key ID is not unique in grants_cas.xlsx.", call. = FALSE)
}

if (any(!projects$year %in% REPORT_YEARS)) {
  stop("Date Sent contains years outside 2021-2026.", call. = FALSE)
}

if (any(is.na(projects$amount) | projects$amount < 0)) {
  stop("At least one project amount is missing or negative.", call. = FALSE)
}

unknown_status <- projects %>%
  filter(!is.na(status), status != "Funded") %>%
  distinct(status)
if (nrow(unknown_status) > 0) {
  stop(
    "Unexpected nonblank status values: ",
    paste(unknown_status$status, collapse = ", "),
    call. = FALSE
  )
}


# ==============================================================================
# 2. Parse investigator names, roles, and unit codes
# ==============================================================================

investigators_long <- projects %>%
  select(key_id, year, lead_unit_code, lead_unit_name, dare_lead, investigators) %>%
  separate_longer_delim(investigators, delim = ";") %>%
  mutate(
    investigator_entry = str_squish(investigators),
    parsed = str_match(
      investigator_entry,
      "^(.*?)\\s+\\((Primary PI|Co-PI|Key Person)\\)\\s+(\\d+)\\s*$"
    ),
    investigator_name = clean_character(parsed[, 2]),
    investigator_role = clean_character(parsed[, 3]),
    investigator_unit_code = clean_character(parsed[, 4]),
    investigator_last = str_trim(str_extract(investigator_name, "^[^,]+")),
    investigator_first = str_trim(str_remove(investigator_name, "^[^,]+,")),
    investigator_person_key = person_key_from_parts(
      investigator_last,
      investigator_first
    ),
    parse_success = !is.na(investigator_name) &
      !is.na(investigator_role) &
      !is.na(investigator_unit_code)
  ) %>%
  select(-investigators, -parsed)

investigator_parse_audit <- investigators_long %>%
  filter(!parse_success) %>%
  select(
    key_id, year, lead_unit_code, lead_unit_name,
    dare_lead, investigator_entry
  )

write_csv(
  investigator_parse_audit,
  file.path(out_dir, "grant_investigator_parse_audit.csv")
)


# ==============================================================================
# 3. Match DARE investigators to the faculty roster
# ==============================================================================

roster <- read_csv(file.path(in_dir, "roster.csv"), show_col_types = FALSE) %>%
  mutate(
    roster_person_key = person_key_from_parts(last, first)
  )

if (anyDuplicated(roster$roster_person_key)) {
  stop(
    "Roster last-name plus first-initial keys are not unique; use a stronger ",
    "faculty matching rule before grant analysis.",
    call. = FALSE
  )
}

roster_long <- roster %>%
  pivot_longer(
    cols = all_of(str_c("y", REPORT_YEARS)),
    names_to = "year_column",
    values_to = "active"
  ) %>%
  mutate(
    year = as.integer(str_remove(year_column, "^y")),
    active = suppressWarnings(as.integer(active))
  ) %>%
  select(
    roster_person_key, last, first, area, year, active,
    research_pct, teaching_pct, outreach_pct
  )

investigators_long <- investigators_long %>%
  mutate(
    investigator_person_key = coalesce(
      unname(FACULTY_PERSON_KEY_ALIASES[investigator_person_key]),
      investigator_person_key
    )
  ) %>%
  left_join(
    roster %>%
      select(
        roster_person_key, roster_last = last, roster_first = first,
        roster_area = area
      ),
    by = c("investigator_person_key" = "roster_person_key"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    dare_roster_match = investigator_unit_code == DARE_UNIT &
      !is.na(roster_last)
  )

faculty_match_audit <- investigators_long %>%
  filter(investigator_unit_code == DARE_UNIT, !dare_roster_match) %>%
  distinct(
    investigator_name, investigator_person_key,
    investigator_role, investigator_unit_code
  ) %>%
  arrange(investigator_name)

write_csv(
  faculty_match_audit,
  file.path(out_dir, "grant_faculty_match_audit.csv")
)


# ==============================================================================
# 4. Table C1: DARE sponsored-project activity by submission cohort year
# ==============================================================================

dare_projects <- projects %>% filter(dare_lead)
dare_awards <- dare_projects %>% filter(funded)

dare_project_counts <- dare_projects %>%
  group_by(year) %>%
  summarize(
    proposals_submitted = n_distinct(key_id),
    awards_received = n_distinct(key_id[funded]),
    total_awarded_dollars = sum(amount[funded], na.rm = TRUE),
    average_award_amount = if_else(
      awards_received > 0,
      total_awarded_dollars / awards_received,
      NA_real_
    ),
    .groups = "drop"
  )

faculty_roles_by_year <- investigators_long %>%
  filter(
    investigator_unit_code == DARE_UNIT,
    investigator_role %in% c("Primary PI", "Co-PI"),
    dare_roster_match
  ) %>%
  inner_join(
    roster_long %>%
      select(roster_person_key, year, active),
    by = c(
      "investigator_person_key" = "roster_person_key",
      "year" = "year"
    ),
    relationship = "many-to-one"
  ) %>%
  filter(active == 1L)

faculty_role_counts <- faculty_roles_by_year %>%
  group_by(year) %>%
  summarize(
    faculty_serving_as_pi = n_distinct(
      investigator_person_key[investigator_role == "Primary PI"]
    ),
    faculty_serving_as_copi = n_distinct(
      investigator_person_key[investigator_role == "Co-PI"]
    ),
    faculty_serving_as_pi_or_copi = n_distinct(investigator_person_key),
    .groups = "drop"
  )

active_faculty <- roster_long %>%
  filter(active == 1L) %>%
  count(year, name = "active_faculty")

table_C1 <- tibble(year = REPORT_YEARS) %>%
  left_join(active_faculty, by = "year", relationship = "one-to-one") %>%
  left_join(dare_project_counts, by = "year", relationship = "one-to-one") %>%
  left_join(faculty_role_counts, by = "year", relationship = "one-to-one") %>%
  mutate(
    across(
      c(
        active_faculty, proposals_submitted, awards_received,
        total_awarded_dollars, faculty_serving_as_pi,
        faculty_serving_as_copi, faculty_serving_as_pi_or_copi
      ),
      ~ replace_na(.x, 0)
    ),
    share_active_faculty_pi_or_copi = safe_divide(
      faculty_serving_as_pi_or_copi,
      active_faculty
    ),
    total_awarded_dollars = round(total_awarded_dollars, 2),
    average_award_amount = round(average_award_amount, 2),
    share_active_faculty_pi_or_copi = round(
      share_active_faculty_pi_or_copi,
      4
    ),
    partial_year = year == 2026,
    year_basis = "Date Sent (proposal submission cohort)"
  )

write_csv(
  table_C1,
  file.path(out_dir, "table_C1_sponsored_projects_by_year.csv")
)


# ==============================================================================
# 5. Table C2: funded DARE awards by originating funding-source type and year
# ==============================================================================

funding_source_levels <- c(
  "Federal", "State", "CSU/Internal", "Foundation/nonprofit",
  "Industry", "Other"
)

table_C2_long <- dare_awards %>%
  count(
    funding_source_type,
    year,
    wt = amount,
    name = "award_dollars"
  ) %>%
  left_join(
    dare_awards %>%
      count(funding_source_type, year, name = "award_count"),
    by = c("funding_source_type", "year"),
    relationship = "one-to-one"
  ) %>%
  complete(
    funding_source_type = funding_source_levels,
    year = REPORT_YEARS,
    fill = list(award_count = 0L, award_dollars = 0)
  ) %>%
  mutate(
    funding_source_type = factor(
      funding_source_type,
      levels = funding_source_levels
    ),
    partial_year = year == 2026,
    year_basis = "Date Sent (proposal submission cohort)",
    award_dollars = round(award_dollars, 2)
  ) %>%
  arrange(funding_source_type, year)

table_C2 <- table_C2_long %>%
  select(funding_source_type, year, award_count, award_dollars) %>%
  pivot_wider(
    names_from = year,
    values_from = c(award_count, award_dollars),
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    six_year_award_count = rowSums(across(starts_with("award_count_"))),
    six_year_award_dollars = round(
      rowSums(across(starts_with("award_dollars_"))),
      2
    )
  )

write_csv(
  table_C2_long,
  file.path(out_dir, "table_C2_awards_by_source_year_long.csv")
)
write_csv(
  table_C2,
  file.path(out_dir, "table_C2_awards_by_source_year.csv")
)


# ==============================================================================
# 6. Table C3: funded DARE awards by originating sponsor
# ==============================================================================

sponsor_faculty_summary <- dare_awards %>%
  select(key_id, originating_sponsor) %>%
  inner_join(
    investigators_long %>% filter(dare_roster_match),
    by = "key_id",
    relationship = "one-to-many"
  ) %>%
  group_by(originating_sponsor) %>%
  summarize(
    number_of_dare_faculty_involved = n_distinct(investigator_person_key),
    n_research_areas = n_distinct(roster_area, na.rm = TRUE),
    primary_research_area = case_when(
      n_research_areas > 1 ~ "Cross-area",
      n_research_areas == 1 ~ first_nonmissing(roster_area),
      TRUE ~ "Unclassified"
    ),
    .groups = "drop"
  )

table_C3 <- dare_awards %>%
  group_by(
    originating_sponsor,
    funding_source_type
  ) %>%
  summarize(
    direct_pass_through_sponsors = paste(
      sort(unique(na.omit(direct_sponsor))),
      collapse = "; "
    ),
    number_of_awards = n_distinct(key_id),
    total_awarded_dollars = sum(amount),
    years_represented = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  left_join(
    sponsor_faculty_summary %>%
      select(
        originating_sponsor,
        number_of_dare_faculty_involved,
        primary_research_area
      ),
    by = "originating_sponsor",
    relationship = "one-to-one"
  ) %>%
  mutate(total_awarded_dollars = round(total_awarded_dollars, 2)) %>%
  arrange(desc(total_awarded_dollars), originating_sponsor)

write_csv(
  table_C3,
  file.path(out_dir, "table_C3_awards_by_sponsor.csv")
)


# ==============================================================================
# 7. Sponsor concentration and F&A analysis
# ==============================================================================

sponsor_dollars <- dare_awards %>%
  group_by(originating_sponsor) %>%
  summarize(
    award_count = n_distinct(key_id),
    award_dollars = sum(amount),
    .groups = "drop"
  ) %>%
  arrange(desc(award_dollars)) %>%
  mutate(
    dollar_share = award_dollars / sum(award_dollars),
    rank = row_number()
  )

concentration_metrics <- tibble(
  metric = c(
    "Number of originating sponsors",
    "Top sponsor share of award dollars",
    "Top 3 sponsors share of award dollars",
    "Top 5 sponsors share of award dollars",
    "Sponsor-dollar HHI",
    "Federal share of award dollars"
  ),
  value = c(
    nrow(sponsor_dollars),
    sum(sponsor_dollars$dollar_share[sponsor_dollars$rank <= 1]),
    sum(sponsor_dollars$dollar_share[sponsor_dollars$rank <= 3]),
    sum(sponsor_dollars$dollar_share[sponsor_dollars$rank <= 5]),
    sum(sponsor_dollars$dollar_share^2),
    sum(dare_awards$amount[dare_awards$funding_source_type == "Federal"]) /
      sum(dare_awards$amount)
  ),
  unit = c("count", "share", "share", "share", "index", "share")
) %>%
  mutate(value = round(value, 4))

write_csv(
  concentration_metrics,
  file.path(out_dir, "grant_sponsor_concentration.csv")
)

fa_sponsor_summary <- projects %>%
  filter(funded) %>%
  group_by(direct_sponsor, direct_sponsor_type) %>%
  summarize(
    funded_award_count = n_distinct(key_id),
    funded_award_dollars = sum(amount),
    dare_award_count = n_distinct(key_id[lead_unit_code == DARE_UNIT]),
    other_cas_award_count = n_distinct(key_id[lead_unit_code != DARE_UNIT]),
    dare_unique_direct_sponsor = dare_award_count > 0 &
      other_cas_award_count == 0,
    median_fa_rate = median(fa_rate, na.rm = TRUE),
    mean_fa_rate = mean(fa_rate, na.rm = TRUE),
    zero_fa_award_count = sum(fa_rate == 0, na.rm = TRUE),
    low_fa_award_count = sum(fa_rate <= LOW_FA_THRESHOLD, na.rm = TRUE),
    low_fa_award_share = mean(fa_rate <= LOW_FA_THRESHOLD, na.rm = TRUE),
    fa_types = paste(sort(unique(na.omit(fa_type))), collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(low_fa_threshold = LOW_FA_THRESHOLD) %>%
  mutate(
    funded_award_dollars = round(funded_award_dollars, 2),
    median_fa_rate = round(median_fa_rate, 2),
    mean_fa_rate = round(mean_fa_rate, 2),
    low_fa_award_share = round(low_fa_award_share, 4)
  ) %>%
  arrange(median_fa_rate, desc(funded_award_dollars), direct_sponsor)

write_csv(
  fa_sponsor_summary,
  file.path(out_dir, "grant_fa_sponsor_summary.csv")
)

fa_rate_distribution <- projects %>%
  filter(funded, !is.na(fa_rate)) %>%
  mutate(
    fa_sponsor_category = if_else(
      university_direct_with_prime,
      "Pass-through institution",
      classify_funding_source(
        direct_sponsor,
        direct_sponsor_type,
        proposal_type
      )
    ),
    lead_group = if_else(dare_lead, "DARE-led", "Other CAS-led")
  ) %>%
  select(
    key_id, year, lead_group, lead_unit_code, lead_unit_name,
    fa_sponsor_category, fa_rate, fa_type,
    direct_sponsor, direct_sponsor_type,
    originating_sponsor, originating_sponsor_type,
    university_direct_with_prime, amount
  ) %>%
  arrange(lead_group, fa_sponsor_category, fa_rate)

fa_category_order <- fa_rate_distribution %>%
  group_by(fa_sponsor_category) %>%
  summarise(category_median = median(fa_rate), .groups = "drop") %>%
  arrange(category_median) %>%
  pull(fa_sponsor_category)

fa_rate_distribution <- fa_rate_distribution %>%
  mutate(
    fa_sponsor_category = factor(
      fa_sponsor_category,
      levels = fa_category_order
    ),
    lead_group = factor(
      lead_group,
      levels = c("DARE-led", "Other CAS-led")
    )
  )

fa_violin_data <- fa_rate_distribution %>%
  add_count(lead_group, fa_sponsor_category, name = "category_panel_n") %>%
  filter(category_panel_n >= 2)

fa_distribution_plot <- ggplot(
  fa_rate_distribution,
  aes(x = fa_rate, y = fa_sponsor_category)
) +
  geom_violin(
    data = fa_violin_data,
    fill = "#1E4D2B", color = "#1E4D2B",
    alpha = 0.18, linewidth = 0.35, scale = "width", trim = TRUE,
    drop = TRUE
  ) +
  geom_boxplot(
    width = 0.16, outlier.shape = NA,
    fill = "white", color = "#1E4D2B", linewidth = 0.4
  ) +
  geom_jitter(
    width = 0, height = 0.07, alpha = 0.28,
    size = 0.9, color = "#1E4D2B"
  ) +
  geom_vline(
    xintercept = LOW_FA_THRESHOLD,
    linetype = "dashed", color = "#C69214", linewidth = 0.55
  ) +
  facet_wrap(vars(lead_group), ncol = 1) +
  scale_x_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "Distribution of F&A rates by direct-sponsor category",
    subtitle = paste(
      "Funded awards, 2021-2026 submission cohorts;",
      "university pass-through awards shown separately"
    ),
    x = "F&A rate (%)",
    y = "Direct-sponsor category",
    caption = paste(
      "Dashed line marks the 10% low-rate screening threshold.",
      "F&A bases (MTDC, TDC, S&W, or no indirect cost) differ;",
      "see the companion CSV. 2026 is partial."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", color = "#1E4D2B"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(color = "#1F2937"),
    plot.caption = element_text(hjust = 0, color = "#4B5563"),
    plot.margin = margin(10, 18, 10, 10)
  )

write_csv(
  fa_rate_distribution %>% mutate(fa_sponsor_category = as.character(fa_sponsor_category)),
  file.path(out_dir, "grant_fa_rate_distribution.csv")
)
ggsave(
  filename = file.path(out_dir, "figure_grant_fa_rate_by_sponsor_type.png"),
  plot = fa_distribution_plot,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# 8. CAS award comparison and DARE cross-unit collaboration appendices
# ==============================================================================

cas_awards_by_department_year <- projects %>%
  group_by(lead_unit_code, lead_unit_name, year) %>%
  summarize(
    proposals_submitted = n_distinct(key_id),
    awards_received = n_distinct(key_id[funded]),
    total_awarded_dollars = sum(amount[funded], na.rm = TRUE),
    partial_year = first(year) == 2026,
    year_basis = "Date Sent (proposal submission cohort)",
    .groups = "drop"
  ) %>%
  mutate(total_awarded_dollars = round(total_awarded_dollars, 2)) %>%
  arrange(lead_unit_code, year)

cas_awards_department_summary <- cas_awards_by_department_year %>%
  group_by(lead_unit_code, lead_unit_name) %>%
  summarize(
    proposals_submitted_2021_2026 = sum(proposals_submitted),
    awards_received_2021_2026 = sum(awards_received),
    total_awarded_dollars_2021_2026 = sum(total_awarded_dollars),
    average_award_amount = safe_divide(
      total_awarded_dollars_2021_2026,
      awards_received_2021_2026
    ),
    .groups = "drop"
  ) %>%
  mutate(
    total_awarded_dollars_2021_2026 = round(
      total_awarded_dollars_2021_2026,
      2
    ),
    average_award_amount = round(average_award_amount, 2)
  ) %>%
  arrange(desc(total_awarded_dollars_2021_2026))

write_csv(
  cas_awards_by_department_year,
  file.path(out_dir, "appendix_CAS_awards_by_department_year.csv")
)
write_csv(
  cas_awards_department_summary,
  file.path(out_dir, "appendix_CAS_awards_department_summary.csv")
)

cas_unit_lookup <- projects %>%
  distinct(lead_unit_code, lead_unit_name)

dare_cross_unit_projects <- investigators_long %>%
  filter(
    dare_lead,
    investigator_unit_code != DARE_UNIT
  ) %>%
  left_join(
    projects %>%
      select(key_id, year, funded, amount, project_title),
    by = c("key_id", "year"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    cas_unit_lookup %>%
      rename(
        investigator_unit_code = lead_unit_code,
        investigator_unit_name = lead_unit_name
      ),
    by = "investigator_unit_code",
    relationship = "many-to-one"
  ) %>%
  select(
    key_id, year, funded, amount, project_title,
    investigator_name, investigator_role,
    investigator_unit_code, investigator_unit_name
  ) %>%
  distinct()

dare_cross_unit_summary <- dare_cross_unit_projects %>%
  group_by(investigator_unit_code, investigator_unit_name) %>%
  summarize(
    project_count = n_distinct(key_id),
    funded_award_count = n_distinct(key_id[funded]),
    funded_award_dollars = sum(
      projects$amount[
        match(unique(key_id[funded]), projects$key_id)
      ],
      na.rm = TRUE
    ),
    investigator_count = n_distinct(investigator_name),
    years_represented = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(funded_award_dollars = round(funded_award_dollars, 2)) %>%
  arrange(desc(project_count), investigator_unit_code)

write_csv(
  dare_cross_unit_projects,
  file.path(out_dir, "appendix_DARE_cross_unit_projects.csv")
)
write_csv(
  dare_cross_unit_summary,
  file.path(out_dir, "appendix_DARE_cross_unit_collaboration.csv")
)

# Reverse direction: rostered DARE faculty serving as PI or Co-PI on projects
# led by another CAS department. Project rows are unique before department
# totals are calculated so awards with multiple DARE investigators count once.
dare_on_other_lead_units_people <- investigators_long %>%
  filter(
    !dare_lead,
    dare_roster_match,
    investigator_role %in% c("Primary PI", "Co-PI")
  ) %>%
  transmute(
    key_id, year, lead_unit_code, lead_unit_name,
    dare_faculty = paste(roster_last, roster_first, sep = ", "),
    dare_faculty_role = investigator_role
  ) %>%
  distinct()

dare_on_other_lead_units_projects <- dare_on_other_lead_units_people %>%
  group_by(key_id, year, lead_unit_code, lead_unit_name) %>%
  summarise(
    dare_faculty_roles = paste(
      sort(unique(paste0(dare_faculty, " [", dare_faculty_role, "]"))),
      collapse = "; "
    ),
    dare_faculty_count = n_distinct(dare_faculty),
    dare_faculty = paste(sort(unique(dare_faculty)), collapse = "; "),
    .groups = "drop"
  ) %>%
  left_join(
    projects %>%
      select(
        key_id, funded, status, amount, project_title,
        originating_sponsor, originating_sponsor_type,
        direct_sponsor, direct_sponsor_type
      ),
    by = "key_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    funding_status_group = if_else(
      funded,
      "Funded",
      "Not funded or in progress"
    )
  ) %>%
  select(
    key_id, year, lead_unit_code, lead_unit_name,
    funded, funding_status_group, amount, project_title,
    dare_faculty, dare_faculty_roles, dare_faculty_count,
    originating_sponsor, originating_sponsor_type,
    direct_sponsor, direct_sponsor_type
  ) %>%
  arrange(lead_unit_name, year, desc(funded), key_id)

dare_on_other_lead_units_summary <- dare_on_other_lead_units_projects %>%
  group_by(lead_unit_code, lead_unit_name) %>%
  summarise(
    proposals_with_dare_pi_or_copi = n_distinct(key_id),
    funded_awards = n_distinct(key_id[funded]),
    not_funded_or_in_progress = n_distinct(key_id[!funded]),
    funded_award_dollars = sum(if_else(funded, amount, 0), na.rm = TRUE),
    years_represented = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  left_join(
    dare_on_other_lead_units_people %>%
      group_by(lead_unit_code, lead_unit_name) %>%
      summarise(
        dare_faculty_involved = n_distinct(dare_faculty),
        .groups = "drop"
      ),
    by = c("lead_unit_code", "lead_unit_name")
  ) %>%
  mutate(funded_award_dollars = round(funded_award_dollars, 2)) %>%
  arrange(desc(proposals_with_dare_pi_or_copi), lead_unit_name)

write_csv(
  dare_on_other_lead_units_projects,
  file.path(out_dir, "appendix_DARE_on_other_lead_units_projects.csv")
)
write_csv(
  dare_on_other_lead_units_summary,
  file.path(out_dir, "appendix_DARE_on_other_lead_units_summary.csv")
)


# ==============================================================================
# 9. Audits and data-availability decisions
# ==============================================================================

date_audit <- projects %>%
  filter(end_date_century_error) %>%
  select(
    key_id, pd_number, project_title, start_date, end_date,
    date_sent, status, amount
  ) %>%
  arrange(date_sent)

status_audit <- projects %>%
  mutate(
    status_interpretation = if_else(
      funded,
      "Funded award; Amount* treated as award amount",
      paste(
        "Not funded or in progress; not counted as an award;",
        "Amount* meaning unresolved"
      )
    )
  ) %>%
  count(status, status_interpretation, name = "project_count")

data_availability <- tribble(
  ~requested_output, ~status, ~source_or_blocker,
  "Table C1: activity by year", "Supported with qualification", "Awards are grouped by Date Sent submission cohort because award date is absent.",
  "Table C2: awards by funding source", "Supported with qualification", "Uses prime sponsor/type when present, otherwise direct sponsor/type; year is Date Sent cohort.",
  "Table C3: awards by sponsor/program", "Partially supported", "Sponsor is available; no explicit program field. Direct pass-through sponsors and derived DARE research areas are retained.",
  "Table C4: proposal and award success", "Not supported for reporting", "Blank status combines not-funded and in-progress records; funded requested dollars are not separately available.",
  "Table C5: sponsored expenditures", "Not supported", "No expenditure or fiscal-year expenditure fields are present.",
  "Table C6: CAS expenditure comparison", "Not supported", "No expenditure data or faculty denominators for other CAS departments are present; an award/proposal comparison is produced instead.",
  "Active sponsored projects", "Not currently supported", "Seventy-two End Date values have an apparent 1900/2000 century error that must be corrected before active-project counts are reliable.",
  "F&A sponsor analysis", "Supported with qualification", "Rates are compared with F&A Type retained because TDC, MTDC, S&W, and no-indirect-cost bases differ."
)

# Summarize higher-education pass-through patterns. Proposal and award counts
# remain separate because blank Status can mean not funded or still in progress.
university_pass_through <- projects %>%
  filter(university_direct_with_prime)

pass_through_top_originating <- university_pass_through %>%
  filter(funded) %>%
  group_by(direct_sponsor, originating_sponsor) %>%
  summarise(
    originating_award_count = n(),
    originating_award_dollars = sum(amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(direct_sponsor) %>%
  arrange(desc(originating_award_dollars), desc(originating_award_count), originating_sponsor) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    direct_sponsor,
    top_originating_sponsor = originating_sponsor,
    top_originating_sponsor_awards = originating_award_count,
    top_originating_sponsor_dollars = originating_award_dollars
  )

pass_through_institution_summary <- university_pass_through %>%
  group_by(direct_sponsor, direct_sponsor_type) %>%
  summarise(
    proposals = n(),
    awards = sum(funded),
    award_dollars = sum(if_else(funded, amount, 0), na.rm = TRUE),
    average_award_amount = if_else(awards > 0, award_dollars / awards, NA_real_),
    observed_funded_share = awards / proposals,
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    years_represented = n_distinct(year),
    originating_sponsors = n_distinct(originating_sponsor),
    federal_awards = sum(funded & originating_sponsor_type == "Federal", na.rm = TRUE),
    federal_award_dollars = sum(
      if_else(funded & originating_sponsor_type == "Federal", amount, 0),
      na.rm = TRUE
    ),
    dare_proposals = sum(lead_unit_code == "1172"),
    dare_awards = sum(funded & lead_unit_code == "1172"),
    dare_award_dollars = sum(
      if_else(funded & lead_unit_code == "1172", amount, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    federal_share_of_award_dollars = if_else(
      award_dollars > 0, federal_award_dollars / award_dollars, NA_real_
    ),
    dare_share_of_award_dollars = if_else(
      award_dollars > 0, dare_award_dollars / award_dollars, NA_real_
    )
  ) %>%
  left_join(pass_through_top_originating, by = "direct_sponsor") %>%
  arrange(desc(award_dollars), desc(awards), direct_sponsor)

pass_through_institution_year <- university_pass_through %>%
  group_by(direct_sponsor, year) %>%
  summarise(
    proposals = n(),
    awards = sum(funded),
    award_dollars = sum(if_else(funded, amount, 0), na.rm = TRUE),
    dare_proposals = sum(lead_unit_code == "1172"),
    dare_awards = sum(funded & lead_unit_code == "1172"),
    dare_award_dollars = sum(
      if_else(funded & lead_unit_code == "1172", amount, 0),
      na.rm = TRUE
    ),
    originating_sponsors = n_distinct(originating_sponsor),
    .groups = "drop"
  ) %>%
  arrange(direct_sponsor, year)

write_csv(
  date_audit,
  file.path(out_dir, "grant_date_audit.csv")
)
write_csv(
  status_audit,
  file.path(out_dir, "grant_status_audit.csv")
)
write_csv(
  projects %>%
    filter(university_direct_with_prime) %>%
    select(
      key_id, year, funded, amount, lead_unit_code, project_title,
      originating_sponsor, originating_sponsor_type,
      direct_sponsor, direct_sponsor_type, sponsor_selection_rule
    ) %>%
    arrange(year, originating_sponsor, direct_sponsor),
  file.path(out_dir, "grant_university_subaward_sponsor_audit.csv")
)
write_csv(
  pass_through_institution_summary,
  file.path(out_dir, "grant_pass_through_institution_summary.csv")
)
write_csv(
  pass_through_institution_year,
  file.path(out_dir, "grant_pass_through_institution_by_year.csv")
)
write_csv(
  data_availability,
  file.path(out_dir, "grant_data_availability_audit.csv")
)
write_csv(
  projects,
  file.path(out_dir, "grants_projects_clean.csv"),
  na = ""
)
write_csv(
  investigators_long,
  file.path(out_dir, "grants_investigators_long.csv"),
  na = ""
)

message(
  "Stage 05 complete. CAS project rows: ", nrow(projects),
  ". DARE-led proposals: ", nrow(dare_projects),
  ". DARE-led funded awards: ", nrow(dare_awards),
  ". DARE award dollars: $",
  format(sum(dare_awards$amount), big.mark = ",", scientific = FALSE),
  "."
)
