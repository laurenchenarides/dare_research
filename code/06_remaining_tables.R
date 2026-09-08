# ==============================================================================
# Script Name:  06_remaining_tables.R
# Purpose:      Produce department-composition, collaboration, recognition,
#               mentoring, and student-engagement tables supported by current
#               project data. Unsupported fields remain explicitly missing.
#
# Reporting window: 2021-2026 (2026 partial)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(readxl)
  library(purrr)
  library(tibble)
})

REPORT_YEARS <- 2021:2026

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "README.md")) &&
        file.exists(file.path(current, "CODEBOOK.md"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Project root not found.", call. = FALSE)
    current <- parent
  }
}

collapse_values <- function(x) {
  values <- sort(unique(na.omit(str_squish(as.character(x)))))
  values <- values[values != ""]
  if (length(values) == 0) NA_character_ else paste(values, collapse = "; ")
}

root <- find_project_root()
data_dir <- file.path(root, "data")
out_dir <- file.path(root, "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

appointments <- read_csv(
  file.path(out_dir, "faculty_appointments.csv"),
  show_col_types = FALSE
)
faculty_year <- read_csv(
  file.path(out_dir, "faculty_year_panel.csv"),
  show_col_types = FALSE
)
publication_level <- read_csv(
  file.path(out_dir, "publication_level_analysis.csv"),
  show_col_types = FALSE
)
coauthors <- read_csv(
  file.path(out_dir, "coauthors_long.csv"),
  show_col_types = FALSE
)
grant_projects <- read_csv(
  file.path(out_dir, "grants_projects_clean.csv"),
  show_col_types = FALSE
)

# ==============================================================================
# A1. Faculty data snapshot
# ==============================================================================

faculty_active_years <- faculty_year %>%
  filter(year %in% REPORT_YEARS, active == 1) %>%
  group_by(faculty_key) %>%
  summarise(
    research_area = first(na.omit(area), default = NA_character_),
    first_active_year = min(year),
    last_active_year = max(year),
    active_years_in_window = n_distinct(year),
    .groups = "drop"
  )

table_a1 <- appointments %>%
  left_join(faculty_active_years, by = "faculty_key") %>%
  transmute(
    faculty_name = candidate_name,
    research_area,
    faculty_type,
    faculty_rank,
    teaching_percent = pct_teaching,
    research_percent = pct_research,
    service_percent = pct_service,
    tenure_track_as_listed = is_tt_as_listed,
    tenure_track_if_ritten_included = is_tt_if_ritten_tt,
    departed_or_retired = departed_flag,
    first_active_year,
    last_active_year,
    active_years_in_window,
    appointment_source,
    classification_source
  ) %>%
  arrange(faculty_name)

write_csv(table_a1, file.path(out_dir, "table_A1_faculty_data_snapshot.csv"))

# ==============================================================================
# E1. Within-department research collaboration
# Uses the same qualifying journal-article definition as Table D1.
# ==============================================================================

qualifying_publications <- publication_level %>%
  filter(
    year %in% REPORT_YEARS,
    journal_article_countable,
    in_reporting_window
  )

e1_faculty_year <- qualifying_publications %>%
  select(year, faculty_names) %>%
  separate_longer_delim(faculty_names, delim = "; ") %>%
  mutate(faculty_names = str_squish(faculty_names)) %>%
  filter(!is.na(faculty_names), faculty_names != "") %>%
  distinct(year, faculty_names) %>%
  count(year, name = "faculty_involved")

table_e1 <- qualifying_publications %>%
  group_by(year) %>%
  summarise(
    unique_qualifying_publications = n_distinct(publication_key),
    publications_with_two_or_more_department_faculty = sum(internal_collaboration),
    share_with_multiple_department_faculty =
      publications_with_two_or_more_department_faculty /
      unique_qualifying_publications,
    cross_area_publications = sum(cross_area_publication),
    .groups = "drop"
  ) %>%
  right_join(tibble(year = REPORT_YEARS), by = "year") %>%
  left_join(e1_faculty_year, by = "year") %>%
  mutate(
    across(
      c(unique_qualifying_publications,
        publications_with_two_or_more_department_faculty,
        cross_area_publications, faculty_involved),
      ~ replace_na(.x, 0L)
    ),
    share_with_multiple_department_faculty = round(
      replace_na(share_with_multiple_department_faculty, 0), 4
    ),
    partial_year = year == 2026
  ) %>%
  arrange(year)

write_csv(table_e1, file.path(out_dir, "table_E1_within_department_collaboration.csv"))

# ==============================================================================
# E2. Collaboration across CSU units -- sponsored-project component
# Publication coauthors identify CSU, but not a reliable internal CSU subunit.
# ==============================================================================

other_on_dare <- read_csv(
  file.path(out_dir, "appendix_DARE_cross_unit_collaboration.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    csu_unit_code = as.character(investigator_unit_code),
    csu_unit = investigator_unit_name,
    collaboration_direction = "Other-unit investigators on DARE-led projects",
    collaborative_publications = NA_integer_,
    collaborative_project_proposals = project_count,
    funded_collaborative_projects = funded_award_count,
    associated_funded_award_dollars = funded_award_dollars,
    faculty_or_investigators_involved = investigator_count,
    research_themes = NA_character_,
    years_active = years_represented,
    publication_component_status =
      "Unavailable: OpenAlex does not reliably identify internal CSU units"
  )

dare_on_other <- read_csv(
  file.path(out_dir, "appendix_DARE_on_other_lead_units_summary.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    csu_unit_code = as.character(lead_unit_code),
    csu_unit = lead_unit_name,
    collaboration_direction = "DARE faculty on projects led by another CAS unit",
    collaborative_publications = NA_integer_,
    collaborative_project_proposals = proposals_with_dare_pi_or_copi,
    funded_collaborative_projects = funded_awards,
    associated_funded_award_dollars = funded_award_dollars,
    faculty_or_investigators_involved = dare_faculty_involved,
    research_themes = NA_character_,
    years_active = years_represented,
    publication_component_status =
      "Unavailable: OpenAlex does not reliably identify internal CSU units"
  )

table_e2 <- bind_rows(other_on_dare, dare_on_other) %>%
  arrange(csu_unit, collaboration_direction)

write_csv(table_e2, file.path(out_dir, "table_E2_collaboration_across_CSU_units.csv"))

# ==============================================================================
# E3. External and cross-institutional collaboration
# Publication partners use OpenAlex affiliations. Sponsored-project partners
# use direct sponsors on funded DARE-led awards and should be interpreted as
# funding/administrative partners, not necessarily publication coauthors.
# ==============================================================================

publication_partner_rows <- qualifying_publications %>%
  select(publication_key, year, research_areas) %>%
  inner_join(
    coauthors %>%
      filter(
        !is.na(institution),
        institution != "",
        !str_detect(institution, regex("Colorado State University", TRUE))
      ) %>%
      select(publication_key, institution, institution_type, country_code),
    by = "publication_key"
  ) %>%
  distinct(publication_key, year, research_areas, institution, institution_type, country_code)

publication_partners <- publication_partner_rows %>%
  group_by(institution, institution_type, country_code) %>%
  summarise(
    coauthored_publications = n_distinct(publication_key),
    research_area = collapse_values(research_areas),
    years_active = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  transmute(
    collaborating_institution_or_partner = institution,
    partner_type = case_when(
      institution_type == "education" ~ "University/education",
      institution_type == "government" ~ "Government",
      institution_type == "company" ~ "Industry",
      institution_type == "nonprofit" ~ "Foundation/nonprofit",
      TRUE ~ coalesce(institution_type, "Other/unknown")
    ),
    country_code,
    coauthored_publications,
    sponsored_projects = 0L,
    multistate_project_involvement = NA_character_,
    research_area,
    years_active,
    relationship_basis = "OpenAlex coauthor affiliation"
  )

publication_partner_overall_summary <- tibble(
  summary_group = "Overall",
  category = c(
    "All external institutions",
    "United States institutions",
    "Institutions outside the United States",
    "Countries represented",
    "Publications with at least one external institutional affiliation"
  ),
  institution_count = c(
    nrow(publication_partners),
    sum(publication_partners$country_code == "US", na.rm = TRUE),
    sum(
      !is.na(publication_partners$country_code) &
        publication_partners$country_code != "US"
    ),
    n_distinct(publication_partners$country_code, na.rm = TRUE),
    NA_integer_
  ),
  coauthored_publication_links = c(
    NA_integer_, NA_integer_, NA_integer_, NA_integer_,
    n_distinct(publication_partner_rows$publication_key)
  )
)

publication_partner_type_summary <- publication_partners %>%
  group_by(category = partner_type) %>%
  summarise(
    institution_count = n(),
    coauthored_publication_links = sum(coauthored_publications),
    .groups = "drop"
  ) %>%
  mutate(summary_group = "Partner type", .before = category)

publication_partner_country_summary <- publication_partners %>%
  group_by(category = country_code) %>%
  summarise(
    institution_count = n(),
    coauthored_publication_links = sum(coauthored_publications),
    .groups = "drop"
  ) %>%
  mutate(summary_group = "Country code", .before = category)

table_e3_publication_partner_summary <- bind_rows(
  publication_partner_overall_summary,
  publication_partner_type_summary,
  publication_partner_country_summary
) %>%
  arrange(
    factor(summary_group, levels = c("Overall", "Partner type", "Country code")),
    desc(institution_count),
    category
  )

write_csv(
  table_e3_publication_partner_summary,
  file.path(out_dir, "table_E3_publication_partner_summary.csv")
)

grant_partners <- grant_projects %>%
  filter(dare_lead, funded, !is.na(direct_sponsor), direct_sponsor != "") %>%
  group_by(direct_sponsor, direct_sponsor_type, funding_source_type) %>%
  summarise(
    sponsored_projects = n_distinct(key_id),
    years_active = paste(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) %>%
  transmute(
    collaborating_institution_or_partner = direct_sponsor,
    partner_type = case_when(
      str_detect(coalesce(direct_sponsor_type, ""), "Higher Education") ~
        "University/education",
      funding_source_type == "Federal" ~ "Government",
      funding_source_type == "State" ~ "Government",
      funding_source_type == "Industry" ~ "Industry",
      funding_source_type == "Foundation/nonprofit" ~ "Foundation/nonprofit",
      funding_source_type == "CSU/Internal" ~ "CSU/Internal",
      TRUE ~ "Other/unknown"
    ),
    country_code = NA_character_,
    coauthored_publications = 0L,
    sponsored_projects,
    multistate_project_involvement = NA_character_,
    research_area = NA_character_,
    years_active,
    relationship_basis = "Direct sponsor on funded DARE-led award"
  )

table_e3 <- bind_rows(publication_partners, grant_partners) %>%
  arrange(desc(coauthored_publications), desc(sponsored_projects),
          collaborating_institution_or_partner)

write_csv(table_e3, file.path(out_dir, "table_E3_external_collaboration.csv"))

# ==============================================================================
# F1. Faculty research awards and scholarly recognitions
# The source does not identify recipients or research areas.
# ==============================================================================

awards <- read_excel(file.path(data_dir, "awards.xlsx"))

# Complete department award inventory. The source records one award per row
# and identifies whether it was received by a faculty member, student, staff
# member, or alumnus/alumna. All listed awards were competitive and required
# an application, as confirmed by the department.
table_f0_awards_summary <- awards %>%
  filter(Year %in% REPORT_YEARS) %>%
  transmute(
    year = as.integer(Year),
    recipient_type = .data[["Faculty or Student?"]]
  ) %>%
  count(year, recipient_type, name = "award_count") %>%
  complete(
    year = REPORT_YEARS,
    recipient_type = c("Faculty", "Student", "Staff", "Alumni"),
    fill = list(award_count = 0L)
  ) %>%
  pivot_wider(
    names_from = recipient_type,
    values_from = award_count,
    names_glue = "{tolower(recipient_type)}_awards"
  ) %>%
  mutate(
    total_awards = faculty_awards + student_awards + staff_awards +
      alumni_awards,
    partial_year = year == 2026
  ) %>%
  select(
    year, total_awards, faculty_awards, student_awards,
    staff_awards, alumni_awards, partial_year
  ) %>%
  arrange(year)

write_csv(
  table_f0_awards_summary,
  file.path(out_dir, "table_F0_department_awards_by_year.csv")
)

table_f1 <- awards %>%
  filter(
    Year %in% REPORT_YEARS,
    .data[["Faculty or Student?"]] == "Faculty",
    .data[["Category (Teaching / Research / Extension / Service / Student)"]] ==
      "Research"
  ) %>%
  transmute(
    award_or_recognition = .data[["Award Name"]],
    sponsoring_organization = .data[["Offering Entity"]],
    recognition_level = recode(
      .data[["Dept / Assoc / Univ / National"]],
      "Department" = "Department",
      "College" = "College",
      "University" = "University",
      "National" = "National",
      .default = .data[["Dept / Assoc / Univ / National"]]
    ),
    year = as.integer(Year),
    research_area_represented = NA_character_,
    competitive_or_elected_designation =
      "Competitive; application required",
    source_limitation =
      "Recipient and research area are not included in awards.xlsx"
  ) %>%
  arrange(year, award_or_recognition)

write_csv(table_f1, file.path(out_dir, "table_F1_faculty_research_awards.csv"))

# ==============================================================================
# Supplemental mentoring evidence from graduate committee membership
# This does not replace the requested F2 faculty-development mechanisms table.
# ==============================================================================

committee_raw <- read_excel(file.path(data_dir, "grad_committee_membership.xlsx")) %>%
  select(1:7) %>%
  mutate(
    membership_id = row_number(),
    member_from_year = suppressWarnings(as.integer(MEMBER_FROM_YEAR))
  ) %>%
  filter(member_from_year %in% REPORT_YEARS)

table_f2_supplement <- committee_raw %>%
  group_by(year = member_from_year) %>%
  summarise(
    committee_memberships_beginning = n_distinct(membership_id),
    advisor_memberships = sum(MEMBER_FUNCTION_DESC == "Advisor", na.rm = TRUE),
    coadvisor_memberships = sum(MEMBER_FUNCTION_DESC == "Co-Advisor", na.rm = TRUE),
    committee_member_memberships = sum(
      MEMBER_FUNCTION_DESC == "Committee Member",
      na.rm = TRUE
    ),
    outside_member_memberships = sum(
      MEMBER_FUNCTION_DESC == "Outside Member",
      na.rm = TRUE
    ),
    faculty_participants = n_distinct(MEMBER_NAME, na.rm = TRUE),
    arec_program_memberships = sum(str_detect(PROGRAM, "^AREC-"), na.rm = TRUE),
    other_program_memberships = sum(
      !str_detect(PROGRAM, "^AREC-"),
      na.rm = TRUE
    ),
    partial_year = first(year) == 2026,
    .groups = "drop"
  ) %>%
  right_join(tibble(year = REPORT_YEARS), by = "year") %>%
  mutate(
    across(
      c(
        committee_memberships_beginning, advisor_memberships,
        coadvisor_memberships, committee_member_memberships,
        outside_member_memberships, faculty_participants,
        arec_program_memberships, other_program_memberships
      ),
      ~ replace_na(.x, 0L)
    ),
    partial_year = year == 2026
  )

write_csv(
  table_f2_supplement,
  file.path(out_dir, "table_F2_grad_committee_mentoring_activity.csv")
)

# ==============================================================================
# G1 and G2. Student engagement -- fields supported by current sources
# ==============================================================================

student_awards_by_year <- awards %>%
  filter(Year %in% REPORT_YEARS, .data[["Faculty or Student?"]] == "Student") %>%
  count(year = as.integer(Year), name = "student_awards")

student_publications_by_year <- publication_level %>%
  filter(year %in% REPORT_YEARS, in_reporting_window, student_coauthored) %>%
  group_by(year) %>%
  summarise(
    student_coauthored_publications = n_distinct(publication_key),
    other_student_scholarly_products = n_distinct(
      publication_key[!journal_article_countable]
    ),
    .groups = "drop"
  )

table_g1 <- tibble(year = REPORT_YEARS) %>%
  left_join(student_publications_by_year, by = "year") %>%
  left_join(student_awards_by_year, by = "year") %>%
  mutate(
    undergraduate_research_participants = NA_integer_,
    graduate_research_assistants = NA_integer_,
    student_conference_presentations = NA_integer_,
    community_engaged_research_projects = NA_integer_,
    across(
      c(student_coauthored_publications,
        other_student_scholarly_products, student_awards),
      ~ replace_na(.x, 0L)
    ),
    partial_year = year == 2026,
    coverage_note = paste(
      "Current sources support student-coauthored publications and student awards only;",
      "student level, GRA appointments, student presentations, and community-engaged",
      "student projects are unavailable."
    )
  ) %>%
  select(
    year, undergraduate_research_participants, graduate_research_assistants,
    student_coauthored_publications, student_conference_presentations,
    student_awards, community_engaged_research_projects,
    other_student_scholarly_products, partial_year, coverage_note
  )

external_publication_partners <- publication_partner_rows %>%
  group_by(publication_key) %>%
  summarise(
    external_collaborator = collapse_values(institution),
    .groups = "drop"
  )

table_g2 <- publication_level %>%
  filter(year %in% REPORT_YEARS, in_reporting_window, student_coauthored) %>%
  left_join(external_publication_partners, by = "publication_key") %>%
  transmute(
    year,
    output_type = type,
    undergraduate_or_graduate_participation = "Not available",
    research_area = research_areas,
    title = title_short,
    conference_journal_or_venue = venue,
    peer_reviewed_or_juried = verified_peer_reviewed,
    external_collaborator = coalesce(
      external_collaborator,
      "None identified or OpenAlex affiliation unavailable"
    ),
    resulting_placement_award_or_outcome = NA_character_,
    student_evidence = "See publication-level student classification",
    partial_year = year == 2026
  ) %>%
  arrange(year, output_type, title)

write_csv(table_g1, file.path(out_dir, "table_G1_student_engagement_partial.csv"))
write_csv(table_g2, file.path(out_dir, "table_G2_student_coauthored_outputs.csv"))

# ==============================================================================
# Empty, schema-valid templates for externally requested information
# ==============================================================================

write_csv(
  tibble(
    institution = character(), comparison_unit = character(), sri = double(),
    percentile_or_rank = character(), number_of_faculty = integer(),
    sri_per_faculty = double(), comparison_year = integer(),
    notes_on_comparability = character()
  ),
  file.path(out_dir, "table_B1_AA_SRI_template.csv")
)

write_csv(
  tibble(
    indicator = c(
      "Journal articles", "Citations", "Citations per publication", "Grants",
      "Grant dollars", "Books", "Conference proceedings", "Awards and honors",
      "Cross-institutional collaboration"
    ),
    csu_department_value = NA_real_, peer_median = NA_real_,
    peer_maximum = NA_real_, csu_percentile = NA_real_,
    interpretation = NA_character_, comparison_year = NA_integer_,
    metric_definition = NA_character_, notes_on_comparability = NA_character_
  ),
  file.path(out_dir, "table_B2_AA_productivity_radar_template.csv")
)

write_csv(
  tibble(
    multistate_project = character(), project_title_or_focus = character(),
    years_of_participation = character(), department_faculty_involved = character(),
    leadership_role = character(), participating_institutions = character(),
    major_outputs = character(), source = character()
  ),
  file.path(out_dir, "table_E4_multistate_projects_template.csv")
)

write_csv(
  tibble(
    career_stage = c(
      "New faculty", "Assistant professors", "All faculty", "Associate professors"
    ),
    mentoring_mechanism = c(
      "Onboarding", "Mentor committee", "Annual promotion-and-tenure committee review",
      "Optional mentor committee"
    ),
    frequency = NA_character_, participants = NA_character_,
    intended_outcome = NA_character_, source_or_contact = NA_character_
  ),
  file.path(out_dir, "table_F2_faculty_mentoring_supports_template.csv")
)

message(
  "Stage 06 complete. A1 faculty rows: ", nrow(table_a1),
  ". E1 years: ", nrow(table_e1),
  ". F1 research recognitions: ", nrow(table_f1),
  ". G2 student-coauthored outputs: ", nrow(table_g2), "."
)
