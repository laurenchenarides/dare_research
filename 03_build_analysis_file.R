# ==============================================================================
# Script Name:  03_build_analysis_file.R
# Author:       Lauren Chenarides
# Last updated: July 2026
# Description:  Joins faculty appointment splits to the enriched publication
#               list and builds the analysis-ready files for Section 4. Produces
#               (a) a publication-level file carrying each author's appointment
#               and rank, and (b) a faculty-year panel with headcount and
#               research-FTE denominators for per-capita productivity measures.
#
# Inputs:
#   - `publications_enriched.csv` : Output of 02_openalex_enrich.R
#   - `appointment_splits.csv`    : Current effort distribution, type, and rank
#   - `roster.csv`                : Per-year active flags; splits for departed faculty
#
# Outputs:
#   - `faculty_appointments.csv`  : Cleaned appointment table with join key
#   - `pubs_analysis.csv`         : Publication level + appointment + rank
#   - `faculty_year_panel.csv`    : Faculty-year panel, active flag, research FTE
#   - `dept_year_summary.csv`     : Department totals by year, headcount and FTE
#   - `appointment_join_audit.csv`: Faculty appearing in only one source
#
# Dependencies:
#   - Packages: dplyr, tidyr, stringr, readr, purrr
#
# Notes:
#   - Appointment splits are treated as constant across 2021-2025 per the
#     methodology decision recorded in CODEBOOK.md.
#   - `appointment_splits.csv` is a CURRENT snapshot. Faculty who left before it
#     was produced (Frasier, Hill, Jablonski, Manning) are not in it and take
#     their splits from roster.csv.
#
# Execution Time: Fast
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

in_dir  <- "data"
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# WINDOW <- 2021:2025

# -------------------------
# 1. Helper: extract a last-name join key
# -------------------------

# Handles multi-word surnames ("Anders Van Sandt") and inconsistent
# capitalization ("MARCO Costanigro") in the appointment file.
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

# -------------------------
# 2. Read and clean the appointment file
# -------------------------

appt_raw <- read_csv(file.path(in_dir, "appointment_splits.csv"),
                     show_col_types = FALSE)

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
    last          = extract_last(candidate_name),
    departed_flag = !is.na(.data[[names(appt_raw)[7]]]),   # the unnamed "GONE" column
    is_tt         = faculty_type == "Tenured/Tenure Track",
    split_sum     = pct_teaching + pct_research + pct_service,
    split_sums_100 = split_sum == 100,
    appt_source   = "appointment_splits.csv"
  ) %>%
  select(last, candidate_name, faculty_type, faculty_rank, is_tt,
         pct_teaching, pct_research, pct_service, split_sum, split_sums_100,
         departed_flag, appt_source)

nrow(faculty_appointments)                              # N = 30
sum(faculty_appointments$is_tt)                         # N = 24 tenured/tenure track
sum(!faculty_appointments$split_sums_100)               # N = 2 (Thilmany 51, Bennett 90)

# NOTE ON SPLITS THAT DO NOT SUM TO 100
# Thilmany (51) and Bennett (90) do not total a full appointment. Weighting uses
# the research share itself, so the shortfall does not distort research FTE:
# Thilmany contributes 0.21 FTE either way. No imputation is applied.

# -------------------------
# 3. Bring in departed faculty from the roster
# -------------------------

# The appointment file is a current snapshot and omits faculty who left during
# the window. Their splits come from roster.csv so that 2021-2024 denominators
# are complete.
roster <- read_csv(file.path(in_dir, "roster.csv"), show_col_types = FALSE) %>%
  mutate(last = str_to_title(last))

roster_only <- roster %>%
  filter(!last %in% faculty_appointments$last) %>%
  transmute(
    last,
    candidate_name = str_c(first, " ", last),
    faculty_type   = "Tenured/Tenure Track",   # CONFIRM for each departed member
    faculty_rank   = NA_character_,
    is_tt          = TRUE,
    pct_teaching   = as.numeric(teaching_pct),
    pct_research   = as.numeric(research_pct),
    pct_service    = as.numeric(outreach_pct),
    split_sum      = pct_teaching + pct_research + pct_service,
    split_sums_100 = split_sum == 100,
    departed_flag  = TRUE,
    appt_source    = "roster.csv (departed or not in appointment file)"
  )

nrow(roster_only)     # N = faculty added from roster (Chouinard, Frasier, Hill,
                      #     Jablonski, Manning)

faculty_appointments <- bind_rows(faculty_appointments, roster_only)

write_csv(faculty_appointments, file.path(out_dir, "faculty_appointments.csv"))

# -------------------------
# 4. Join audit
# -------------------------

pubs <- read_csv(file.path(out_dir, "publications_enriched.csv"),
                 show_col_types = FALSE) %>%
  mutate(last = str_to_title(last))

appointment_join_audit <- full_join(
  faculty_appointments %>% distinct(last, appt_source, faculty_type),
  pubs %>% distinct(last) %>% mutate(in_publications = TRUE),
  by = "last"
) %>%
  mutate(
    in_appointments = !is.na(appt_source),
    in_publications = replace_na(in_publications, FALSE),
    audit_note = case_when(
      in_appointments & !in_publications ~ "Appointment record, no publications in window",
      !in_appointments & in_publications ~ "Publications but no appointment record",
      TRUE ~ "Matched"
    )
  ) %>%
  arrange(audit_note, last)

write_csv(appointment_join_audit, file.path(out_dir, "appointment_join_audit.csv"))
count(appointment_join_audit, audit_note)

# -------------------------
# 5. Publication-level analysis file
# -------------------------

pubs_analysis <- pubs %>%
  left_join(
    faculty_appointments %>%
      select(last, faculty_type, faculty_rank, is_tt, pct_research, departed_flag),
    by = "last"
  ) %>%
  mutate(
    research_fte = pct_research / 100
  )

nrow(pubs_analysis)                                  # N = 373
sum(is.na(pubs_analysis$pct_research))               # N = rows with no appointment match

write_csv(pubs_analysis, file.path(out_dir, "pubs_analysis.csv"))

# -------------------------
# 6. Faculty-year panel
# -------------------------

# Long form of the roster's per-year active flags.
active_long <- roster %>%
  select(last, starts_with("y20")) %>%
  pivot_longer(cols = starts_with("y20"), names_to = "year", values_to = "active") %>%
  mutate(year = as.integer(str_remove(year, "^y")),
         active = as.integer(active)) 

# Output counts per faculty-year, research-countable rows only.
output_counts <- pubs_analysis %>%
  count(last, year, name = "n_outputs")

# Citation counts per faculty-year, if 02_openalex_enrich.R has been run.
citation_counts <- pubs_analysis %>%
  group_by(last, year) %>%
  summarize(total_citations = sum(cited_by_count, na.rm = TRUE),
            mean_if = round(mean(impact_factor, na.rm = TRUE), 2),
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
    research_fte    = if_else(active == 1, pct_research / 100, 0),
    # Output per unit of research appointment - undefined at 0% research
    output_per_fte  = if_else(research_fte > 0, n_outputs / research_fte, NA_real_)
  ) %>%
  arrange(last, year) %>%
  filter(active==1)

write_csv(faculty_year_panel, file.path(out_dir, "faculty_year_panel.csv"))

# -------------------------
# 7. Department-year summary
# -------------------------

# Two denominators. Headcount is what a reviewer computes by default; research
# FTE is what the department is actually funded to produce. Reporting both is
# the point - the gap between them is the argument.
dept_year_summary <- faculty_year_panel %>%
  filter(active == 1) %>%
  group_by(year) %>%
  summarize(
    headcount_all     = n(),
    headcount_tt      = sum(is_tt, na.rm = TRUE),
    research_fte      = sum(research_fte, na.rm = TRUE),
    n_outputs         = sum(n_outputs),
    total_citations   = sum(total_citations),
    output_per_head   = round(n_outputs / headcount_all, 2),
    output_per_tt     = round(n_outputs / headcount_tt, 2),
    output_per_fte    = round(n_outputs / research_fte, 2),
    .groups = "drop"
  )

write_csv(dept_year_summary, file.path(out_dir, "dept_year_summary.csv"))
print(dept_year_summary)

# -------------------------
# 8. Rank and appointment-type cuts
# -------------------------

# Rank is new information from the appointment file and supports the mentoring
# and trajectory discussion in Section 4.
by_rank <- faculty_year_panel %>%
  filter(active == 1, !is.na(faculty_rank)) %>%
  group_by(faculty_rank) %>%
  summarize(
    n_faculty_years = n(),
    research_fte    = sum(research_fte),
    n_outputs       = sum(n_outputs),
    output_per_fte  = round(n_outputs / sum(research_fte), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(output_per_fte))

print(by_rank)

by_type <- faculty_year_panel %>%
  filter(active == 1) %>%
  group_by(faculty_type) %>%
  summarize(
    n_faculty_years = n(),
    research_fte    = sum(research_fte, na.rm = TRUE),
    n_outputs       = sum(n_outputs),
    .groups = "drop"
  )

print(by_type)

