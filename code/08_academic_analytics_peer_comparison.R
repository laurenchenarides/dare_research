# ==============================================================================
# Script Name:  08_academic_analytics_peer_comparison.R
# Purpose:      Build reproducible Academic Analytics peer-benchmarking tables
#               and figures for Section 4.
#
# Peer definition supplied by the department:
#   - Institution type: Public
#   - Land-grant institutions only
#   - Academic Analytics comparison discipline: Agricultural Economics
#   - 36 units and 745 faculty before the DARE roster adjustment
#
# DARE unit-modeling adjustment:
#   Exclude four faculty without a research role from the benchmarking roster.
#   Academic Analytics Stage 1 results are authoritative for adjusted unit-level
#   publication and citation totals because faculty-level records can count an
#   internally coauthored work once for each participating faculty member.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

EXPECTED_PEER_UNITS <- 36L
EXPECTED_PEER_FACULTY <- 745L
EXPECTED_DARE_FACULTY_RAW <- 25L
EXPECTED_DARE_FACULTY_FILTERED <- 21L

DARE_INSTITUTION <- "Colorado State University"
DARE_UNIT <- "Agricultural and Resource Economics, Department of"
PEER_FILTER_DESCRIPTION <- "Public, land-grant institutions"

EXCLUDED_FACULTY <- c(
  "CHOUINARD, HAYLEY",
  "PRITCHETT, JAMES G",
  "PERRY, GREGORY MERRILL",
  "ENNS, KELLIE J"
)

CSU_GREEN <- "#1E4D2B"
SLATE <- "#59636E"
SAGE <- "#6F8173"
WARM_GRAY <- "#A7A8AA"
GOLD <- "#C69214"
DARK_TEXT <- "#1F2937"

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "README.md")) &&
        file.exists(file.path(current, "CODEBOOK.md"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found.", call. = FALSE)
    }
    current <- parent
  }
}

# Several Academic Analytics filenames exceed the traditional Windows path
# limit when combined with the OneDrive project path. A temporary subst drive
# keeps reads reproducible without renaming or modifying the source exports.
create_short_project_root <- function(root) {
  if (.Platform$OS.type != "windows") {
    return(list(root = root, drive = NA_character_))
  }

  root_native <- normalizePath(root, winslash = "\\", mustWork = TRUE)
  candidates <- paste0(c("Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), ":")

  for (drive in candidates) {
    if (!dir.exists(paste0(drive, "/"))) {
      status <- suppressWarnings(system2("subst", c(drive, shQuote(root_native))))
      if (identical(status, 0L) && dir.exists(paste0(drive, "/"))) {
        return(list(root = paste0(drive, "/"), drive = drive))
      }
    }
  }

  stop(
    "Could not create a temporary short drive for the long Academic Analytics paths.",
    call. = FALSE
  )
}

normalize_person_name <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]+", " ") %>%
    str_squish()
}

find_one_file <- function(files, pattern, label) {
  hits <- files[str_detect(basename(files), regex(pattern, ignore_case = TRUE))]
  if (length(hits) != 1L) {
    stop(
      label, ": expected one file matching ", pattern,
      "; found ", length(hits), ".",
      call. = FALSE
    )
  }
  hits
}

read_aa_csv <- function(path) {
  read_csv(
    path,
    show_col_types = FALSE,
    na = c("", "NA"),
    name_repair = "unique"
  )
}

metric_row <- function(unit_model, stage, metric) {
  result <- unit_model %>% filter(Stage == stage, Metric == metric)
  if (nrow(result) != 1L) {
    stop(
      "Expected one ", stage, " row for metric '", metric,
      "'; found ", nrow(result), ".",
      call. = FALSE
    )
  }
  result
}

interpret_percentile <- function(percentile) {
  case_when(
    is.na(percentile) ~ "Not benchmarked",
    percentile >= 75 ~ "Strength",
    percentile >= 40 ~ "Comparable",
    TRUE ~ "Opportunity"
  )
}

theme_dare <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", color = CSU_GREEN),
      plot.subtitle = element_text(color = DARK_TEXT),
      plot.caption = element_text(hjust = 0, color = SLATE),
      axis.text = element_text(color = DARK_TEXT),
      legend.position = "bottom",
      plot.margin = margin(10, 18, 10, 10)
    )
}

main <- function() {
  project_root <- find_project_root()
  short_root <- create_short_project_root(project_root)
  if (!is.na(short_root$drive)) {
    on.exit(
      suppressWarnings(system2("subst", c(short_root$drive, "/D"))),
      add = TRUE
    )
  }

  io_root <- short_root$root
  aa_dir <- file.path(io_root, "academic_analytics")
  out_dir <- file.path(io_root, "output", "academic_analytics")
  figure_dir <- file.path(io_root, "output", "figures", "academic_analytics")

  if (!dir.exists(aa_dir)) {
    stop("academic_analytics directory not found.", call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  csv_files <- list.files(
    aa_dir,
    pattern = "[.]csv$",
    recursive = TRUE,
    full.names = TRUE
  )

  full_data_path <- find_one_file(
    csv_files, "Full Data[.]csv_?[.]csv$", "Full peer export"
  )
  faculty_path <- find_one_file(
    csv_files, "Faculty List[.]csv$", "Faculty list"
  )
  unit_model_path <- find_one_file(
    csv_files, "Unit Modeling - Summary[.]csv$", "Unit modeling summary"
  )
  article_market_path <- find_one_file(
    csv_files, "Article Marketshare[.]csv$", "Article market share"
  )
  book_market_path <- find_one_file(
    csv_files, "Book Marketshare[.]csv$", "Book market share"
  )
  chapter_market_path <- find_one_file(
    csv_files, "Chapter Marketshare[.]csv$", "Chapter market share"
  )
  within_unit_path <- find_one_file(
    csv_files, "Collaborations - Within Unit[.]csv$", "Within-unit collaborations"
  )
  across_units_path <- find_one_file(
    csv_files, "Collaborations - Across Units[.]csv$", "Across-unit collaborations"
  )
  across_institutions_path <- find_one_file(
    csv_files,
    "Collaborations - Across Institutions[.]csv$",
    "Across-institution collaborations"
  )

  full_data <- read_aa_csv(full_data_path)
  faculty <- read_aa_csv(faculty_path)
  unit_model <- read_aa_csv(unit_model_path)

  if (nrow(full_data) != EXPECTED_PEER_UNITS) {
    stop(
      "Peer export contains ", nrow(full_data), " units; expected ",
      EXPECTED_PEER_UNITS, ".",
      call. = FALSE
    )
  }
  if (sum(full_data$facultycount, na.rm = TRUE) != EXPECTED_PEER_FACULTY) {
    stop(
      "Peer export contains ", sum(full_data$facultycount, na.rm = TRUE),
      " faculty; expected ", EXPECTED_PEER_FACULTY, ".",
      call. = FALSE
    )
  }
  if (nrow(faculty) != EXPECTED_DARE_FACULTY_RAW) {
    stop(
      "DARE faculty export contains ", nrow(faculty), " rows; expected ",
      EXPECTED_DARE_FACULTY_RAW, ".",
      call. = FALSE
    )
  }

  excluded_keys <- normalize_person_name(EXCLUDED_FACULTY)
  faculty_audit <- faculty %>%
    mutate(
      faculty_name_key = normalize_person_name(Name),
      included_in_research_benchmark = !(faculty_name_key %in% excluded_keys),
      inclusion_note = if_else(
        included_in_research_benchmark,
        "Included in research-active DARE benchmark",
        "Excluded at department direction: no research role for this benchmark"
      )
    ) %>%
    select(
      Name,
      AAUID,
      `Faculty Rank`,
      included_in_research_benchmark,
      inclusion_note,
      Articles,
      Citations,
      Awards,
      Books,
      Chapters,
      `Conf Proc`,
      Grants,
      `Grant Dollars`
    )

  missing_exclusions <- setdiff(
    excluded_keys,
    normalize_person_name(faculty$Name)
  )
  if (length(missing_exclusions) > 0L) {
    stop(
      "Requested exclusions not found in faculty export: ",
      paste(missing_exclusions, collapse = "; "),
      call. = FALSE
    )
  }

  retained_faculty <- sum(faculty_audit$included_in_research_benchmark)
  if (retained_faculty != EXPECTED_DARE_FACULTY_FILTERED) {
    stop(
      "Filtered DARE roster contains ", retained_faculty,
      " faculty; expected ", EXPECTED_DARE_FACULTY_FILTERED, ".",
      call. = FALSE
    )
  }

  stage1_faculty <- metric_row(
    unit_model, "Stage 1", "Number of Faculty"
  )$Value
  if (!isTRUE(all.equal(stage1_faculty, as.numeric(retained_faculty)))) {
    stop(
      "Stage 1 faculty count does not match the filtered faculty roster.",
      call. = FALSE
    )
  }

  write_csv(
    faculty_audit,
    file.path(out_dir, "aa_faculty_inclusion_audit.csv")
  )
  write_csv(
    unit_model %>% arrange(Stage, Metric),
    file.path(out_dir, "aa_unit_modeling_all_metrics.csv")
  )

  # --------------------------------------------------------------------------
  # Table B1. Scholarly Research Index compared with public land-grant peers
  # --------------------------------------------------------------------------

  stage1_sri <- metric_row(
    unit_model, "Stage 1", "Scholarly Research Index"
  )

  table_b1 <- full_data %>%
    transmute(
      institution = institutionname,
      comparison_unit = unitname,
      faculty_count = facultycount,
      scholarly_research_index = sri
    ) %>%
    mutate(
      faculty_count = if_else(
        institution == DARE_INSTITUTION,
        as.numeric(retained_faculty),
        faculty_count
      ),
      scholarly_research_index = if_else(
        institution == DARE_INSTITUTION,
        stage1_sri$Value,
        scholarly_research_index
      )
    ) %>%
    mutate(
      derived_rank = min_rank(desc(scholarly_research_index)),
      derived_percentile =
        (n() - derived_rank + 1) / n() * 100,
      comparison_year = unique(full_data$releasename)[1],
      comparability_note = if_else(
        institution == DARE_INSTITUTION,
        paste0(
          "DARE Stage 1 model; excludes ",
          length(EXCLUDED_FACULTY),
          " faculty without a research role"
        ),
        "Academic Analytics public land-grant peer export"
      )
    ) %>%
    arrange(derived_rank, institution)

  dare_b1 <- table_b1 %>% filter(institution == DARE_INSTITUTION)
  if (nrow(dare_b1) != 1L ||
      dare_b1$derived_rank != stage1_sri$Rank ||
      abs(dare_b1$derived_percentile - stage1_sri$Percentile) > 0.01) {
    stop(
      "Reconstructed SRI rank or percentile does not match Stage 1.",
      call. = FALSE
    )
  }

  write_csv(
    table_b1,
    file.path(out_dir, "table_B1_AA_SRI_peer_comparison.csv")
  )

  # --------------------------------------------------------------------------
  # Table B2. Size-adjusted productivity indicators
  # --------------------------------------------------------------------------

  metric_map <- tribble(
    ~indicator, ~stage_metric, ~peer_column, ~unit,
    "Articles per faculty member", "Articles per Faculty Member", "articlecountperfaculty", "count per faculty",
    "Citations per faculty member", "Citations per Faculty Member", "citationcountperfaculty", "count per faculty",
    "Citations per publication", "Citations per Publication", "citationcountperarticle", "count per publication",
    "Grants per faculty member", "Grants per Faculty Member", "grantcountperfaculty", "count per faculty",
    "Grant dollars per faculty member", "Grant Dollars per Faculty Member", "grantdollarsperfaculty", "dollars per faculty",
    "Books per faculty member", "Book Publications per Faculty", "bookcountperfaculty", "count per faculty",
    "Chapters per faculty member", "Chapter Publications per Faculty", "chaptercountperfaculty", "count per faculty",
    "Conference proceedings per faculty member", "Conference Proceedings per Faculty Member", "confproccountperfaculty", "count per faculty",
    "Awards per faculty member", "Awards per Faculty Member", "awardcountperfaculty", "count per faculty"
  )

  build_metric_comparison <- function(indicator, stage_metric, peer_column, unit) {
    stage_row <- metric_row(unit_model, "Stage 1", stage_metric)
    peer_values <- full_data[[peer_column]]
    peer_values[full_data$institutionname == DARE_INSTITUTION] <- stage_row$Value

    tibble(
      indicator = indicator,
      department_value = stage_row$Value,
      peer_median = median(peer_values, na.rm = TRUE),
      peer_75th_percentile = as.numeric(
        quantile(peer_values, probs = 0.75, na.rm = TRUE, names = FALSE)
      ),
      peer_maximum = max(peer_values, na.rm = TRUE),
      department_rank = stage_row$Rank,
      department_percentile = stage_row$Percentile,
      peer_units = length(peer_values),
      unit = unit,
      interpretation = interpret_percentile(stage_row$Percentile),
      academic_analytics_metric = stage_metric,
      academic_analytics_field = peer_column
    )
  }

  table_b2 <- pmap_dfr(metric_map, build_metric_comparison)

  write_csv(
    table_b2,
    file.path(out_dir, "table_B2_AA_productivity_peer_comparison.csv")
  )

  # Citation subset for the publication-impact appendix table.
  citation_metrics <- c(
    "Total Citations",
    "Citations per Faculty Member",
    "Citations per Publication",
    "Percentage of Faculty With a Citation",
    "Percentage of Authors With a Citation"
  )
  table_d4_aa <- unit_model %>%
    filter(Stage == "Stage 1", Metric %in% citation_metrics) %>%
    transmute(
      indicator = Metric,
      department_value = Value,
      department_rank = Rank,
      department_percentile = Percentile,
      peer_units = EXPECTED_PEER_UNITS,
      data_source = "Academic Analytics",
      data_release = Release
    ) %>%
    arrange(desc(department_percentile))

  write_csv(
    table_d4_aa,
    file.path(out_dir, "table_D4_AA_citation_impact_indicators.csv")
  )

  # --------------------------------------------------------------------------
  # Adjusted market-share comparison
  # --------------------------------------------------------------------------

  article_market <- read_aa_csv(article_market_path) %>%
    filter(Institution == DARE_INSTITUTION)
  book_market <- read_aa_csv(book_market_path) %>%
    filter(Institution == DARE_INSTITUTION)
  chapter_market <- read_aa_csv(chapter_market_path) %>%
    filter(Institution == DARE_INSTITUTION)

  if (nrow(article_market) != 1L ||
      nrow(book_market) != 1L ||
      nrow(chapter_market) != 1L) {
    stop("Could not identify one CSU row in each market-share export.", call. = FALSE)
  }

  market_map <- tribble(
    ~output_type, ~stage_metric, ~raw_dare_output, ~raw_peer_output,
    "Articles", "Total Articles", article_market$Articles, article_market$`Peer Group Articles`,
    "Books", "Total Number of Books", book_market$Books, book_market$`Peer Books`,
    "Chapters", "Total Number of Chapters", chapter_market$Chapters, chapter_market$`Peer Chapters`
  )

  adjusted_peer_faculty <- EXPECTED_PEER_FACULTY - length(EXCLUDED_FACULTY)

  market_share <- market_map %>%
    mutate(
      filtered_dare_output = map_dbl(
        stage_metric,
        ~ metric_row(unit_model, "Stage 1", .x)$Value
      ),
      adjusted_peer_output =
        raw_peer_output - raw_dare_output + filtered_dare_output,
      dare_output_share = filtered_dare_output / adjusted_peer_output,
      dare_faculty_share = retained_faculty / adjusted_peer_faculty,
      output_to_faculty_share_ratio = dare_output_share / dare_faculty_share,
      filtered_dare_faculty = retained_faculty,
      adjusted_peer_faculty = adjusted_peer_faculty
    ) %>%
    select(
      output_type,
      filtered_dare_output,
      adjusted_peer_output,
      dare_output_share,
      dare_faculty_share,
      output_to_faculty_share_ratio,
      filtered_dare_faculty,
      adjusted_peer_faculty
    )

  write_csv(
    market_share,
    file.path(out_dir, "aa_market_share_filtered.csv")
  )

  # --------------------------------------------------------------------------
  # Collaboration outputs with the same DARE roster exclusion
  # --------------------------------------------------------------------------

  within_unit <- read_aa_csv(within_unit_path) %>%
    mutate(
      unit_scholar_key = normalize_person_name(`Unit Scholar`),
      collaborator_key = normalize_person_name(`Collab Scholar`)
    ) %>%
    filter(
      !(unit_scholar_key %in% excluded_keys),
      !(collaborator_key %in% excluded_keys),
      unit_scholar_key != collaborator_key
    ) %>%
    mutate(
      faculty_1 = pmin(`Unit Scholar`, `Collab Scholar`),
      faculty_2 = pmax(`Unit Scholar`, `Collab Scholar`)
    ) %>%
    group_by(faculty_1, faculty_2) %>%
    summarise(
      across(starts_with("Co-Authored"), ~ max(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(desc(`Co-Authored Articles`), faculty_1, faculty_2)

  across_units <- read_aa_csv(across_units_path) %>%
    mutate(unit_scholar_key = normalize_person_name(`Unit Scholar`)) %>%
    filter(
      !(unit_scholar_key %in% excluded_keys),
      `Collab Unit` != DARE_UNIT
    ) %>%
    group_by(`Collab Unit`) %>%
    summarise(
      dare_faculty = n_distinct(`Unit Scholar`),
      collaborators = n_distinct(`Collab Scholar`),
      across(starts_with("Co-Authored"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(desc(`Co-Authored Articles`), `Collab Unit`)

  external_collaboration <- read_aa_csv(across_institutions_path) %>%
    mutate(unit_scholar_key = normalize_person_name(`Unit Scholar`)) %>%
    filter(
      !(unit_scholar_key %in% excluded_keys),
      `Collab Institution` != DARE_INSTITUTION
    ) %>%
    group_by(`Collab Institution`) %>%
    summarise(
      dare_faculty = n_distinct(`Unit Scholar`),
      collaborators = n_distinct(`Collab Scholar`),
      across(starts_with("Co-Authored"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(desc(`Co-Authored Articles`), `Collab Institution`)

  collaboration_summary <- tribble(
    ~indicator, ~value, ~interpretation_note,
    "Research-active DARE faculty in the benchmark", retained_faculty, "Stage 1 faculty count",
    "Within-DARE faculty collaboration pairs", nrow(within_unit), "Distinct faculty pairs after removing reciprocal duplicate rows",
    "Within-DARE pairwise article links", sum(within_unit$`Co-Authored Articles`), "Pairwise links, not unique publications",
    "Other CSU units represented", nrow(across_units), "Units with at least one filtered DARE collaboration record",
    "External institutions represented", nrow(external_collaboration), "Institutions outside CSU with at least one filtered DARE collaboration record",
    "External collaborators represented", sum(external_collaboration$collaborators), "Distinct collaborators within institution; a person may appear under more than one institution only if AA records do so"
  )

  write_csv(
    within_unit,
    file.path(out_dir, "aa_collaboration_within_DARE_filtered.csv")
  )
  write_csv(
    across_units,
    file.path(out_dir, "aa_collaboration_across_CSU_units_filtered.csv")
  )
  write_csv(
    external_collaboration,
    file.path(out_dir, "aa_collaboration_external_institutions_filtered.csv")
  )
  write_csv(
    collaboration_summary,
    file.path(out_dir, "aa_collaboration_summary_filtered.csv")
  )

  # --------------------------------------------------------------------------
  # Metadata and quality checks
  # --------------------------------------------------------------------------

  metadata <- tibble(
    item = c(
      "Academic Analytics release",
      "Academic Analytics export creation date",
      "Peer filter",
      "Comparison discipline",
      "Peer units",
      "Peer faculty before DARE adjustment",
      "DARE faculty before adjustment",
      "DARE faculty excluded",
      "DARE faculty after adjustment",
      "Peer faculty after DARE adjustment",
      "DARE Stage 1 SRI",
      "DARE Stage 1 SRI rank",
      "DARE Stage 1 SRI percentile"
    ),
    value = c(
      unique(full_data$releasename)[1],
      unique(full_data$createddate)[1],
      PEER_FILTER_DESCRIPTION,
      unique(full_data$comparisongroupname)[1],
      as.character(nrow(full_data)),
      as.character(sum(full_data$facultycount)),
      as.character(nrow(faculty)),
      as.character(length(EXCLUDED_FACULTY)),
      as.character(retained_faculty),
      as.character(adjusted_peer_faculty),
      as.character(stage1_sri$Value),
      as.character(stage1_sri$Rank),
      as.character(stage1_sri$Percentile)
    )
  )

  write_csv(metadata, file.path(out_dir, "aa_analysis_metadata.csv"))

  # --------------------------------------------------------------------------
  # Figures
  # --------------------------------------------------------------------------

  figure_sri_data <- table_b1 %>%
    mutate(
      institution_label = str_wrap(institution, width = 34),
      institution_label = reorder(
        institution_label,
        scholarly_research_index
      ),
      department = if_else(
        institution == DARE_INSTITUTION,
        "DARE (filtered)",
        "Public land-grant peer"
      )
    )

  peer_sri_median <- median(
    figure_sri_data$scholarly_research_index,
    na.rm = TRUE
  )

  figure_sri <- ggplot(
    figure_sri_data,
    aes(x = scholarly_research_index, y = institution_label)
  ) +
    geom_vline(
      xintercept = peer_sri_median,
      linetype = "dashed",
      color = WARM_GRAY,
      linewidth = 0.55
    ) +
    geom_segment(
      aes(x = peer_sri_median, xend = scholarly_research_index, yend = institution_label),
      color = "#D5D8DA",
      linewidth = 0.55
    ) +
    geom_point(
      aes(color = department, size = department),
      alpha = 0.95
    ) +
    scale_color_manual(
      values = c(
        "DARE (filtered)" = CSU_GREEN,
        "Public land-grant peer" = SLATE
      )
    ) +
    scale_size_manual(
      values = c(
        "DARE (filtered)" = 3.4,
        "Public land-grant peer" = 2.0
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = pretty_breaks(),
      expand = expansion(mult = c(0.03, 0.08))
    ) +
    labs(
      title = "DARE's filtered Scholarly Research Index is in the top decile",
      subtitle = paste0(
        "SRI = ", number(stage1_sri$Value, accuracy = 0.1),
        "; tied for rank ", stage1_sri$Rank,
        " among ", EXPECTED_PEER_UNITS,
        " public land-grant Agricultural Economics units"
      ),
      x = "Scholarly Research Index",
      y = NULL,
      color = NULL,
      caption = paste0(
        "Dashed line marks the peer median. DARE Stage 1 excludes four faculty without a research role.\n",
        "Source: Academic Analytics release ", unique(full_data$releasename)[1], "."
      )
    ) +
    theme_dare(base_size = 10) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )

  ggsave(
    file.path(figure_dir, "figure_AA_SRI_peer_comparison.png"),
    figure_sri,
    width = 9,
    height = 11,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  figure_productivity_data <- table_b2 %>%
    mutate(
      indicator = str_wrap(indicator, width = 34),
      indicator = reorder(indicator, department_percentile),
      interpretation = factor(
        interpretation,
        levels = c("Opportunity", "Comparable", "Strength")
      )
    )

  figure_productivity <- ggplot(
    figure_productivity_data,
    aes(x = department_percentile, y = indicator)
  ) +
    geom_vline(
      xintercept = c(50, 75),
      linetype = c("dashed", "solid"),
      color = c(WARM_GRAY, SAGE),
      linewidth = c(0.5, 0.45)
    ) +
    geom_segment(
      aes(x = 0, xend = department_percentile, yend = indicator),
      color = "#D5D8DA",
      linewidth = 0.75
    ) +
    geom_point(
      aes(color = interpretation),
      size = 3.5
    ) +
    geom_text(
      aes(label = number(department_percentile, accuracy = 0.1)),
      hjust = -0.15,
      size = 3.3,
      color = DARK_TEXT
    ) +
    scale_color_manual(
      values = c(
        "Strength" = CSU_GREEN,
        "Comparable" = SLATE,
        "Opportunity" = GOLD
      )
    ) +
    scale_x_continuous(
      limits = c(0, 106),
      breaks = seq(0, 100, 25),
      labels = label_percent(scale = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = "DARE research productivity relative to public land-grant peers",
      x = "DARE percentile among 36 comparison units",
      y = NULL,
      color = NULL,
      caption = paste0(
        "Strength = 75th percentile or higher; comparable = 40th-74.9th; opportunity = below 40th.\n",
        "Metric coverage periods vary in Academic Analytics; source release ",
        unique(full_data$releasename)[1], "."
      )
    ) +
    theme_dare() +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )

  ggsave(
    file.path(figure_dir, "figure_AA_productivity_percentiles.png"),
    figure_productivity,
    width = 9,
    height = 6.6,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  figure_market_data <- market_share %>%
    mutate(output_type = factor(output_type, levels = rev(c("Articles", "Books", "Chapters"))))

  figure_market <- ggplot(figure_market_data, aes(y = output_type)) +
    geom_segment(
      aes(x = dare_faculty_share, xend = dare_output_share, yend = output_type),
      color = WARM_GRAY,
      linewidth = 1.0
    ) +
    geom_point(
      aes(x = dare_faculty_share, color = "Faculty share"),
      size = 3.2
    ) +
    geom_point(
      aes(x = dare_output_share, color = "Output share"),
      size = 3.8
    ) +
    geom_text(
      aes(x = dare_output_share, label = percent(dare_output_share, accuracy = 0.1)),
      hjust = -0.25,
      size = 3.3,
      color = DARK_TEXT
    ) +
    scale_color_manual(
      values = c("Faculty share" = SLATE, "Output share" = CSU_GREEN)
    ) +
    scale_x_continuous(
      labels = label_percent(accuracy = 1),
      limits = c(0, max(figure_market_data$dare_output_share) * 1.2),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = "DARE's share of books and chapters exceeds its faculty share",
      x = "Share of public land-grant peer-group total",
      y = NULL,
      color = NULL,
      caption = paste0(
        "Peer totals were adjusted to replace DARE's unfiltered values with its Stage 1 values.\n",
        "Source: Academic Analytics release ", unique(full_data$releasename)[1], "."
      )
    ) +
    theme_dare() +
    theme(panel.grid.major.y = element_blank())

  ggsave(
    file.path(figure_dir, "figure_AA_market_share_vs_faculty_share.png"),
    figure_market,
    width = 8.5,
    height = 4.6,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  plot_collaboration_bars <- function(data, category, title, subtitle, filename) {
    category_sym <- rlang::ensym(category)
    plot_data <- data %>%
      slice_max(`Co-Authored Articles`, n = 10, with_ties = FALSE) %>%
      mutate(
        category_label = str_wrap(as.character(!!category_sym), width = 35),
        category_label = reorder(category_label, `Co-Authored Articles`)
      )

    plot <- ggplot(
      plot_data,
      aes(x = `Co-Authored Articles`, y = category_label)
    ) +
      geom_col(fill = CSU_GREEN, width = 0.68) +
      geom_text(
        aes(label = `Co-Authored Articles`),
        hjust = -0.2,
        size = 3.2,
        color = DARK_TEXT
      ) +
      scale_x_continuous(
        breaks = pretty_breaks(),
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Pairwise coauthored-article links",
        y = NULL,
        caption = paste0(
          "Pairwise links are summed across DARE faculty-collaborator relationships and are not counts of unique publications.\n",
          "Four faculty without a research role are excluded. Source: Academic Analytics release ",
          unique(full_data$releasename)[1], "."
        )
      ) +
      theme_dare() +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      )

    ggsave(
      file.path(figure_dir, filename),
      plot,
      width = 9,
      height = 6,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }

  plot_collaboration_bars(
    across_units,
    `Collab Unit`,
    "DARE collaboration across CSU units",
    "Ten CSU units with the largest numbers of pairwise coauthored-article links",
    "figure_AA_collaboration_across_CSU_units.png"
  )

  plot_collaboration_bars(
    external_collaboration,
    `Collab Institution`,
    "DARE cross-institutional collaboration",
    "Ten external institutions with the largest numbers of pairwise coauthored-article links",
    "figure_AA_external_collaboration.png"
  )

  message(
    "Academic Analytics analysis complete: ",
    normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  )
  message(
    "Figures written to: ",
    normalizePath(figure_dir, winslash = "/", mustWork = TRUE)
  )
}

main()
