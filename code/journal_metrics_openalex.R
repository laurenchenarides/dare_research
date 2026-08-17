# journal_metrics_openalex.R
# Enrich a list of journals with OpenAlex source-level metrics + ISSN.
#
# What you get per journal:
#   issn_l, issn, works_count, cited_by_count,
#   twoyr_mean_citedness  (OpenAlex's citation metric; a proxy for impact factor,
#                          NOT the Clarivate JIF or Scopus CiteScore)
#   h_index, i10_index
#
# What you do NOT get: the official Journal Impact Factor (JCR/Clarivate) or
# CiteScore (Scopus). Those are proprietary and not in OpenAlex. If the review
# needs them, pull from JCR/Scopus via the library and populate manually.

setwd("C:/Users/lachenar/OneDrive - Colostate/CAS DARE Team-5 year Review - Documents/Research and Creative Artistry - LAUREN") 
getwd()

# ---- 0. Install once, then load ---------------------------------------------
# install.packages(c("openalexR", "dplyr", "purrr", "readr", "stringr"))
library(openalexR)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# ---- 1. Credentials ---------------------------------------------------------
# Polite pool (faster) + API key (required since Feb 2025; free key at
# https://openalex.org/settings/api). Set these to your own values.
options(openalexR.mailto = "lauren.chenarides@colostate.edu")
Sys.setenv(OPENALEX_KEY = "U2FPUGLOmS0L57XfZu9H4s")  # or set in .Renviron

# Sanity check FIRST. One row and no 429s means credentials are live.
test <- oa_fetch(entity = "sources", issn = "0002-9092", verbose = TRUE)  # AJAE
stopifnot(nrow(test) == 1)

# ---- 2. Input ---------------------------------------------------------------
# Expect a CSV with at least a `journal` column and, where known, an `issn`
# column (any one valid ISSN per row is fine; hyphenated, e.g. 0002-9092).
# Blank ISSNs are allowed; those rows fall back to a name search.
#
# Example expected file: journals_in.csv
#   journal,issn
#   American Journal of Agricultural Economics,0002-9092
#   Land Economics,
#   Journal of Environmental Economics and Management,0095-0696

journals_in <- read_csv("journals_in.csv", show_col_types = FALSE) %>%
  mutate(
    journal = str_squish(journal),
    issn    = str_squish(ifelse(is.na(issn), "", as.character(issn))),
    issn    = if_else(issn == "", NA_character_, issn)
  )

# ---- 3. Helper: pull the three summary_stats regardless of column shape -----
# openalexR returns summary_stats either as a list-column or as flat columns
# depending on version, so extract defensively.
get_stat <- function(df, key, pos) {
  # flat column already present?
  flat <- names(df)[str_detect(names(df), fixed(key))]
  if (length(flat) >= 1) return(as.numeric(df[[flat[1]]]))
  if ("summary_stats" %in% names(df)) {
    return(vapply(df$summary_stats, function(s) {
      if (is.null(s)) return(NA_real_)
      s <- unlist(s)
      if (!is.null(names(s)) && any(str_detect(names(s), fixed(key)))) {
        return(as.numeric(s[str_detect(names(s), fixed(key))][1]))
      }
      if (length(s) >= pos) return(as.numeric(s[pos])) else return(NA_real_)
    }, numeric(1)))
  }
  rep(NA_real_, nrow(df))
}

tidy_sources <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  first_issn <- function(x) {
    x <- unlist(x); if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
  }
  tibble(
    oa_display_name      = df$display_name,
    oa_type              = if ("type" %in% names(df)) df$type else NA_character_,
    issn_l               = if ("issn_l" %in% names(df)) as.character(df$issn_l) else NA_character_,
    issn_all             = map_chr(df$issn, first_issn),
    works_count          = if ("works_count" %in% names(df)) df$works_count else NA_integer_,
    cited_by_count       = if ("cited_by_count" %in% names(df)) df$cited_by_count else NA_integer_,
    twoyr_mean_citedness = get_stat(df, "2yr_mean_citedness", 1),
    h_index              = get_stat(df, "h_index", 2),
    i10_index            = get_stat(df, "i10_index", 3)
  )
}

# ---- 4a. Fetch by ISSN (preferred, exact) -----------------------------------
issn_rows <- journals_in %>% filter(!is.na(issn))
by_issn <- NULL
if (nrow(issn_rows) > 0) {
  by_issn <- oa_fetch(entity = "sources", issn = issn_rows$issn, verbose = TRUE) %>%
    tidy_sources() %>%
    mutate(match_issn = issn_all)  # keep for joining back
}

# ---- 4b. Fetch by name search (fallback for rows with no ISSN) --------------
fetch_by_name <- function(nm, tries = 5) {
  for (i in seq_len(tries)) {
    res <- tryCatch(
      oa_fetch(entity = "sources", search = nm, per_page = 1, verbose = FALSE),
      error = function(e) e
    )
    if (!inherits(res, "error") && !is.null(res) && nrow(res) > 0) return(res)
    Sys.sleep(min(2^i, 30))   # back off on transient 429s, capped at 30s
  }
  NULL
}

name_rows <- journals_in %>% filter(is.na(issn))
by_name <- NULL
if (nrow(name_rows) > 0) {
  by_name <- map_dfr(name_rows$journal, function(nm) {
    res <- fetch_by_name(nm)
    out <- tidy_sources(res)
    Sys.sleep(0.2)  # gentle spacing between journals
    if (!is.null(out) && nrow(out) > 0) out[1, , drop = FALSE] %>% mutate(query_name = nm)
    else tibble(query_name = nm, oa_display_name = NA_character_)
  })
}

# ---- 5. Stitch back to the original list ------------------------------------
# Join ISSN matches on the ISSN you supplied; join name matches on the name.
out_issn <- issn_rows %>%
  left_join(
    by_issn %>% mutate(issn_hyphen = str_extract(issn_all, "\\d{4}-\\d{3}[\\dXx]")),
    by = c("issn" = "issn_hyphen")
  )

out_name <- name_rows %>%
  left_join(by_name, by = c("journal" = "query_name"))

enriched <- bind_rows(out_issn, out_name) %>%
  select(
    journal, issn,
    oa_display_name, oa_type, issn_l, issn_all,
    works_count, cited_by_count,
    twoyr_mean_citedness, h_index, i10_index
  ) %>%
  arrange(journal)

# ---- 6. Write out -----------------------------------------------------------
write_csv(enriched, "journals_enriched.csv")

# Quick look at anything that didn't match, so you can fix names/ISSNs by hand:
enriched %>% filter(is.na(oa_display_name)) %>% print(n = Inf)