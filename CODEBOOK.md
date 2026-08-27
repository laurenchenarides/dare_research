# DARE Section 4 — Publication Extraction Codebook

Source of truth for every publication counting rule. `data/publications_faculty_doi_cleaned.csv` is the curated baseline. `code/02_openalex_enrich.R` applies the rules below and generates the authoritative downstream file, `data/publications_faculty_doi_updated.csv`. Any rule change requires rerunning the affected pipeline stages and documenting the change here.

Sponsored-project rules are maintained separately in `GRANTS-CODEBOOK.md`.

## Window

2021–2026 inclusive. The 2026 observations are partial through the latest refresh, currently August 27, 2026. They are included in publication tables and flagged `edge_2026 = 1`; comparisons must label 2026 as incomplete. Publications count regardless of institutional affiliation at the time of publication, subject to the faculty-activity rule applied in stage 03. Some 2026 entries come from Google Scholar or OpenAlex because CVs often have incomplete current-year coverage.

## Core schema — publication files

| Field | Values | Definition |
|---|---|---|
| `last`, `first` | text | As on the roster, not as on the CV byline |
| `area` | ENRE / Ag and Food / Ag Ed | From roster |
| `year` | 2021–2026 | Assigned by the rule below; 2026 is partial |
| `type` | JA/BC/BK/RP/WP/CP/EX | See type rules |
| `index_class` | a / b / *(blank)* | `a` or `b` for journal articles; blank otherwise |
| `student_coauthor` | Y/N/U | See student rules |
| `dare_coauthors` | 0 or 1 | Takes a value of `1` if publication inludes more than one DARE author. |
| `dare_coauthors_names` | text | Names of all DARE coauthors if `dare_coauthors = 1`. |
| `title_short` | text | Trimmed to ~60 chars, no trailing punctuation |
| `venue` | text | Journal, press, or issuing body as printed on the CV |
| `doi` | valid bare DOI, no URL prefix | Blank if none is available; generated files remove invalid placeholders |
| `source_file` | filename | The `txt/` file the row came from |

`publications_faculty_doi_updated.csv` adds OpenAlex identifiers and types, mapping and review flags, extension and provenance flags, discovery date, `edge_2026`, a reproducible `publication_key`, and publication-affiliation fields. `output/publications_enriched.csv` adds citation, impact-factor, graduate-coauthor, year-audit, and journal-classification evidence used by later stages.

## Index class (journal articles only)

Stage 02 reassigns every journal article from `data/journal_impact_factors_2021_2026.xlsx`; an existing CSV value is never allowed to override the workbook rule.

- `index_class = a` — the normalized journal title matches a workbook journal with at least one populated value in `IF_2021` through `IF_2026`.
- `index_class = b` — the row is a journal article, but its journal is absent from the workbook or the matching workbook row has no populated `IF_2021`–`IF_2026` value.
- **blank** — required for non-journal outputs. A blank journal-article class after stage 02 is an error.

`index_class` applies only to `type = JA`. Books, chapters, reports, and proceedings are never impact-factored and are reported as their own categories.

The classification rule is journal-level, not publication-year-level. The numeric `impact_factor` field separately uses the exact journal-year value where available and otherwise the nearest available workbook year, with earlier years winning equal-distance ties. OpenAlex work coverage and `2yr_mean_citedness` do not determine `index_class` and are not impact factors.

## Year assignment

1. Use the year printed in the citation.
2. "Forthcoming", "accepted", "in press" with **no** year: **exclude**, and log to `data/excluded.csv` with reason `no_year_forthcoming`. These are not lost — they are recoverable once the CV or Crossref supplies a year.
3. "Forthcoming" **with** a stated acceptance year: count in that year, flag `forthcoming_dated`.
4. Reports and non-journal items dated by month: use the stated year.
5. Where a CV gives both an online-first year and an issue year, use the issue year, flag `year_ambiguous`.

## Type

| Code | Includes |
|---|---|
| JA | Peer-reviewed journal article |
| BC | Chapter in an edited volume |
| BK | Authored or edited book |
| RP | Research report, working paper, commissioned report |
| WP | Working paper retained as a separate source code and excluded from the countable-research type list unless the rule is changed explicitly |
| CP | Refereed conference proceeding or transaction |
| EX | Extension publication, newsletter, fact sheet, trade press |

**JA and EX are never pooled.** Several DARE faculty carry majority extension appointments and their EX output is large; folding it into a peer-reviewed count would misrepresent the department to a reviewer checking against Academic Analytics, which does not index extension outputs. Report EX in its own table.

Items listed on a CV as "under review", "in preparation", "submitted", or "R&R" are **not output** and are excluded entirely.

## Student co-authorship

- `Y` where the CV explicitly marks the co-author as a graduate student (asterisk convention, or a "* indicates graduate student" legend).
- `Y` where the co-author appears on the same CV's graduate advising list with a completion year at or after the publication year.
- `N` where the co-author appears on the advising list with a completion year **before** the publication year. These are alumni co-authorships. They are real and worth reporting, but they are not the "engaging students in inquiry" claim Section 4 asks for. Flagged `alumni_coauthor` so they can be recovered as a separate count.
- `U` where the CV gives no basis to decide. Use the `conferred_degrees.xlsx` file to determine student coauthorship.

OpenAlex coauthor names are first matched to the conferred-degree roster by exact normalized name. The five last-name-plus-first-initial candidates recorded in `output/coauthor_match_review.csv` were manually reviewed and approved by the project owner on August 27, 2026; stage 02 therefore includes them as registrar matches while preserving `match_type` as manually reviewed evidence. New non-exact candidates must be reviewed before they are accepted.

## Cross-population of co-authored papers

When any DARE faculty member lists a paper co-authored with other DARE faculty, every DARE co-author receives a row for that paper — even if their own CV omits it. CVs are updated at different times and people forget entries, so a paper present on one CV is authoritative for all its DARE authors.

Each row records `dare_coauthors_names` (semicolon-separated last names of DARE faculty on the paper). 

After all CVs are read, papers collapse to a distinct record (DOI where present, else normalized title+year). For each distinct record, one row is emitted per DARE author. Rows created for an author whose own CV lacked the paper are flagged `added_from_coauthor` and keep the `source_file` of the CV where the paper was actually found, preserving traceability.

Consequence: the sum of per-faculty counts exceeds the department-level distinct-paper count by exactly the volume of within-department collaboration.

## Pipeline and known limitations

1. **CV unpacking is complete.** `code/01_unpack_cvs.py` remains available for reproducibility, but routine publication work begins from `publications_faculty_doi_cleaned.csv`.
2. **CV-to-row extraction is curated.** CVs have no common section headings, citation format, or date placement. Every baseline row carries `source_file`. The current CSV does not contain `source_page`, so page-level traceability would require a future re-extraction or source-page audit; do not claim that the existing CSV provides page-level provenance.
3. **OpenAlex discovery is additive and auditable.** Only works matched through verified roster identifiers and active-year rules may be appended. `added_from_openalex`, `author_match_rule`, `discovery_date`, and the discovery audit preserve provenance. An empty discovery audit means no new rows were accepted in that run; it does not prove the curated baseline is complete.
4. **Statistics are generated.** Stage 03 applies roster, appointment, affiliation, type, extension, and index-class rules. Stage 04 produces the publication tables and figures for all six reporting years, with 2026 clearly identified as partial.

## Verified corrections — August 27, 2026

- `10.1371/journal.pone.0261833` is assigned to Jordan Suter's “Summer Crowds” publication and not to Jesse Burkhardt's lettuce-production paper.
- `10.1080/10871209.2024.2414880` appears once, on Dana Hoag's 2025 wolf-depredation publication.
- `publications_faculty_doi_cleaned - Copy.csv` is byte-for-byte identical to `publications_faculty_doi_cleaned.csv` and is not used by the pipeline.
