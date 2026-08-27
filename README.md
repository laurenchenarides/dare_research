# README.md — DARE Section 4 Data Pipeline

Instructions for AI agent working in this repository. Read this before running anything.

## What this project is

This repo builds the quantitative evidence base for **Section 4 (Research and Creative Artistry)** of the Department of Agricultural and Resource Economics (DARE) 2026 academic program review at Colorado State University. The output is a set of reproducible CSVs and an R analysis pipeline that feed a written narrative submitted to the provost's office.

The owner is Lauren Chenarides (faculty, DARE). Publication reporting covers **2021–2026**. The 2026 results are an explicitly labeled partial year through the latest data refresh (currently August 27, 2026), and rows retain `edge_2026 = 1` so partial-year results remain visible. Grant rules are documented separately and will be finalized in the grants stage.

## Hard constraints

**Every quantitative claim must trace to a CV page or a stated rule in `CODEBOOK.md`.** No number enters the pipeline that cannot be sourced. This constraint outranks convenience, completeness, and speed. When a value is unknown, it stays blank and flagged — it is never estimated, inferred from memory, or filled to make a table look finished.

1. **Never hand-edit pipeline outputs.** The CSVs in `data/` and `output/` are generated. Fix the source or the script, then regenerate. A manual edit that can't be reproduced from inputs breaks the project's core requirement.
2. **Never invent a DOI, dollar amount, impact factor, citation count, or publication year.** If it isn't in a CV, an input file, or an API response, it is blank with a flag. Placeholder DOIs are the literal problem that corrupted an earlier join — invalid DOIs become `NA`, never a shared string.
3. **Never present OpenAlex `2yr_mean_citedness` as an impact factor.** OpenAlex does not carry Clarivate JIF or Scopus CiteScore. Official IF values come only from JCR/Scopus via library access. Label OpenAlex metrics honestly.
4. **Never pool extension output into research counts.** Rows with `extension_output = 1` are excluded from every research productivity figure. They are retained for narrative use only (e.g. explaining heavy-extension appointments).
5. **Never merge the two index classes silently.** `index_class = a` (carries an impact factor) and `b` (peer-reviewed, no impact factor) are always distinguishable in any reported table so a reviewer can reconcile to Academic Analytics.
6. **Preserve the headcount-vs-research-FTE gap.** The department's strongest quantitative argument is that ~27 faculty correspond to ~9.4 research FTE. All three denominators (total headcount, TT headcount, research FTE) must survive into any per-capita table. Do not collapse them to one.
7. **Reproducibility is absolute.** The same inputs must produce the same outputs with no manual step. If a task can't be done reproducibly, say so rather than doing it by hand.

## Repository layout

```
data/        input CSVs and the generated faculty-level files
  publications_faculty_doi_cleaned.csv   curated CV/Google Scholar baseline
  publications_faculty_doi_updated.csv   stage 02 publication output; baseline
                                          plus accepted OpenAlex discoveries
  grants.csv                             one row per grant
  presentations.csv                      one row per faculty-year-talk
  roster.csv                             per-year active flags, appointment splits
  appointment_splits.csv                 effort distribution, faculty type, rank
  journal_impact_factors_2021_2026.xlsx  wide IF lookup (Lauren maintains)
  conferred_degrees.xlsx                 CSU AREC conferred degrees (registrar)
code/
  01_unpack_cvs.py                       CV packets -> per-faculty text
  02_openalex_enrich.R                  Enriches the faculty-DOI publication list 
                                          with OpenAlex metadata
  03_build_analysis_file.R              Joins faculty appointment splits to the 
                                          enriched publication list and builds the 
                                          analysis-ready files
  04_publication_tables.R               Produces the publication and disciplinary-influence tables
output/      generated analysis files (see script headers); safe to delete/regen
CODEBOOK.md  the authoritative rule set — consult before changing any counting logic
README.md    this file
```

## Pipeline stages and how to run them

1. **`code/01_unpack_cvs.py`** — Unpacks CV packets to text. The packets have a `.pdf` extension but most are **ZIP archives of page images plus text**, not true PDFs; a few are genuine PDFs. The script handles both. When a packet fails, inspect the real format with `xxd` before assuming — do not trust the extension.
2. **`code/02_openalex_enrich.R`** — Starts from `publications_faculty_doi_cleaned.csv`, discovers potentially missing works using verified roster identifiers, enriches valid DOIs with OpenAlex metadata, matches graduate coauthors, and reassigns journal index classes from the impact-factor workbook. It writes `data/publications_faculty_doi_updated.csv` and `output/publications_enriched.csv`. API caches are used by default. Set the environment variable `REFRESH_DISCOVERY=true` to rerun author-based discovery or `REFRESH_DOI_CACHE=true` to discard and rebuild the DOI cache for a run.
3. **`code/03_build_analysis_file.R`** — Joins appointment and roster information to `output/publications_enriched.csv`, applies the counting rules, and builds faculty-publication, publication-level, faculty-year, and department-year analysis files.
4. **`code/04_publication_tables.R`** — Produces Tables D1–D4, publication audits, figures, and the combined publication workbook from the stage 03 outputs and `publications_faculty_doi_updated.csv`.

The pipeline is numbered. Run stages in order. Later stages read earlier outputs.

### Publication-file roles

- `publications_faculty_doi_cleaned.csv` is the curated baseline produced after CV extraction and manual cleaning. Stage 02 reads it; later stages do not.
- `publications_faculty_doi_cleaned - Copy.csv` is byte-for-byte identical to the curated baseline as of August 27, 2026. It is redundant and is not read by any script.
- `publications_faculty_doi_updated.csv` is generated by stage 02 and is the authoritative downstream publication file. It contains the baseline rows, any accepted OpenAlex discoveries, normalized DOI values, refreshed `a`/`b` classifications, and provenance/audit fields. Never edit it by hand.
- OpenAlex discovery is additive. CV-curated rows remain authoritative for their publication year, title, and other coded fields; API metadata does not silently overwrite them.

## Counting rules

- **Reporting window** is 2021–2026. Publications from all six years enter the tables and figures. Because 2026 is incomplete, it is flagged `edge_2026` and must be labeled as a partial year in every interpretation or narrative comparison.
- **Publication types:** JA, BC (book chapter), BK (book), RP (research report), CP (conference proceeding). `extension_output = 1` marks extension and non-refereed reports, excluded from research counts.
- **Index class:** Stage 02 reassigns every journal article from `journal_impact_factors_2021_2026.xlsx`. Class `a` means the normalized journal title has at least one populated `IF_2021`–`IF_2026` value in the workbook. Class `b` means the item is a journal article but the workbook supplies no populated impact-factor value for that journal. Non-journal outputs have a blank index class. OpenAlex coverage is not used to assign `a` or `b`.
- **Student coauthor (union rule):** a CV-coded `Y` is authoritative and is never downgraded; a registrar match adds `Y` where the CV did not claim one. `student_evidence` records which source supports each `Y`. A CV-only `Y` that the registrar can't corroborate is expected — the registrar file covers only AREC graduates, not undergraduates, other departments, or other institutions.
- **Cross-population (fallback only):** a co-authored paper is added to a DARE co-author's list only if it is absent from their own CV. If already present, their own entry stands — no propagated duplicate. Per-faculty counts therefore exceed the department distinct-paper count by the volume of internal collaboration; that gap is the interdisciplinary-collaboration evidence, not double counting. Department totals dedupe on DOI (title+year where no DOI).
- **Grants:** the intended reporting window is 2021–2026, with 2026 labeled as partial. Include any award active during that window regardless of start year. Keep funded, submitted, under review, and pending; exclude only not-funded/declined. On shared grants keep the PI's figure. Award amounts were retrieved from the [OVPR](https://vprweb.research.colostate.edu/Proposal-Award-History-Search/Proposal.aspx) where `Date Submitted Between: 01/01/2021 and 08/01/2026` and `Lead Unit = Agricultural + Resource Economics (1172)`. Confirm and encode the final grant-window details when the grants stage begins.
- **Presentations:** one row per faculty-year-talk. Each venue is its own row (a paper presented at three venues is three rows). Typed conference / invited / other; posters flagged.
- **Appointment splits** are held constant across the window. Chouinard is 5% research. Weighting uses the research share itself, so splits that don't total 100 (Thilmany, Bennett) do not distort research FTE.
- **Departed/retired faculty scope** (Hill, Manning, Jablonski, and Perry who left before the window). Count them for the years they were active, consistent with partial-window treatment of faculty hired after 2021.

## Open decisions

- **John Ritten: Continuing vs TT.** He is the highest-output faculty member by a wide margin, so his classification materially moves both numerator and denominator. The appointment file lists him as Continuing; the original instruction was to report on TT faculty. Keep all three denominators so the effect is visible; flag, don't choose.
- **AA alignment.** If Academic Analytics excludes departed faculty, our counts must match or the discrepancy must be stated explicitly.

## Publication corrections verified August 27, 2026

- `10.1371/journal.pone.0261833` belongs to Jordan Suter's “Summer Crowds” paper. The curated and updated files assign it only to that publication.
- `10.1080/10871209.2024.2414880` appears once, on Dana Hoag's 2025 wolf-depredation paper.
- `publications_faculty_doi_cleaned.csv` and `publications_faculty_doi_cleaned - Copy.csv` are exact duplicates. The latter is not a pipeline input.
- Stage 02 converts legacy non-DOI placeholders such as `nodoi`, ISBNs stored in the DOI field, and literal `NA` values to missing DOI values in the generated updated file.

Publication integrity remains reproducibly checked in `output/doi_integrity_audit.csv`; journal matching and classification are checked in `output/journal_if_audit.csv` and `output/if_key_audit.csv`.

Manual-verification priorities for later stages: Magnan (grants did not extract cleanly), Koontz (extension volume), and Thilmany and Seidl (grant dollars via VPR).

## Writing (only when asked to draft narrative)

Follow `DEPT-VOICE.md`. In short: positive declarative statements over contrast constructions; specific numbers over vague quantifiers (replace "numerous" with a count); no em dashes; no evaluative flourishes; no redundancy. Every claim in the narrative names specific faculty and papers before it is written. Vague or overstated claims will not survive reviewer scrutiny.

## Working style

Direct, specific, and willing to push back. When you spot a data error or a rule conflict, name it plainly and propose the fix rather than working around it silently. State assumptions inline. When a decision belongs to the owner, lay out the options and the tradeoffs and stop there.
