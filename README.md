# README.md — DARE Section 4 Data Pipeline

Instructions for AI agent working in this repository. Read this before running anything.

## What this project is

This repo builds the quantitative evidence base for **Section 4 (Research and Creative Artistry)** of the Department of Agricultural and Resource Economics (DARE) 2026 academic program review at Colorado State University. The output is a set of reproducible CSVs and an R analysis pipeline that feed a written narrative submitted to the provost's office.

The owner is Lauren Chenarides (faculty, DARE). The review covers a **2021–2025** publication and grant window. Some 2026 items are collected but held out of reporting and flagged `edge_2026`.

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
  publications_faculty_doi_cleaned.csv   faculty-DOI level publication list
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
2. **`code/02_openalex_enrich.R`** — Queries OpenAlex by DOI for authorship, affiliations, and citation counts; matches coauthors to the conferred-degree roster; maps impact factors from the wide lookup; joins appointment splits; builds the faculty-year panel and department summary. Expects inputs in `data/`, writes to `output/`. Caches the raw API response to `output/oa_works_raw.rds` — set `REFRESH_OPENALEX <- TRUE` to re-query.

The pipeline is numbered. Run stages in order. Later stages read earlier outputs.

## Counting rules (summary — CODEBOOK.md is authoritative)

- **Window:** 2021–2025. Publications count regardless of the author's institution at the time. 2026 items are held out, flagged `edge_2026`.
- **Publication types:** JA, BC (book chapter), BK (book), RP (research report), CP (conference proceeding). `extension_output = 1` marks extension and non-refereed reports, excluded from research counts.
- **Index class:** `a` = carries an impact factor; `b` = peer-reviewed, no impact factor. Blank = not yet verified against JCR/Scopus. A blank is a to-do, not a zero.
- **Student coauthor (union rule):** a CV-coded `Y` is authoritative and is never downgraded; a registrar match adds `Y` where the CV did not claim one. `student_evidence` records which source supports each `Y`. A CV-only `Y` that the registrar can't corroborate is expected — the registrar file covers only AREC graduates, not undergraduates, other departments, or other institutions.
- **Cross-population (fallback only):** a co-authored paper is added to a DARE co-author's list only if it is absent from their own CV. If already present, their own entry stands — no propagated duplicate. Per-faculty counts therefore exceed the department distinct-paper count by the volume of internal collaboration; that gap is the interdisciplinary-collaboration evidence, not double counting. Department totals dedupe on DOI (title+year where no DOI).
- **Grants:** include any award active during 2021–2025 regardless of start year (pre-2021 starts flagged `active_prewindow`). Keep funded, submitted, under review, and pending; exclude only not-funded/declined. Dollar amounts are provisional pending VPR corroboration; `dollar_basis` records total vs CSU-share. On shared grants keep the PI's figure and flag disagreements. Award amounts were retrieved from the [OVPR](https://vprweb.research.colostate.edu/Proposal-Award-History-Search/Proposal.aspx) where `Date Submitted Between: 01/01/2021 and 08/01/2026` and `Lead Unit = Agricultural + Resource Economics (1172)`.
- **Presentations:** one row per faculty-year-talk. Each venue is its own row (a paper presented at three venues is three rows). Typed conference / invited / other; posters flagged.
- **Appointment splits** are held constant across the window. Chouinard is 5% research. Weighting uses the research share itself, so splits that don't total 100 (Thilmany, Bennett) do not distort research FTE.
- **Departed/retired faculty scope** (Hill, Manning, Jablonski, and Perry who left before the window). Count them for the years they were active, consistent with partial-window treatment of faculty hired after 2021.

## Open decisions

- **John Ritten: Continuing vs TT.** He is the highest-output faculty member by a wide margin, so his classification materially moves both numerator and denominator. The appointment file lists him as Continuing; the original instruction was to report on TT faculty. Keep all three denominators so the effect is visible; flag, don't choose.
- **AA alignment.** If Academic Analytics excludes departed faculty, our counts must match or the discrepancy must be stated explicitly.

## Known data items needing human correction

Flagged by the pipeline to `output/doi_integrity_audit.csv`, not auto-fixed:

- `10.1371/journal.pone.0261833` — a PLOS ONE DOI correctly on Suter's "Summer Crowds"; Burkhardt's lettuce-production paper carries it incorrectly.
- `10.1080/10871209.2024.2414880` — appears twice on Hoag's rows with different titles/years; likely one paper entered twice.

Manual-verification priorities: Hoag (CV gives career aggregates, not dated titles), Magnan (grants didn't extract cleanly), Koontz (extension volume), Thilmany and Seidl (grant dollars via VPR).

## Writing (only when asked to draft narrative)

Follow `DEPT-VOICE.md`. In short: positive declarative statements over contrast constructions; specific numbers over vague quantifiers (replace "numerous" with a count); no em dashes; no evaluative flourishes; no redundancy. Every claim in the narrative names specific faculty and papers before it is written. Vague or overstated claims will not survive reviewer scrutiny.

## Working style

Direct, specific, and willing to push back. When you spot a data error or a rule conflict, name it plainly and propose the fix rather than working around it silently. State assumptions inline. When a decision belongs to the owner, lay out the options and the tradeoffs and stop there.