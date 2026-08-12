# DARE Section 4 — Publication Extraction Codebook

Version 0.1 (test batch). Source of truth for every counting rule applied in
`data/publications.csv`. Any change to a rule here requires regenerating the
affected faculty rows and noting the change in this file.

## Window

2021–2025 inclusive. Publications count regardless of institutional
affiliation at the time of publication. 2026 is excluded because no CV in the
packet set covers it completely.

## Schema — data/publications.csv

| Field | Values | Definition |
|---|---|---|
| `last`, `first` | text | As on the roster, not as on the CV byline |
| `area` | ENRE / Ag and Food / Ag Ed | From roster |
| `year` | 2021–2025 | Assigned by the rule below |
| `title_short` | text | Trimmed to ~60 chars, no trailing punctuation |
| `venue` | text | Journal, press, or issuing body as printed on the CV |
| `type` | JA/BC/BK/RP/CP | See type rules |
| `index_class` | a / b / *(blank)* | Journal articles only. See below. |
| `extension_output` | 1 / 0 | 1 = extension or non-refereed report; excluded from research counts |
| `student_coauthor` | Y/N/U | See student rules |
| `doi` | bare DOI, no URL prefix | Blank if the CV gives none |
| `source_file` | filename | The `txt/` file the row came from |
| `source_page` | integer | Page marker in that file |
| `flag` | text | Non-empty means a human decision is pending |

## Index class (journal articles only)

Per Lauren's rule, output counts if it is either **(a) indexed and
peer-reviewed** or **(b) peer-reviewed but not indexed**. Indexing is proxied
by whether the journal carries an impact factor.

- `index_class = a` — journal carries an impact factor (JIF or CiteScore).
- `index_class = b` — peer-reviewed journal with no impact factor
  (e.g. Choices, Rangelands, Western Economics Forum).
- **blank** — impact-factor status not yet verified. Left blank on purpose so
  Lauren can look it up against JCR/Scopus. A blank is a to-do, not a zero.

`index_class` applies only to `type = JA`. Books, chapters, reports, and
proceedings are never impact-factored and are reported as their own categories.

## extension_output flag

`extension_output = 1` marks any item that is neither indexed nor
peer-reviewed in the research sense: extension newsletters, fact sheets, market
reports, and commissioned/technical reports (World Bank, USGS, Farm Bureau,
etc.). These are **excluded from every research productivity count** but kept
in the file so they can be tabulated separately and used in the narrative to
explain heavy-extension appointments (e.g. Koontz). Everything else carries
`extension_output = 0`.

Note: this bucket mixes true extension pieces with commissioned technical
reports. If those need to be separated later, split on `type` (EX vs RP)
rather than re-reading the CVs.

## Year assignment

1. Use the year printed in the citation.
2. "Forthcoming", "accepted", "in press" with **no** year: **exclude**, and log
   to `data/excluded.csv` with reason `no_year_forthcoming`. These are not lost
   — they are recoverable once the CV or Crossref supplies a year.
3. "Forthcoming" **with** a stated acceptance year: count in that year, flag
   `forthcoming_dated`.
4. Reports and non-journal items dated by month: use the stated year.
5. Where a CV gives both an online-first year and an issue year, use the issue
   year, flag `year_ambiguous`.

## Type

| Code | Includes |
|---|---|
| JA | Peer-reviewed journal article |
| BC | Chapter in an edited volume |
| BK | Authored or edited book |
| RP | Research report, working paper, commissioned report |
| CP | Refereed conference proceeding or transaction |
| EX | Extension publication, newsletter, fact sheet, trade press |

**JA and EX are never pooled.** Several DARE faculty carry majority extension
appointments and their EX output is large; folding it into a peer-reviewed
count would misrepresent the department to a reviewer checking against
Academic Analytics, which does not index extension outputs. Report EX in its
own table.

Items listed on a CV as "under review", "in preparation", "submitted", or
"R&R" are **not output** and are excluded entirely.

## Student co-authorship

- `Y` where the CV explicitly marks the co-author as a graduate student
  (asterisk convention, or a "* indicates graduate student" legend).
- `Y` where the co-author appears on the same CV's graduate advising list with
  a completion year at or after the publication year.
- `N` where the co-author appears on the advising list with a completion year
  **before** the publication year. These are alumni co-authorships. They are
  real and worth reporting, but they are not the "engaging students in inquiry"
  claim Section 4 asks for. Flagged `alumni_coauthor` so they can be recovered
  as a separate count.
- `U` where the CV gives no basis to decide. Not guessed.

## Cross-population of co-authored papers

When any DARE faculty member lists a paper co-authored with other DARE
faculty, every DARE co-author receives a row for that paper — even if their own
CV omits it. CVs are updated at different times and people forget entries, so a
paper present on one CV is authoritative for all its DARE authors.

Mechanism: each row records `dare_coauthors` (semicolon-separated last names of
DARE faculty on the paper). After all CVs are read, papers collapse to a
distinct record (DOI where present, else normalized title+year). For each
distinct record, one row is emitted per DARE author. Rows created for an author
whose own CV lacked the paper are flagged `added_from_coauthor` and keep the
`source_file` of the CV where the paper was actually found, preserving
traceability.

Consequence, stated so it is not mistaken for an error: the sum of per-faculty
counts exceeds the department-level distinct-paper count by exactly the volume
of within-department collaboration. That difference is the item 6 evidence base,
not double counting — the department total dedupes on the distinct record.

## Known limitation

Step 1 (unpack) and step 3 (statistics) are scripts. Step 2 — CV to structured
rows — is a documented read, not a regex, because the CVs share no common
section headings, citation format, or date placement. Every row carries
`source_file` and `source_page` so any number in Section 4 can be traced back
to a page of a CV.
