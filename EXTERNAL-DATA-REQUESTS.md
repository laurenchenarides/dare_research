# External data requests for research and creative artistry tables

## Academic Analytics request: Tables B1, B2, and peer fields in D4

Please provide an export for Colorado State University’s Department of Agricultural and Resource Economics and the agreed set of comparable R1 departments. Use the same comparison year, faculty inclusion rules, discipline/taxonomy, and observation window for every institution. Please include the metric definitions and indicate whether departed faculty, non-tenure-track faculty, joint appointments, and zero-research appointments are included.

For **Table B1**, we need one row per institution with:

- Institution
- Exact comparison unit
- Departmental Scholarly Research Index (SRI)
- SRI percentile or rank and the comparison population used
- Number of faculty in the SRI denominator
- SRI per faculty, if supplied or permitted to calculate
- Comparison year
- Notes on comparability, including taxonomy or unit-boundary differences

For **Table B2**, please provide the CSU value and the full peer distribution—or at minimum peer median, peer maximum, and CSU percentile—for:

- Journal articles
- Citations
- Citations per publication
- Grants
- Grant dollars
- Books
- Conference proceedings
- Awards and honors
- Cross-institutional collaboration

For every indicator, please include the exact definition, source database, publication or grant window, citation snapshot date, whether values are totals or per-faculty rates, and the faculty denominator.

For the peer-comparison fields in **Table D4**, please provide peer median, peer 75th percentile, and CSU percentile for total citations, citations per publication, percentage of publications cited, field-weighted citation impact, and highly cited publications. If Google Scholar values are available, they should be supplied separately and not substituted for curated publication counts.

The empty layouts are in:

- `output/table_B1_AA_SRI_template.csv`
- `output/table_B2_AA_productivity_radar_template.csv`
- `output/table_D4_impact_indicators.csv` (existing CSU/OpenAlex values with peer fields blank)

## CSU-unit publication collaboration request: Table E2

Please provide publication-level collaboration data identifying the CSU college, department, center, or institute associated with each CSU coauthor on DARE publications during 2021–2026. Required fields are publication identifier, publication year, DARE faculty author, CSU collaborator, collaborator’s CSU unit, and research area or subject tags. OpenAlex identifies Colorado State University but does not reliably resolve internal CSU units.

## USDA Multistate Research Projects request: Table E4

Please provide all USDA Multistate Research Projects involving DARE faculty during 2021–2026, including project code, title/focus, participation dates, DARE faculty, formal leadership roles, participating institutions, and major outputs. The current grants and publication files do not contain a reliable project-membership or leadership field.

The requested layout is `output/table_E4_multistate_projects_template.csv`.

## Faculty mentoring and advancement supports request: Table F2

Please provide the department’s formal and informal faculty-development mechanisms, including career stage, mechanism, frequency, expected participants, actual participants if appropriate for internal use, and intended outcome. Requested mechanisms include onboarding, mentor committees, annual promotion-and-tenure committee review, annual department-head meetings, and optional mentor committees for associate professors.

The graduate committee workbook measures faculty service on student committees; it does not document faculty mentoring and advancement supports. Its results are therefore reported separately in `output/table_F2_grad_committee_mentoring_activity.csv`.

The requested F2 layout is `output/table_F2_faculty_mentoring_supports_template.csv`.

## Student-engagement gaps: Tables G1 and G2

Please provide annual 2021–2026 records for undergraduate research participants, graduate research assistants, student conference presentations, community-engaged research projects involving students, and other student scholarly products. For output-level records, please include undergraduate/graduate status, research area, venue, peer-review or jury status, external collaborator, and any resulting placement, award, or other outcome.

Current sources support student-coauthored publications and student awards, but they do not consistently identify student level or the remaining requested measures.
