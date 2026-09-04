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

The working layouts are in:

- `output/table_B1_AA_SRI_template.csv`
- `output/table_B2_AA_productivity_radar_template.csv`
- `output/table_D4_impact_indicators.csv` (existing CSU Google Scholar values with peer fields blank)

## Sponsored-project outcomes, expenditures, and student support request: Tables C4–C6

Please provide sponsored-project data needed to distinguish proposals, awards, and expenditures for 2021–2026. The current proposal and award records do not contain final outcomes for all proposals, requested dollars, award dates, or fiscal-year expenditures.

For **Table C4**, please provide project identifier, submission date, final proposal outcome, dollars requested, award date, and dollars awarded. Please distinguish proposals still pending from proposals not funded and identify supplements, continuations, and resubmissions so they are not treated as independent outcomes without review.

For **Table C5**, please provide fiscal-year expenditures by project, including total sponsored expenditures, direct costs, applicable F&A base, F&A recovered, graduate research assistant wages, graduate tuition or fee support, and project start and end dates. These fields will allow the department to report how sponsored funding supports research activity and graduate students rather than treating award amounts as expenditures.

For **Table C6**, please provide the same fiscal-year expenditure measures for each CAS academic department, together with annual active tenure-track faculty counts and, if available, research FTE. Please identify major laboratory, experimental-facility, field-station, and equipment costs or provide a consistent research-infrastructure category to support appropriate interpretation across departments.

For the F&A analysis, please also provide the negotiated or sponsor-limited rate, the applicable base (such as modified total direct costs, total direct costs, or salaries and wages), actual F&A recovered, direct sponsor, prime sponsor, and pass-through terms. A stated F&A rate alone cannot determine the share of total award dollars available for direct project activities.

## CSU-unit publication collaboration request: Table E2

Please provide publication-level collaboration data identifying the CSU college, department, center, or institute associated with each CSU coauthor on DARE publications during 2021–2026. Required fields are publication identifier, publication year, DARE faculty author, CSU collaborator, collaborator’s CSU unit, and research area or subject tags. OpenAlex identifies Colorado State University but does not reliably resolve internal CSU units.

## USDA Multistate Research Projects request: Table E4

Please provide all USDA Multistate Research Projects involving DARE faculty during 2021–2026, including project code, title/focus, participation dates, DARE faculty, formal leadership roles, participating institutions, and major outputs. The current grants and publication files do not contain a reliable project-membership or leadership field.

The requested layout is `output/table_E4_multistate_projects_template.csv`.

## Faculty mentoring and advancement supports request: Table F2

Please provide the department’s formal and informal faculty-development mechanisms, including career stage, mechanism, frequency, expected participants, actual participants if appropriate for internal use, and intended outcome. Requested mechanisms include onboarding, mentor committees, annual promotion-and-tenure committee review, annual department-head meetings, and optional mentor committees for associate professors.

The graduate committee workbook measures faculty service on student committees; it does not document faculty mentoring and advancement supports. Its results are therefore reported separately in `output/table_F2_grad_committee_mentoring_activity.csv`.

The requested F2 layout is `output/table_F2_faculty_mentoring_supports_template.csv`.

## Scholarly service and disciplinary leadership request

Please provide or verify faculty scholarly-service roles held during 2021–2026. Include journal editor, co-editor, associate editor, and editorial-board roles; elected offices and committee leadership in disciplinary associations; elected fellow status; major grant or journal review panels; leadership in USDA Multistate Research Projects; and other national or international disciplinary service.

For each role, please provide faculty member, organization or journal, role title, start and end year, whether the role was elected, appointed, or competitively selected, and whether it is still active. A targeted CV extraction can create the initial list, but faculty should verify it because CV formatting and update dates vary.

## Student-engagement gaps: Tables G1 and G2

Please provide annual 2021–2026 records for undergraduate research participants, graduate research assistants, student conference presentations, community-engaged research projects involving students, and other student scholarly products. For output-level records, please include undergraduate/graduate status, research area, venue, peer-review or jury status, external collaborator, and any resulting placement, award, or other outcome.

Current sources support student-coauthored publications and student awards, but they do not consistently identify student level or the remaining requested measures.
