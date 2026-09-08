# DARE Section 4 — Sponsored Projects Codebook

Source of truth for sponsored-project rules applied by `code/05_grant_tables.R` to `data/grants_cas.xlsx`.

## Scope and source grain

- Reporting window: 2021–2026 based on `Date Sent`; 2026 is partial through July 21 in the current export.
- The current workbook contains 1,500 unique project/proposal records. Any future formula or nonproject row with a missing `Key ID` is excluded to prevent double-counting.
- `Key ID`, `IP Nbr`, and `PD Nbr` are unique across the 1,500 project rows.
- DARE-led records are identified by `Lead Unit = 1172-Agricultural + Resource Economics`.
- A project is counted once in its `Date Sent` year. A full multi-year award amount is not repeated in later years.

## Status and amount

- `Status = Funded` identifies an award.
- Blank status means **not funded or still in progress** and is not counted as an award. The source does not distinguish those two outcomes.
- `Amount*` is treated as awarded dollars only for `Funded` rows.
- On blank-status rows, `Amount*` is retained but not reported as awarded dollars. Its exact meaning must be confirmed before proposal-dollar or dollar-success-rate reporting.
- Awards and award dollars are grouped by proposal-submission cohort (`Date Sent` year), not award date, because the workbook contains no award date.
- Award dollars are not expenditures. The workbook contains no expenditure transactions or fiscal-year expenditure totals.

### Reporting-window reconciliation

The proposal-submission rule produces 85 funded DARE-led records totaling $63,423,126.10. Filtering those funded records by `Start Date` instead produces 83 records totaling $63,343,445.96. The $79,680.14 difference consists of two records submitted during the reporting window whose project start dates are in 2020: `K000148564` ($75,080.14) and `K000154606` ($4,600.00). Tables C1-C3 and the CAS comparisons retain the `Date Sent` rule so funded and unfunded proposals remain in the same submission cohorts. The two definitions are reproduced in `output/grant_total_reconciliation.csv`, and the differing records are listed in `output/grant_reporting_window_difference.csv`.

## Investigators and faculty participation

`Investigators` is parsed at semicolons into one row per person-project-role-unit combination. Expected entries have:

- name;
- role: `Primary PI`, `Co-PI`, or `Key Person`;
- unit code.

DARE faculty participation uses investigator unit `1172` across all CAS-led projects. This retains DARE faculty involvement when another CAS department is the lead unit. Investigator names are matched to `roster.csv` using normalized last name plus first initial, which is unique in the current roster. The approved alias `Jablonski,Rebecca BR` → `Jablonski,Becca` is applied explicitly.

Only rostered faculty active in the relevant year enter the annual PI/co-PI faculty numerator. `Key Person` does not count as PI or co-PI.

Nineteen truncated or incomplete investigator fragments are retained in `output/grant_investigator_parse_audit.csv` and excluded from person/role/unit counts. Two occur on DARE-led project strings; usable entries earlier in those same strings remain included.

## Funding-source classification

The originating source is the `Prime Sponsor` and `Prime Sponsor Type` whenever present; otherwise it is the direct sponsor/type. In particular, when another university is the direct sponsor and a prime sponsor is supplied, tables report the prime sponsor because the university is acting as a pass-through on a likely subaward. Colorado State University itself is excluded from the pass-through-institution summaries. The direct university is retained in the audit and Table C3 pass-through field. Table C2 maps originating sources as follows:

- **Federal:** `Federal`.
- **State:** State of Colorado, other domestic state government, and local-government types.
- **CSU/Internal:** `Proposal Type = Internal RFP` or originating sponsor name identifies Colorado State University.
- **Foundation/nonprofit:** foundation or nonprofit types.
- **Industry:** commercial types.
- **Other:** higher education, foreign government, and all remaining types.

Higher-education pass-through awards with a federal prime sponsor are Federal, not internal.

## F&A analysis

F&A analysis groups funded awards by direct sponsor because the direct sponsor issued the award to CSU. It reports F&A rate together with `F&A Type`; rates based on TDC, MTDC, salaries and wages, or no-indirect-cost rules are not assumed to be directly comparable.

The F&A distribution assigns higher-education direct sponsors with a populated prime sponsor to a separate `Pass-through institution` category. Remaining records use the broad direct-sponsor categories Federal, State, CSU/Internal, Foundation/nonprofit, Industry, or Other. The plot is faceted into DARE-led awards and awards led by the other CAS academic departments. The Dean's office is excluded from this department comparison. The plot labels each displayed category with the number of funded awards having a reported F&A rate and omits the heterogeneous `Other` category from the visual while reporting its excluded count in the caption. The companion row-level CSV retains all included categories, including `Other`, together with the direct sponsor, originating sponsor, and F&A basis.

`grant_fa_comparison_summary.csv` provides reproducible DARE-versus-other-CAS comparisons overall and by direct-sponsor category. It reports award count, mean, median, minimum and maximum F&A rate, zero-rate count, and the number and share at or below the 10 percent screening threshold. These are comparisons of stated rates, not comparisons of the share of total award dollars devoted to F&A, because the applicable bases differ.

The low-F&A audit uses a transparent threshold of 10 percent or less and separately identifies zero-rate awards. This is a screening tool, not a claim that a sponsor always imposes a low rate.

## Output definitions

- **Table C1:** DARE-led proposal and funded-award counts/dollars by `Date Sent` year. Faculty PI/co-PI counts cover rostered DARE faculty participating anywhere in the CAS project file.
- **Table C2:** DARE-led funded awards by originating funding-source category and `Date Sent` year.
- **Table C3:** DARE-led funded awards by originating sponsor, with `direct_pass_through_sponsors`, DARE faculty involvement, derived research-area coverage, and years represented.
- **CAS appendix:** proposal and award comparisons by CAS lead unit. The department summary includes recorded funded share and ranks for proposals, funded awards, award dollars, average award amount, and funded share among the five academic departments represented in the extract. The Dean's unit is retained but excluded from academic-department ranks. These are not expenditure or per-capita comparisons.
- **Sponsor concentration:** shares of DARE award dollars and HHI across originating sponsors.
- **F&A appendix:** direct-sponsor award volume, rates, bases, DARE involvement, and DARE-unique sponsors.
- **Cross-unit appendix:** non-1172 investigators on DARE-led projects. Award dollars can appear under more than one collaborating unit and must not be summed across units.
- **Reverse cross-unit appendix:** rostered DARE faculty serving as PI or co-PI on projects led by another CAS department, supplied both as unique project records and a lead-department summary. Each project and its award dollars count once even if multiple DARE faculty participated.
- **F&A distribution:** row-level plot data and a violin/box/point plot of funded-award F&A rates by direct-sponsor category, with pass-through institutions separated and DARE-led awards shown apart from other CAS-led awards.
- **F&A comparison summary:** overall and sponsor-category rate comparisons between DARE-led and other-CAS-led funded awards.
- **Pass-through institution summary:** proposal and funded-award activity by direct higher-education institution, including originating-sponsor diversity, federal funding share, years represented, and DARE-specific counts and dollars.
- **Pass-through institution by year:** the same activity by direct institution and `Date Sent` year. The observed funded share is descriptive only because blank statuses combine not-funded and in-progress proposals.
- **Grant total reconciliation:** DARE funded-award count and dollars under both the report's `Date Sent` rule and a comparison based on `Start Date`.
- **Reporting-window difference:** row-level records included under one of those date definitions but not the other.

## Tables not supported by this workbook

- **Table C4, proposal success:** blank outcomes combine not-funded and in-progress proposals, and requested dollars for funded proposals are not separately available.
- **Table C5, DARE expenditures:** fiscal-year expenditure data are absent.
- **Table C6, CAS expenditure comparison:** expenditures and other departments’ faculty denominators are absent.
- **Active sponsored projects:** 72 `End Date` values have an apparent 1900/2000 century error. They remain flagged and are not silently corrected.

These tables require additional source data rather than assumptions.
