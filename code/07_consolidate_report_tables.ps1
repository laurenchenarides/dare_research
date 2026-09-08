param(
    [string]$ProjectRoot = (Split-Path -Parent (Split-Path -Parent $PSScriptRoot)),
    [string]$OutputFile = "output/SECTION-4-REPORT-TABLES.xlsx"
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path -LiteralPath (Join-Path $ProjectRoot "SECTION-4-RESEARCH-AND-CREATIVE-ARTISTRY.md"))) {
    $ProjectRoot = Split-Path -Parent $PSScriptRoot
}

$outputPath = [System.IO.Path]::GetFullPath((Join-Path $ProjectRoot $OutputFile))
$outputDir = Split-Path -Parent $outputPath
$tempPath = Join-Path ([System.IO.Path]::GetTempPath()) (
    "SECTION-4-REPORT-TABLES-" + [guid]::NewGuid().ToString("N") + ".xlsx"
)
[System.IO.Directory]::CreateDirectory($outputDir) | Out-Null

$colors = @{
    Green       = 0x2B4D1E  # #1E4D2B in Excel BGR order
    GreenLight  = 0xE8F1E6
    Gold        = 0x1492C6  # #C69214 in Excel BGR order
    GoldLight   = 0xEAF6FF
    Gray        = 0xF2F2F2
    GrayDark    = 0x666666
    Border      = 0xD9D9D9
    White       = 0xFFFFFF
    Amber       = 0xD9EAFB
    BlueLight   = 0xF4EDE3
}

function Convert-HeaderLabel {
    param([string]$Header)
    $special = @{
        "sri" = "SRI"
        "csu" = "CSU"
        "tt" = "TT"
        "pi" = "PI"
        "copi" = "Co-PI"
        "fa" = "F&A"
        "fte" = "FTE"
        "arec" = "AREC"
    }
    $parts = $Header -split "_"
    $labels = foreach ($part in $parts) {
        $lower = $part.ToLowerInvariant()
        if ($special.ContainsKey($lower)) { $special[$lower] }
        elseif ($lower -eq "or") { "or" }
        elseif ($lower -eq "of") { "of" }
        elseif ($lower -eq "and") { "and" }
        else { (Get-Culture).TextInfo.ToTitleCase($lower) }
    }
    return ($labels -join " ")
}

function Convert-TypedValue {
    param([string]$Header, $Value)
    if ($null -eq $Value) { return $null }
    $text = ([string]$Value).Trim()
    if ($text -eq "") { return $null }
    if ($text -in @("NA", "N/A")) { return $null }
    if ($text -eq "TRUE") { return $true }
    if ($text -eq "FALSE") { return $false }

    $number = 0.0
    if ([double]::TryParse($text, [Globalization.NumberStyles]::Any,
            [Globalization.CultureInfo]::InvariantCulture, [ref]$number)) {
        if ($Header -match "(?i)(^|_)(share|percentage|percent|rate)($|_)") {
            if ([math]::Abs($number) -gt 1) { return $number / 100.0 }
            return $number
        }
        if ($Header -match "(?i)(year|code)$" -and $text -match "^\d+$") {
            return $text
        }
        return $number
    }
    return $text
}

function Read-CsvTable {
    param(
        [string]$RelativePath,
        [string[]]$Columns = @(),
        [hashtable]$Labels = @{},
        [int]$Take = 0,
        [scriptblock]$Filter = $null
    )
    $path = Join-Path $ProjectRoot $RelativePath
    if (-not (Test-Path -LiteralPath $path)) {
        throw "Required source file not found: $RelativePath"
    }
    $rows = @(Import-Csv -LiteralPath $path -Encoding UTF8)
    if ($Filter) { $rows = @($rows | Where-Object $Filter) }
    if ($Take -gt 0) { $rows = @($rows | Select-Object -First $Take) }

    if ($Columns.Count -eq 0) {
        if ($rows.Count -gt 0) { $Columns = @($rows[0].PSObject.Properties.Name) }
        else {
            $headerLine = Get-Content -LiteralPath $path -Encoding UTF8 -First 1
            $Columns = @($headerLine -split ",")
        }
    }

    $headers = foreach ($column in $Columns) {
        if ($Labels.ContainsKey($column)) { $Labels[$column] } else { Convert-HeaderLabel $column }
    }
    $data = foreach ($row in $rows) {
        $record = foreach ($column in $Columns) { Convert-TypedValue $column $row.$column }
        ,@($record)
    }
    return @{ Headers = @($headers); Data = @($data); Source = $RelativePath }
}

function Read-MarkdownTable {
    param([string]$HeadingText)
    $path = Join-Path $ProjectRoot "SECTION-4-RESEARCH-AND-CREATIVE-ARTISTRY.md"
    $lines = [System.IO.File]::ReadAllLines($path, [Text.Encoding]::UTF8)
    $headingIndex = -1
    for ($i = 0; $i -lt $lines.Length; $i++) {
        if ($lines[$i] -like "*$HeadingText*") { $headingIndex = $i; break }
    }
    if ($headingIndex -lt 0) { throw "Markdown heading not found: $HeadingText" }

    $tableLines = New-Object System.Collections.Generic.List[string]
    for ($i = $headingIndex + 1; $i -lt $lines.Length; $i++) {
        $line = $lines[$i].Trim()
        if ($line.StartsWith("|")) { $tableLines.Add($line) }
        elseif ($tableLines.Count -gt 0) { break }
    }
    if ($tableLines.Count -lt 2) { throw "Markdown table not found after: $HeadingText" }

    function Split-MarkdownRow([string]$line) {
        $cells = $line.Trim().Trim("|") -split "\|"
        return @($cells | ForEach-Object {
            $_.Trim() -replace "\*\*", ""
        })
    }

    $headers = Split-MarkdownRow $tableLines[0]
    $data = @()
    for ($i = 2; $i -lt $tableLines.Count; $i++) {
        $cells = Split-MarkdownRow $tableLines[$i]
        $typed = for ($j = 0; $j -lt $headers.Count; $j++) {
            $value = if ($j -lt $cells.Count) { $cells[$j] } else { "" }
            Convert-TypedValue $headers[$j] ($value -replace "^\$", "" -replace ",", "" -replace "%$", "")
        }
        $data += ,@($typed)
    }
    return @{ Headers = @($headers); Data = @($data); Source = "SECTION-4-RESEARCH-AND-CREATIVE-ARTISTRY.md" }
}

function New-PlaceholderTable {
    param([string[]]$Headers)
    return @{ Headers = $Headers; Data = @(); Source = "Awaiting external data" }
}

$tables = New-Object System.Collections.Generic.List[hashtable]

function Add-TableDefinition {
    param(
        [string]$Sheet,
        [string]$TableId,
        [string]$Section,
        [string]$Title,
        [string]$Status,
        [string]$Note,
        [hashtable]$Table,
        [hashtable]$ColumnFormats = @{},
        [string]$HighlightRowLabel = ""
    )
    $tables.Add(@{
        Sheet = $Sheet; TableId = $TableId; Section = $Section; Title = $Title;
        Status = $Status; Note = $Note; Headers = $Table.Headers; Data = $Table.Data;
        Source = $Table.Source; ColumnFormats = $ColumnFormats; HighlightRowLabel = $HighlightRowLabel
    })
}

$t41 = Read-CsvTable "output/table_D1_publications_by_year.csv" `
    @("year", "active_tt_faculty", "unique_department_publications", "faculty_publication_count") `
    @{ year = "Year"; active_tt_faculty = "Active TT faculty"; unique_department_publications = "Unique department publications"; faculty_publication_count = "Faculty-publication credits" }
$t41.Data = @($t41.Data) + ,@("Total", $null, 264.0, 344.0)
Add-TableDefinition "4.1 Publications" "Table 4.1" "Main text" `
    "Peer-reviewed journal publication productivity, 2021-2026" "Complete" `
    "2026 is partial. A faculty-publication credit counts one publication for each participating DARE faculty member." $t41 `
    @{} "Total"

$t42 = Read-CsvTable "output/table_D4b_citations_by_publication_year.csv" `
    @("year", "peer_reviewed_journal_publications", "total_google_scholar_citations", "mean_google_scholar_citations_per_publication", "median_google_scholar_citations_per_publication", "percentage_of_publications_cited_in_google_scholar", "publication_weighted_average_journal_impact_factor") `
    @{ year = "Publication year"; peer_reviewed_journal_publications = "Publications"; total_google_scholar_citations = "Google Scholar citations"; mean_google_scholar_citations_per_publication = "Mean citations per publication"; median_google_scholar_citations_per_publication = "Median citations per publication"; percentage_of_publications_cited_in_google_scholar = "Percentage cited"; publication_weighted_average_journal_impact_factor = "Publication-weighted average journal IF" }
Add-TableDefinition "4.2 Citations" "Table 4.2" "Main text" `
    "Citation indicators and journal-portfolio benchmark by publication year, 2021-2026" "Complete" `
    "Google Scholar citations are cumulative; 2026 is partial. Journal impact factors are publication-weighted within each cohort." $t42 `
    @{} "Total"

$ifPath = Join-Path $ProjectRoot "output/table_D4a_impact_factor_summary.csv"
$ifRecord = @(Import-Csv -LiteralPath $ifPath -Encoding UTF8)[0]
$ifColumns = @(
    "peer_reviewed_journal_publications",
    "publications_with_available_impact_factor",
    "mean_impact_factor",
    "median_impact_factor",
    "minimum_impact_factor",
    "maximum_impact_factor"
)
$ifLabels = @("Publications in the journal-impact analysis", "Publications with an available impact factor", "Mean impact factor", "Median impact factor", "Minimum impact factor", "Maximum impact factor")
$ifData = for ($i = 0; $i -lt $ifColumns.Count; $i++) {
    ,@($ifLabels[$i], (Convert-TypedValue $ifColumns[$i] $ifRecord.($ifColumns[$i])))
}
$t43 = @{ Headers = @("Impact-factor indicator", "Department value"); Data = @($ifData); Source = "output/table_D4a_impact_factor_summary.csv" }
Add-TableDefinition "4.3 IF Summary" "Table 4.3" "Main text" `
    "Journal impact-factor coverage and distribution for peer-reviewed publications" "Complete" `
    "Impact-factor statistics describe journal placement; they are not a departmental impact factor." $t43

$t44 = Read-CsvTable "output/table_D4c_journals_by_publication_count.csv" `
    @("journal", "publication_count", "average_impact_factor", "total_google_scholar_citations", "average_google_scholar_citations_per_publication") `
    @{ journal = "Journal"; publication_count = "Publications"; average_impact_factor = "Average impact factor"; total_google_scholar_citations = "Total Google Scholar citations"; average_google_scholar_citations_per_publication = "Average Google Scholar citations per publication" } 10
foreach ($row in $t44.Data) { if ($null -eq $row[2]) { $row[2] = "Not available" } }
Add-TableDefinition "4.4 Top Journals" "Table 4.4" "Main text" `
    "Journals with the largest numbers of DARE publications" "Complete" `
    "Journals are ranked by publication count. Citation totals are manually verified Google Scholar counts." $t44

Add-TableDefinition "A1 Faculty Snapshot" "Table A1" "Appendix A" "Faculty Data Snapshot" "Partial" `
    "Research-area classifications remain to be confirmed for four faculty members." `
    (Read-CsvTable "output/table_A1_faculty_data_snapshot.csv")

Add-TableDefinition "B1 SRI Template" "Table B1" "Appendix B" "Departmental Scholarly Research Index compared with selected R1 peers" "Awaiting data" `
    "Populate with a consistent comparison year, faculty definition, unit taxonomy, and metric window for DARE and all peers." `
    (Read-CsvTable "output/table_B1_AA_SRI_template.csv")

Add-TableDefinition "B2 Radar Template" "Table B2" "Appendix B" "Academic Analytics Productivity Radar indicators compared with R1 peers" "Awaiting data" `
    "Department values and aligned peer statistics are awaiting Academic Analytics data." `
    (Read-CsvTable "output/table_B2_AA_productivity_radar_template.csv")

$c1 = Read-CsvTable "output/table_C1_sponsored_projects_by_year.csv" `
    @("year", "proposals_submitted", "awards_received", "total_awarded_dollars", "average_award_amount") `
    @{ year = "Year"; proposals_submitted = "Proposals submitted"; awards_received = "Funded award count"; total_awarded_dollars = "Total award dollars"; average_award_amount = "Average award amount" }
$c1SourceRows = @(Import-Csv -LiteralPath (Join-Path $ProjectRoot "output/table_C1_sponsored_projects_by_year.csv") -Encoding UTF8)
$c1ProposalTotal = ($c1SourceRows | Measure-Object -Property proposals_submitted -Sum).Sum
$c1AwardTotal = ($c1SourceRows | Measure-Object -Property awards_received -Sum).Sum
$c1DollarTotal = ($c1SourceRows | Measure-Object -Property total_awarded_dollars -Sum).Sum
$c1AverageTotal = if ($c1AwardTotal -gt 0) { $c1DollarTotal / $c1AwardTotal } else { $null }
$c1.Data = @($c1.Data) + ,@("Total", [double]$c1ProposalTotal, [double]$c1AwardTotal, [double]$c1DollarTotal, [double]$c1AverageTotal)
Add-TableDefinition "C1 Projects by Year" "Table C1" "Appendix C" "Sponsored project activity by year, 2021-2026" "Complete" `
    "Projects are assigned to the proposal-submission year; full multi-year awards are not repeated in later years. 2026 is partial." $c1 `
    @{} "Total"

$c1Faculty = Read-CsvTable "output/table_C1_sponsored_projects_by_year.csv" `
    @("year", "active_faculty", "faculty_serving_as_pi", "faculty_serving_as_copi", "faculty_serving_as_pi_or_copi", "share_active_faculty_pi_or_copi") `
    @{ year = "Year"; active_faculty = "Active faculty"; faculty_serving_as_pi = "Faculty serving as PI"; faculty_serving_as_copi = "Faculty serving as Co-PI"; faculty_serving_as_pi_or_copi = "Faculty serving as PI or Co-PI"; share_active_faculty_pi_or_copi = "Share of active faculty serving as PI or Co-PI" }
Add-TableDefinition "C1 Faculty Roles" "Table C1 supplement" "Appendix C" "Faculty participation in sponsored projects by year" "Complete" `
    "This companion panel preserves the faculty-participation measures while keeping the principal Table C1 narrow enough for a portrait page." $c1Faculty

Add-TableDefinition "C2 Awards by Source" "Table C2" "Appendix C" "Sponsored awards by funding-source type and year" "Complete" `
    "The four-column long format fits one page wide. Zero-count source-year combinations are retained for completeness." `
    (Read-CsvTable "output/table_C2_awards_by_source_year_long.csv" `
        @("year", "funding_source_type", "award_count", "award_dollars") `
        @{ year = "Year"; funding_source_type = "Funding source type"; award_count = "Funded award count"; award_dollars = "Award dollars" })

Add-TableDefinition "C3 Awards by Sponsor" "Table C3" "Appendix C" "Sponsored awards by sponsor" "Partial" `
    "Sponsor, award, dollar, faculty-involvement, and year fields are populated. Primary research area remains to be completed." `
    (Read-CsvTable "output/table_C3_awards_by_sponsor.csv")

Add-TableDefinition "C4 Success Template" "Table C4" "Appendix C" "Proposal and award success indicators" "Awaiting data" `
    "Requires resolved proposal outcomes, requested dollars, and compatible proposal-to-award timing." `
    (New-PlaceholderTable @("Year", "Proposals submitted", "Dollars requested", "Awards received", "Dollars awarded", "Proposal success rate", "Dollar success rate"))

Add-TableDefinition "C5 Expenditures" "Table C5" "Appendix C" "Sponsored project expenditures, 2021-2026" "Unavailable" `
    "Fiscal-year expenditure data are not available. Award amounts should not be substituted for expenditures." `
    (New-PlaceholderTable @("Fiscal year", "Total sponsored expenditures", "Expenditures per active TT faculty member", "Graduate research assistant expenditures", "Number of active sponsored projects"))

Add-TableDefinition "C6 CAS Expenditures" "Table C6" "Appendix C" "Sponsored project expenditures across CAS departments" "Unavailable" `
    "Requires comparable CAS department expenditures and active tenure-track faculty denominators." `
    (New-PlaceholderTable @("Department", "Active TT faculty", "Sponsored expenditures", "Expenditures per TT faculty member", "Five-year total or average", "Research infrastructure category"))

Add-TableDefinition "C7 CAS Comparison" "Table C7" "Appendix C" "Proposal and award activity across CAS academic departments, 2021-2026" "Complete" `
    "Results use proposal-submission cohorts. The college dean unit is excluded from the five-department comparison." `
    (Read-CsvTable "output/appendix_CAS_awards_department_summary.csv" `
        @("lead_unit_name", "proposals_submitted_2021_2026", "awards_received_2021_2026", "recorded_funded_share", "total_awarded_dollars_2021_2026", "average_award_amount", "proposal_count_rank_among_cas_departments", "funded_award_count_rank_among_cas_departments", "award_dollars_rank_among_cas_departments", "average_award_rank_among_cas_departments", "funded_share_rank_among_cas_departments") `
        @{ lead_unit_name = "CAS academic department" } 0 { $_.lead_unit_code -ne "1101" }) `
    @{} "Agricultural + Resource Economics"

Add-TableDefinition "C8 Other CAS on DARE" "Supplement C8" "Appendix C" "Other CAS investigators participating on DARE-led projects" "Complete" `
    "Summarizes two-way sponsored-project collaboration using investigator unit codes." `
    (Read-CsvTable "output/appendix_DARE_cross_unit_collaboration.csv")

Add-TableDefinition "C9 DARE on Other CAS" "Supplement C9" "Appendix C" "DARE faculty participating on projects led by other CAS units" "Complete" `
    "Projects are grouped by the other CAS lead unit." `
    (Read-CsvTable "output/appendix_DARE_on_other_lead_units_summary.csv")

Add-TableDefinition "C10 Sponsor Concentration" "Supplement C10" "Appendix C" "DARE sponsor concentration indicators" "Complete" `
    "Concentration measures use originating sponsors for identified pass-through arrangements." `
    (Read-CsvTable "output/grant_sponsor_concentration.csv")

Add-TableDefinition "C11 Pass Through" "Supplement C11" "Appendix C" "Pass-through institution patterns across CAS" "Complete" `
    "Direct higher-education sponsors are retained here to describe pass-through partners; originating sponsors are reported separately." `
    (Read-CsvTable "output/grant_pass_through_institution_summary.csv")

Add-TableDefinition "C12 FA Rate Summary" "Supplement C12" "Appendix C" "F&A rate comparison by sponsor category" "Complete" `
    "Compares DARE-led funded awards with funded awards led by the other CAS academic departments combined." `
    (Read-CsvTable "output/grant_fa_comparison_summary.csv")

Add-TableDefinition "D2 Pubs by Area" "Table D2" "Appendix D" "Peer-reviewed publications by research area" "Partial" `
    "Cross-area publications are counted once in the department total. Unclassified publications remain visible pending faculty-area review." `
    (Read-CsvTable "output/table_D2_publications_by_area.csv")

Add-TableDefinition "D3 Other Outputs" "Table D3" "Appendix D" "Other scholarly outputs by year" "Complete" `
    "Reported separately from peer-reviewed journal publication totals." `
    (Read-CsvTable "output/table_D3_other_scholarly_outputs.csv")

Add-TableDefinition "D4 Impact Indicators" "Table D4" "Appendix D" "Citation and publication-impact indicators" "Partial" `
    "Department Google Scholar values are populated; peer comparison fields remain to be added." `
    (Read-CsvTable "output/table_D4_impact_indicators.csv")

Add-TableDefinition "E1 Within Dept" "Table E1" "Appendix E" "Within-department research collaboration" "Complete" `
    "A publication with two or more participating DARE faculty is counted once as an internally collaborative publication." `
    (Read-CsvTable "output/table_E1_within_department_collaboration.csv")

Add-TableDefinition "E2 CSU Units" "Table E2" "Appendix E" "Collaboration across CSU units" "Partial" `
    "Sponsored-project collaboration is populated. Internal CSU publication collaboration awaits CSU subunit affiliation data." `
    (Read-CsvTable "output/table_E2_collaboration_across_CSU_units.csv")

Add-TableDefinition "E3 Partner Summary" "Table E3 summary" "Appendix E" "External and cross-institutional collaboration summary" "Complete" `
    "Summarizes OpenAlex coauthor-affiliation records by institution type and country." `
    (Read-CsvTable "output/table_E3_publication_partner_summary.csv")

Add-TableDefinition "E3 Partner Detail" "Table E3 detail" "Appendix E" "External and cross-institutional collaboration detail" "Complete" `
    "Publication affiliations are from OpenAlex; funded-project relationships are reported as a separate relationship basis." `
    (Read-CsvTable "output/table_E3_external_collaboration.csv")

Add-TableDefinition "E4 Multistate Template" "Table E4" "Appendix E" "Leadership in USDA Multistate Research Projects" "Awaiting data" `
    "Requires participation and leadership records from faculty or department files." `
    (Read-CsvTable "output/table_E4_multistate_projects_template.csv")

Add-TableDefinition "F0 Award Summary" "Recognition summary" "Appendix F" "Department awards by recipient type and year" "Complete" `
    "Includes all competitive awards recorded for faculty, students, staff, and alumni during 2021-2026." `
    (Read-CsvTable "output/table_F0_department_awards_by_year.csv")

Add-TableDefinition "F1 Awards" "Table F1" "Appendix F" "Faculty research awards, fellowships, and scholarly recognitions" "Partial" `
    "All listed awards were competitive and required an application. Research-area classifications are not available in the source." `
    (Read-CsvTable "output/table_F1_faculty_research_awards.csv")

Add-TableDefinition "F2 Mentoring Supports" "Table F2" "Appendix F" "Faculty research mentoring and advancement supports" "Awaiting verification" `
    "The proposed mechanisms require confirmation of frequency, participants, and intended outcomes." `
    (Read-CsvTable "output/table_F2_faculty_mentoring_supports_template.csv")

Add-TableDefinition "F2 Grad Committees" "Table F2 supplement" "Appendix F" "Graduate committee mentoring activity" "Complete" `
    "Counts are faculty-student committee memberships beginning in each year, based on MEMBER_FROM_YEAR; they are not counts of unique students." `
    (Read-CsvTable "output/table_F2_grad_committee_mentoring_activity.csv")

Add-TableDefinition "G1 Student Engagement" "Table G1" "Appendix G" "Student participation in research and scholarly activity" "Partial" `
    "Student-coauthored outputs and documented awards are populated; other participation measures require additional records." `
    (Read-CsvTable "output/table_G1_student_engagement_partial.csv")

Add-TableDefinition "G2 Student Outputs" "Table G2" "Appendix G" "Student-coauthored scholarly outputs" "Partial" `
    "Student level and resulting outcomes require graduate-program records and faculty verification." `
    (Read-CsvTable "output/table_G2_student_coauthored_outputs.csv")

Add-TableDefinition "H1 Strategic Assessment" "Table H1" "Appendix H" "Summary of research strengths, opportunities, and proposed actions" "Current draft" `
    "This table reflects the current Section 4 draft and should be refreshed after the remaining external data are collected." `
    (Read-MarkdownTable "Table H1. Summary of research strengths")

function Set-ColumnFormats {
    param($Worksheet, [string[]]$Headers, [int]$FirstDataRow, [int]$LastDataRow)
    if ($LastDataRow -lt $FirstDataRow) { return }
    for ($c = 1; $c -le $Headers.Count; $c++) {
        $header = $Headers[$c - 1]
        $range = $Worksheet.Range($Worksheet.Cells.Item($FirstDataRow, $c), $Worksheet.Cells.Item($LastDataRow, $c))
        if ($header -match "(?i)\b(share|percentage|percent|rate)\b") {
            $range.NumberFormat = '0.0%'
        } elseif ($header -match "(?i)(dollar|award amount|expenditure)") {
            $range.NumberFormat = '$#,##0'
        } elseif ($header -match "(?i)(impact factor|journal IF|mean citations|median citations|average citations|research FTE|per faculty|per TT)") {
            $range.NumberFormat = '0.00'
        } elseif ($header -match "(?i)(count|publications|citations|faculty|proposals|awards|memberships|participants|rank|projects|outputs|institutions)$") {
            $range.NumberFormat = '#,##0'
        }
    }
}

function Write-WorksheetTable {
    param($Workbook, [hashtable]$Definition)
    $afterSheet = $Workbook.Worksheets.Item($Workbook.Worksheets.Count)
    $ws = $Workbook.Worksheets.Add([System.Type]::Missing, $afterSheet)
    $ws.Name = $Definition.Sheet
    $ws.Cells.Font.Name = "Aptos"
    $ws.Cells.Font.Size = 10

    $headers = @($Definition.Headers)
    $data = @($Definition.Data)
    $columnCount = [math]::Max(1, $headers.Count)
    $lastColumnLetter = $ws.Cells.Item(1, $columnCount).Address($false, $false) -replace '\d', ''

    $ws.Range("A1:${lastColumnLetter}1").Merge()
    $ws.Range("A1").Value2 = "$($Definition.TableId). $($Definition.Title)"
    $ws.Range("A1:${lastColumnLetter}1").Interior.Color = $colors.Green
    $ws.Range("A1:${lastColumnLetter}1").Font.Color = $colors.White
    $ws.Range("A1:${lastColumnLetter}1").Font.Bold = $true
    $ws.Range("A1:${lastColumnLetter}1").Font.Size = 15
    $ws.Range("A1:${lastColumnLetter}1").RowHeight = 28

    $ws.Range("A2:${lastColumnLetter}2").Merge()
    $ws.Range("A2").Value2 = "Status: $($Definition.Status)"
    $statusColor = if ($Definition.Status -eq "Complete") { $colors.GreenLight } elseif ($Definition.Status -match "Awaiting|Unavailable") { $colors.GoldLight } else { $colors.BlueLight }
    $ws.Range("A2:${lastColumnLetter}2").Interior.Color = $statusColor
    $ws.Range("A2:${lastColumnLetter}2").Font.Bold = $true
    $ws.Range("A2:${lastColumnLetter}2").RowHeight = 20

    $ws.Range("A3:${lastColumnLetter}3").Merge()
    $ws.Range("A3").Value2 = $Definition.Note
    $ws.Range("A3:${lastColumnLetter}3").Font.Color = $colors.GrayDark
    $ws.Range("A3:${lastColumnLetter}3").WrapText = $true
    $ws.Range("A3:${lastColumnLetter}3").RowHeight = 32

    $firstHeaderRow = 5
    for ($c = 1; $c -le $headers.Count; $c++) { $ws.Cells.Item($firstHeaderRow, $c).Value2 = $headers[$c - 1] }
    $headerRange = $ws.Range($ws.Cells.Item($firstHeaderRow, 1), $ws.Cells.Item($firstHeaderRow, $columnCount))
    $headerRange.Interior.Color = $colors.Green
    $headerRange.Font.Color = $colors.White
    $headerRange.Font.Bold = $true
    $headerRange.WrapText = $true
    $headerRange.VerticalAlignment = -4108
    $headerRange.RowHeight = 34

    $firstDataRow = 6
    if ($data.Count -gt 0) {
        for ($r = 0; $r -lt $data.Count; $r++) {
            for ($c = 0; $c -lt $columnCount; $c++) {
                $value = if ($c -lt $data[$r].Count) { $data[$r][$c] } else { $null }
                $ws.Cells.Item($firstDataRow + $r, $c + 1).Value2 = $value
            }
        }
        $lastDataRow = $firstDataRow + $data.Count - 1
        $dataRange = $ws.Range($ws.Cells.Item($firstDataRow, 1), $ws.Cells.Item($lastDataRow, $columnCount))
        $dataRange.VerticalAlignment = -4160
        $dataRange.Borders.Item(9).Color = $colors.Border
        $dataRange.Borders.Item(9).LineStyle = 1
        $dataRange.Borders.Item(9).Weight = 2
        for ($r = $firstDataRow; $r -le $lastDataRow; $r++) {
            if ((($r - $firstDataRow) % 2) -eq 1) { $ws.Range($ws.Cells.Item($r, 1), $ws.Cells.Item($r, $columnCount)).Interior.Color = $colors.Gray }
        }
        if ($Definition.HighlightRowLabel) {
            for ($r = $firstDataRow; $r -le $lastDataRow; $r++) {
                if ([string]$ws.Cells.Item($r, 1).Text -eq $Definition.HighlightRowLabel) {
                    $ws.Range($ws.Cells.Item($r, 1), $ws.Cells.Item($r, $columnCount)).Interior.Color = $colors.GoldLight
                    $ws.Range($ws.Cells.Item($r, 1), $ws.Cells.Item($r, $columnCount)).Font.Bold = $true
                }
            }
        }
        Set-ColumnFormats $ws $headers $firstDataRow $lastDataRow
        $ws.Range($ws.Cells.Item($firstHeaderRow, 1), $ws.Cells.Item($lastDataRow, $columnCount)).AutoFilter() | Out-Null
    } else {
        $lastDataRow = $firstHeaderRow
        $ws.Range("A6:${lastColumnLetter}6").Merge()
        $ws.Range("A6").Value2 = "No data rows are currently available. Use the headers above when the requested data are received."
        $ws.Range("A6:${lastColumnLetter}6").Font.Italic = $true
        $ws.Range("A6:${lastColumnLetter}6").Font.Color = $colors.GrayDark
        $ws.Range("A6:${lastColumnLetter}6").Interior.Color = $colors.GoldLight
        $ws.Range("A6:${lastColumnLetter}6").WrapText = $true
        $ws.Range("A6:${lastColumnLetter}6").RowHeight = 30
    }

    $usedLastRow = if ($data.Count -gt 0) { $lastDataRow } else { 6 }
    for ($c = 1; $c -le $columnCount; $c++) {
        $maxLen = $headers[$c - 1].Length
        $sampleLast = [math]::Min($usedLastRow, 205)
        for ($r = $firstDataRow; $r -le $sampleLast; $r++) {
            $length = ([string]$ws.Cells.Item($r, $c).Text).Length
            if ($length -gt $maxLen) { $maxLen = $length }
        }
        $width = [math]::Min(42, [math]::Max(11, $maxLen + 2))
        if ($headers[$c - 1] -match "(?i)(note|theme|title|organization|institution|partner|award or recognition|proposed action|implication|source)") {
            $width = [math]::Min(48, [math]::Max(22, $width))
        }
        $ws.Columns.Item($c).ColumnWidth = $width
    }
    $ws.Range($ws.Cells.Item($firstDataRow, 1), $ws.Cells.Item($usedLastRow, $columnCount)).WrapText = $true
    $ws.Activate()
    $ws.Application.ActiveWindow.DisplayGridlines = $false
    $ws.Application.ActiveWindow.SplitRow = 5
    $ws.Application.ActiveWindow.FreezePanes = $true
    $ws.PageSetup.Orientation = 2
    $ws.PageSetup.Zoom = $false
    $ws.PageSetup.FitToPagesWide = 1
    $ws.PageSetup.FitToPagesTall = $false
    $ws.PageSetup.LeftMargin = $ws.Application.InchesToPoints(0.35)
    $ws.PageSetup.RightMargin = $ws.Application.InchesToPoints(0.35)
    $ws.PageSetup.TopMargin = $ws.Application.InchesToPoints(0.5)
    $ws.PageSetup.BottomMargin = $ws.Application.InchesToPoints(0.5)
    $ws.PageSetup.PrintTitleRows = '$1:$5'
    $ws.PageSetup.PrintArea = "A1:${lastColumnLetter}${usedLastRow}"
    return $ws
}

$excel = $null
$workbook = $null
try {
    $excel = New-Object -ComObject Excel.Application
    $excel.Visible = $false
    $excel.DisplayAlerts = $false
    $excel.ScreenUpdating = $false
    $workbook = $excel.Workbooks.Add()

    while ($workbook.Worksheets.Count -gt 1) { $workbook.Worksheets.Item($workbook.Worksheets.Count).Delete() }
    $contents = $workbook.Worksheets.Item(1)
    $contents.Name = "Contents"
    $contents.Cells.Font.Name = "Aptos"
    $contents.Cells.Font.Size = 10
    $contents.Range("A1:F1").Merge()
    $contents.Range("A1").Value2 = "Section 4 Research and Creative Artistry: Consolidated Report Tables"
    $contents.Range("A1:F1").Interior.Color = $colors.Green
    $contents.Range("A1:F1").Font.Color = $colors.White
    $contents.Range("A1:F1").Font.Bold = $true
    $contents.Range("A1:F1").Font.Size = 16
    $contents.Range("A1:F1").RowHeight = 30
    $contents.Range("A2:F2").Merge()
    $contents.Range("A2").Value2 = "Coverage: 2021 through August 2026. Use the status column to distinguish completed results from partial tables and data-request templates."
    $contents.Range("A2:F2").Interior.Color = $colors.GreenLight
    $contents.Range("A2:F2").WrapText = $true
    $contents.Range("A2:F2").RowHeight = 30
    $contentsHeaders = [object[,]]::new(1, 6)
    $contentsHeaderValues = @("Section", "Table", "Worksheet", "Status", "Description", "Source or next step")
    for ($c = 0; $c -lt 6; $c++) { $contentsHeaders[0, $c] = $contentsHeaderValues[$c] }
    $contents.Range("A4:F4").Value2 = [object]$contentsHeaders
    $contents.Range("A4:F4").Interior.Color = $colors.Green
    $contents.Range("A4:F4").Font.Color = $colors.White
    $contents.Range("A4:F4").Font.Bold = $true

    $row = 5
    foreach ($definition in $tables) {
        $ws = Write-WorksheetTable $workbook $definition
        $contents.Cells.Item($row, 1).Value2 = $definition.Section
        $contents.Cells.Item($row, 2).Value2 = $definition.TableId
        $contents.Cells.Item($row, 3).Value2 = $definition.Sheet
        $contents.Hyperlinks.Add($contents.Cells.Item($row, 3), "", "'$($definition.Sheet)'!A1", "Open worksheet", $definition.Sheet) | Out-Null
        $contents.Cells.Item($row, 4).Value2 = $definition.Status
        $contents.Cells.Item($row, 5).Value2 = $definition.Title
        $contents.Cells.Item($row, 6).Value2 = if ($definition.Status -match "Awaiting|Unavailable") { $definition.Note } else { $definition.Source }
        if ((($row - 5) % 2) -eq 1) { $contents.Range("A${row}:F${row}").Interior.Color = $colors.Gray }
        if ($definition.Status -eq "Complete") { $contents.Cells.Item($row, 4).Interior.Color = $colors.GreenLight }
        elseif ($definition.Status -match "Awaiting|Unavailable") { $contents.Cells.Item($row, 4).Interior.Color = $colors.GoldLight }
        else { $contents.Cells.Item($row, 4).Interior.Color = $colors.BlueLight }
        $row++
    }

    $contents.Range("A5:F$($row - 1)").VerticalAlignment = -4160
    $contents.Range("A5:F$($row - 1)").WrapText = $true
    $contents.Range("A4:F$($row - 1)").AutoFilter() | Out-Null
    $contents.Columns.Item(1).ColumnWidth = 14
    $contents.Columns.Item(2).ColumnWidth = 18
    $contents.Columns.Item(3).ColumnWidth = 25
    $contents.Columns.Item(4).ColumnWidth = 19
    $contents.Columns.Item(5).ColumnWidth = 48
    $contents.Columns.Item(6).ColumnWidth = 48
    $contents.Rows("5:$($row - 1)").RowHeight = 32
    $contents.Activate()
    $contents.Application.ActiveWindow.DisplayGridlines = $false
    $contents.Application.ActiveWindow.SplitRow = 4
    $contents.Application.ActiveWindow.FreezePanes = $true
    $contents.PageSetup.Orientation = 2
    $contents.PageSetup.Zoom = $false
    $contents.PageSetup.FitToPagesWide = 1
    $contents.PageSetup.FitToPagesTall = $false

    $contents.Activate()
    if (Test-Path -LiteralPath $tempPath) { Remove-Item -LiteralPath $tempPath -Force }
    $workbook.SaveAs($tempPath, 51)
    $workbook.Close($true)
    $workbook = $null
    $excel.Quit()
    $excel = $null

    if (Test-Path -LiteralPath $outputPath) { Remove-Item -LiteralPath $outputPath -Force }
    Move-Item -LiteralPath $tempPath -Destination $outputPath
    Write-Output "Created $outputPath"
    Write-Output "Worksheets: $($tables.Count + 1)"
}
finally {
    if ($null -ne $workbook) { try { $workbook.Close($false) } catch {} }
    if ($null -ne $excel) { try { $excel.Quit() } catch {} }
    if (Test-Path -LiteralPath $tempPath) {
        try { Remove-Item -LiteralPath $tempPath -Force } catch {}
    }
    [GC]::Collect()
    [GC]::WaitForPendingFinalizers()
}
