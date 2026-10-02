"""Build the Section 4 tables-and-figures packet from current project outputs.

The packet follows the order in which items appear in the strategic draft and
omits template-only tables. Run from the project root:

    python code/09_build_section4_appendix.py
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Callable, Iterable
from xml.sax.saxutils import escape

from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_LEFT
from reportlab.lib.pagesizes import landscape, letter
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.lib.units import inch
from reportlab.platypus import (
    Image,
    PageBreak,
    Paragraph,
    SimpleDocTemplate,
    Spacer,
    Table,
    TableStyle,
)


ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "output" / "pdf" / "SECTION-4-TABLES-AND-FIGURES-PACKET.pdf"

CSU_GREEN = colors.HexColor("#1E4D2B")
CSU_GOLD = colors.HexColor("#C8C372")
LIGHT_GREEN = colors.HexColor("#E8EFEA")
LIGHT_GRAY = colors.HexColor("#F4F5F4")
MID_GRAY = colors.HexColor("#D5D9D6")
DARK = colors.HexColor("#263238")
SLATE = colors.HexColor("#59636D")
PAGE_W, PAGE_H = landscape(letter)


def clean(value) -> str:
    if value is None:
        return ""
    text = str(value).strip()
    if text.lower() in {"na", "nan", "none", "null"}:
        return ""
    replacements = {
        "\u2013": "-",
        "\u2014": "-",
        "\u2011": "-",
        "\u2212": "-",
        "\u2018": "'",
        "\u2019": "'",
        "\u201c": '"',
        "\u201d": '"',
        "\u2026": "...",
        "\u00a0": " ",
    }
    for source, target in replacements.items():
        text = text.replace(source, target)
    return text


def read_csv(relative_path: str) -> list[dict[str, str]]:
    path = ROOT / relative_path
    if not path.exists():
        raise FileNotFoundError(path)
    with path.open("r", encoding="utf-8-sig", newline="") as stream:
        return [{key: clean(value) for key, value in row.items()} for row in csv.DictReader(stream)]


def as_float(value) -> float | None:
    text = clean(value).replace("$", "").replace(",", "").replace("%", "")
    if not text:
        return None
    try:
        return float(text)
    except ValueError:
        return None


def integer(value) -> str:
    number = as_float(value)
    return "" if number is None else f"{number:,.0f}"


def year_value(value) -> str:
    number = as_float(value)
    return clean(value) if number is None else f"{number:.0f}"


def decimal(value, digits: int = 2) -> str:
    number = as_float(value)
    return "" if number is None else f"{number:,.{digits}f}"


def dollars(value) -> str:
    number = as_float(value)
    return "" if number is None else f"${number:,.0f}"


def percent(value, proportion: bool = True, digits: int = 1) -> str:
    number = as_float(value)
    if number is None:
        return ""
    if proportion:
        number *= 100
    return f"{number:.{digits}f}%"


def year_label(value, partial="") -> str:
    year = year_value(value)
    if not year:
        return clean(value)
    return f"{year} partial" if clean(partial).lower() == "true" else year


styles = getSampleStyleSheet()
TITLE = ParagraphStyle(
    "PacketTitle",
    parent=styles["Title"],
    fontName="Helvetica-Bold",
    fontSize=25,
    leading=29,
    alignment=TA_CENTER,
    textColor=CSU_GREEN,
    spaceAfter=14,
)
SUBTITLE = ParagraphStyle(
    "PacketSubtitle",
    parent=styles["Normal"],
    fontName="Helvetica",
    fontSize=12,
    leading=16,
    alignment=TA_CENTER,
    textColor=DARK,
)
ITEM_TITLE = ParagraphStyle(
    "ItemTitle",
    parent=styles["Heading1"],
    fontName="Helvetica-Bold",
    fontSize=14,
    leading=17,
    textColor=CSU_GREEN,
    spaceAfter=5,
)
NOTE = ParagraphStyle(
    "Note",
    parent=styles["Normal"],
    fontName="Helvetica",
    fontSize=8.2,
    leading=10.2,
    textColor=SLATE,
    spaceAfter=8,
)
CONTENTS = ParagraphStyle(
    "Contents",
    parent=styles["Normal"],
    fontName="Helvetica",
    fontSize=8.5,
    leading=11,
    textColor=DARK,
    leftIndent=8,
)


def paragraph(text: str, style: ParagraphStyle) -> Paragraph:
    return Paragraph(escape(clean(text)).replace("\n", "<br/>"), style)


def cell_style(font_size: float) -> ParagraphStyle:
    return ParagraphStyle(
        f"Cell{font_size}",
        parent=styles["Normal"],
        fontName="Helvetica",
        fontSize=font_size,
        leading=font_size + 1.2,
        textColor=DARK,
        alignment=TA_LEFT,
    )


def header_style(font_size: float) -> ParagraphStyle:
    return ParagraphStyle(
        f"Header{font_size}",
        parent=styles["Normal"],
        fontName="Helvetica-Bold",
        fontSize=font_size,
        leading=font_size + 1.2,
        textColor=colors.white,
        alignment=TA_LEFT,
    )


def widths_from_shares(shares: Iterable[float]) -> list[float]:
    usable = PAGE_W - 0.72 * inch
    values = list(shares)
    total = sum(values)
    return [usable * value / total for value in values]


def table_flowable(
    headers: list[str],
    rows: list[list[str]],
    shares: list[float] | None = None,
    font_size: float = 6.4,
) -> Table:
    if shares is None:
        shares = [1.0] * len(headers)
    header_cells = [paragraph(header, header_style(font_size)) for header in headers]
    body_style = cell_style(font_size)
    body = [[paragraph(value, body_style) for value in row] for row in rows]
    table = Table(
        [header_cells] + body,
        colWidths=widths_from_shares(shares),
        repeatRows=1,
        splitByRow=1,
        hAlign="LEFT",
    )
    commands = [
        ("BACKGROUND", (0, 0), (-1, 0), CSU_GREEN),
        ("TEXTCOLOR", (0, 0), (-1, 0), colors.white),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("GRID", (0, 0), (-1, -1), 0.25, MID_GRAY),
        ("LEFTPADDING", (0, 0), (-1, -1), 3),
        ("RIGHTPADDING", (0, 0), (-1, -1), 3),
        ("TOPPADDING", (0, 0), (-1, -1), 2.5),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 2.5),
    ]
    for row_index in range(1, len(rows) + 1):
        background = colors.white if row_index % 2 else LIGHT_GRAY
        commands.append(("BACKGROUND", (0, row_index), (-1, row_index), background))
    table.setStyle(TableStyle(commands))
    return table


def add_table(
    story: list,
    title: str,
    headers: list[str],
    rows: list[list[str]],
    note: str = "",
    shares: list[float] | None = None,
    font_size: float = 6.4,
) -> None:
    story.append(PageBreak())
    story.append(paragraph(title, ITEM_TITLE))
    if note:
        story.append(paragraph(note, NOTE))
    story.append(table_flowable(headers, rows, shares, font_size))


def add_figure(story: list, title: str, relative_path: str, note: str = "") -> None:
    path = ROOT / relative_path
    if not path.exists():
        raise FileNotFoundError(path)
    story.append(PageBreak())
    story.append(paragraph(title, ITEM_TITLE))
    if note:
        story.append(paragraph(note, NOTE))
    image = Image(str(path))
    max_width = PAGE_W - 0.9 * inch
    # Reserve enough vertical space for the packet title and note. Without
    # this reserve, very tall source images can be deferred and then hidden
    # by the following explicit page break.
    max_height = PAGE_H - 2.25 * inch
    scale = min(max_width / image.imageWidth, max_height / image.imageHeight)
    image.drawWidth = image.imageWidth * scale
    image.drawHeight = image.imageHeight * scale
    image.hAlign = "CENTER"
    story.append(image)


def page_decor(canvas, document) -> None:
    page = canvas.getPageNumber()
    canvas.saveState()
    if page > 1:
        canvas.setStrokeColor(CSU_GREEN)
        canvas.setLineWidth(1.0)
        canvas.line(0.36 * inch, PAGE_H - 0.30 * inch, PAGE_W - 0.36 * inch, PAGE_H - 0.30 * inch)
        canvas.setFont("Helvetica-Bold", 8)
        canvas.setFillColor(CSU_GREEN)
        canvas.drawString(0.36 * inch, PAGE_H - 0.22 * inch, "DARE Research and Creative Artistry")
        canvas.setFont("Helvetica", 8)
        canvas.setFillColor(SLATE)
        canvas.drawRightString(PAGE_W - 0.36 * inch, PAGE_H - 0.22 * inch, "Tables and figures packet")
        canvas.line(0.36 * inch, 0.28 * inch, PAGE_W - 0.36 * inch, 0.28 * inch)
        canvas.drawString(0.36 * inch, 0.15 * inch, "Reporting period: 2021 through August 2026")
        canvas.drawRightString(PAGE_W - 0.36 * inch, 0.15 * inch, f"Page {page}")
    canvas.restoreState()


def selected(rows: list[dict[str, str]], spec: list[tuple[str, str, Callable[[str], str] | None]]) -> tuple[list[str], list[list[str]]]:
    headers = [label for _, label, _ in spec]
    output = []
    for row in rows:
        values = []
        for field, _, formatter in spec:
            value = row.get(field, "")
            values.append(formatter(value) if formatter else clean(value))
        output.append(values)
    return headers, output


def faculty_snapshot() -> tuple[list[str], list[list[str]]]:
    data = read_csv("output/table_A1_faculty_data_snapshot.csv")
    asterisk_names = {"Dessa Watson", "Katharine Eshelman", "Kathie Riley", "Kathy Riley", "Laston Charriez"}
    output = []
    for row in data:
        name = clean(row["faculty_name"]).title()
        teaching = integer(row["teaching_percent"])
        research = integer(row["research_percent"])
        service = integer(row["service_percent"])
        appointment = "/".join(value or "NA" for value in (teaching, research, service))
        if name in asterisk_names:
            active_period = "*"
            years = "*"
        else:
            first = year_value(row["first_active_year"])
            last = year_value(row["last_active_year"])
            active_period = f"{first}-{last}" if first and last else ""
            years = integer(row["active_years_in_window"])
        status = "Departed/retired" if clean(row["departed_or_retired"]).lower() == "true" else "Active through 2026"
        output.append([
            name,
            row["research_area"],
            row["faculty_type"],
            row["faculty_rank"],
            appointment,
            active_period,
            years,
            status,
        ])
    return ["Faculty", "Research area", "Faculty type", "Rank", "T/R/S (%)", "Active period", "Years in window", "Status"], output


def publication_productivity() -> tuple[list[str], list[list[str]]]:
    data = read_csv("output/table_D1_publications_by_year.csv")
    rows = []
    for row in data:
        rows.append([
            year_label(row["year"], "true" if row["year"] == "2026" else "false"),
            integer(row["active_tt_faculty"]),
            integer(row["unique_department_publications"]),
            integer(row["faculty_publication_count"]),
        ])
    rows.append(["Total", "", integer(sum(as_float(row["unique_department_publications"]) or 0 for row in data)), integer(sum(as_float(row["faculty_publication_count"]) or 0 for row in data))])
    return ["Year", "Active tenure-track faculty", "Unique department publications", "Faculty-publication credits"], rows


def citation_table() -> tuple[list[str], list[list[str]]]:
    data = read_csv("output/table_D4b_citations_by_publication_year.csv")
    rows = []
    for row in data:
        year = clean(row["year"])
        if year == "2026":
            year = "2026 partial"
        rows.append([
            year,
            integer(row["peer_reviewed_journal_publications"]),
            integer(row["total_google_scholar_citations"]),
            decimal(row["mean_google_scholar_citations_per_publication"]),
            decimal(row["median_google_scholar_citations_per_publication"]),
            percent(row["percentage_of_publications_cited_in_google_scholar"], proportion=False, digits=1),
            decimal(row["publication_weighted_average_journal_impact_factor"]),
        ])
    return ["Publication year", "Publications", "Google Scholar citations", "Mean citations per publication", "Median citations per publication", "Percentage cited", "Publication-weighted average journal IF"], rows


def top_journals() -> tuple[list[str], list[list[str]]]:
    data = read_csv("output/table_D4c_journals_by_publication_count.csv")[:10]
    rows = []
    for row in data:
        impact = decimal(row["average_impact_factor"])
        rows.append([
            row["journal"],
            integer(row["publication_count"]),
            impact or "Not available",
            integer(row["total_google_scholar_citations"]),
            decimal(row["average_google_scholar_citations_per_publication"]),
        ])
    return ["Journal", "Publications", "Average impact factor", "Total Google Scholar citations", "Average Google Scholar citations per publication"], rows


def sponsored_main() -> tuple[list[str], list[list[str]]]:
    data = read_csv("output/table_C1_sponsored_projects_by_year.csv")
    rows = []
    for row in data:
        rows.append([
            year_label(row["year"], row["partial_year"]),
            integer(row["proposals_submitted"]),
            integer(row["awards_received"]),
            dollars(row["total_awarded_dollars"]),
            dollars(row["average_award_amount"]),
        ])
    proposals = sum(as_float(row["proposals_submitted"]) or 0 for row in data)
    awards = sum(as_float(row["awards_received"]) or 0 for row in data)
    total_dollars = sum(as_float(row["total_awarded_dollars"]) or 0 for row in data)
    rows.append(["Total", integer(proposals), integer(awards), dollars(total_dollars), dollars(total_dollars / awards if awards else 0)])
    return ["Year", "Proposals submitted", "Funded award count", "Total award dollars", "Average award amount"], rows


def strategic_rows() -> tuple[list[str], list[list[str]]]:
    rows = [
        ["264 peer-reviewed publications and 344 faculty-publication credits across mixed appointments", "Sustain output and quality as teaching, Extension, and service demands continue", "Protect research time where strategically warranted; maintain access to data, software, and research assistance", "Preserve scholarly productivity and support ambitious work"],
        ["Strong journal placement and 4,649 verified citations; SRI percentile rank of 91.7 among public land-grant peers", "Maintain a consistent benchmark and test whether conclusions change under an R1-only comparison", "Secure recurring Academic Analytics access, document metric windows and faculty inclusion rules, and assign responsibility for annual benchmarking", "Support defensible claims about national standing and identify specific areas for improvement"],
        ["$63.42 million in awards and leadership of a $50.0 million regional initiative", "Reduce dependence on a small number of federal sponsors without weakening productive USDA relationships", "Provide proposal-development support, interdisciplinary seed funding, and partner development for state, foundation, nonprofit, and mission-aligned industry opportunities", "Increase portfolio resilience and create new pathways for impact"],
        ["Two-way CAS project collaboration and 325 documented external publication partners", "Convert broad relationships into sustained interdisciplinary programs and proposals", "Support cross-college team formation, shared research staff, and development of multistate and multi-institution proposals", "Extend the scope, competitiveness, and application of DARE research"],
        ["75 student-coauthored outputs, 441 new faculty committee roles, and 14 student awards", "Expand access to research experiences and document career outcomes", "Increase graduate research assistant, undergraduate research, travel, data, and software support; record participation and outcomes consistently", "Prepare students for research-intensive academic, public, nonprofit, and private-sector careers"],
        ["Competitive faculty recognition and several evidence-based nomination prospects", "Translate scholarly accomplishment into greater disciplinary visibility", "Continue the awards committee's nomination planning and provide administrative support for nomination packages", "Increase consideration for university awards, fellowships, association honors, editorial leadership, and national recognition"],
        ["Strong award data but no comparable expenditure series", "Demonstrate how sponsored resources support people, students, and research activity", "Obtain annual expenditure and appointment data from central and college systems", "Distinguish awards from expenditures and support valid CAS and R1 comparisons"],
        ["Breadth across AFE, ENRE, and AgEd", "Recruit and retain faculty in priority fields while maintaining coverage during retirements and departures", "Use a multiyear hiring plan, competitive start-up support, mentoring, and clear workload expectations", "Preserve disciplinary depth and support emerging research priorities"],
        ["Shared computing services and proprietary consumer, retail, sensory, location, and foot-traffic data", "Sustain and expand infrastructure that depends on licenses, secure storage, technical administration, and responsible data governance", "Maintain data licenses, computing and storage capacity, technical support, research staff, software, survey and fieldwork support, and clear access procedures", "Differentiate DARE from peer programs and support externally competitive faculty and student research"],
    ]
    return ["Evidence and strength", "Anticipated challenge or opportunity", "Resource or action needed", "Intended effect"], rows


def build() -> None:
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    doc = SimpleDocTemplate(
        str(OUTPUT),
        pagesize=landscape(letter),
        leftMargin=0.36 * inch,
        rightMargin=0.36 * inch,
        topMargin=0.43 * inch,
        bottomMargin=0.34 * inch,
        title="Section 4 Research and Creative Artistry - Tables and Figures Packet",
        author="Department of Agricultural and Resource Economics, Colorado State University",
        subject="Supporting tables and figures in narrative order",
    )
    story: list = [Spacer(1, 1.25 * inch)]
    story.append(paragraph("Section 4: Research and Creative Artistry", TITLE))
    story.append(paragraph("Tables and Figures Packet", ParagraphStyle("PacketSubhead", parent=TITLE, fontSize=20, leading=24, textColor=DARK)))
    story.append(Spacer(1, 0.32 * inch))
    story.append(paragraph("Department of Agricultural and Resource Economics<br/>Colorado State University", SUBTITLE))
    story.append(Spacer(1, 0.25 * inch))
    story.append(paragraph("Reporting period: 2021 through August 2026", SUBTITLE))
    story.append(Spacer(1, 0.42 * inch))
    story.append(paragraph("Items are ordered by first appearance in the strategic draft. The packet includes only tables and figures supported by currently available data.", ParagraphStyle("CoverNote", parent=NOTE, alignment=TA_CENTER, fontSize=9.5, leading=13)))

    contents = [
        "Appendix Table A1: Faculty Data Snapshot",
        "Appendix Table D2: Peer-reviewed publications by research area",
        "Table 4.1: Peer-reviewed journal publication productivity",
        "Appendix Figure D1: Annual peer-reviewed publication output",
        "Appendix Figure D2: Publications relative to faculty capacity",
        "Table 4.2: Citation and journal-placement indicators by publication year",
        "Table 4.3: Journals with the largest numbers of DARE publications",
        "Appendix Table D3: Other scholarly outputs by year",
        "Appendix Table B1: Scholarly Research Index among public land-grant peers",
        "Appendix Figure B1: Scholarly Research Index distribution",
        "Appendix Table B2: Academic Analytics productivity indicators",
        "Figure 4.1: DARE research productivity relative to public land-grant peers",
        "Appendix Figure B3: DARE output share relative to faculty share",
        "Appendix Table D4: Academic Analytics citation-impact indicators",
        "Table 4.4: DARE-led sponsored-project activity by proposal year",
        "Appendix Table C1: Sponsored-project activity and faculty leadership by year",
        "Appendix Table C7: Proposal and award activity across CAS academic departments",
        "Appendix Table C3: Sponsored awards by originating sponsor",
        "Appendix Table C2: Sponsored awards by funding-source type and year",
        "Appendix Table E2: Sponsored-project collaboration across CSU units",
        "Appendix Figure C1: Distribution of F&A rates by direct-sponsor category",
        "Appendix Table E1: Within-department research collaboration",
        "Appendix Table E3: External and cross-institutional collaboration",
        "Appendix Figure E1: Academic Analytics collaboration across CSU units",
        "Appendix Figure E2: Academic Analytics external collaboration",
        "Appendix Table F1: Faculty research awards, fellowships, and scholarly recognitions",
        "Appendix Table G1: Student participation in research and scholarly activity",
        "Appendix Table G2: Student-coauthored scholarly outputs",
        "Appendix Table H1: Research strengths, anticipated challenges, and resource priorities",
    ]
    story.append(PageBreak())
    story.append(paragraph("Contents in narrative order", ITEM_TITLE))
    for item in contents:
        story.append(paragraph(f"- {item}", CONTENTS))

    headers, rows = faculty_snapshot()
    add_table(story, "Appendix Table A1. Faculty Data Snapshot", headers, rows, "Appointment is shown as teaching/research/service percentages. *Continuing or contract faculty identified by the department; their data are not included in the tenure-track analysis.", [1.25, 0.8, 1.0, 0.85, 0.55, 0.65, 0.55, 0.9], 5.9)

    data = read_csv("output/table_D2_publications_by_area.csv")
    headers, rows = selected(data, [("year", "Year", year_value), ("agricultural_education", "Agricultural Education", integer), ("agricultural_and_food_economics", "Agricultural and Food Economics", integer), ("enre", "Environmental and Natural Resource Economics", integer), ("cross_area_publications", "Cross-area", integer), ("unclassified_publications", "Unclassified", integer), ("department_total", "Department total", integer)])
    for row in rows:
        if row[0] == "2026":
            row[0] = "2026 partial"
    add_table(story, "Appendix Table D2. Peer-reviewed publications by research area, 2021-2026", headers, rows, "Publications spanning research areas are shown separately and counted once in the department total.", [0.7, 1.05, 1.5, 1.6, 0.75, 0.75, 0.8], 6.5)

    headers, rows = publication_productivity()
    add_table(story, "Table 4.1. Peer-reviewed journal publication productivity, 2021-2026", headers, rows, "A faculty-publication credit counts one publication for each participating DARE faculty member; unique publications count each article once for the department.", [0.8, 1.25, 1.5, 1.35], 7.0)
    add_figure(story, "Appendix Figure D1. Annual peer-reviewed publication output", "output/figures/figure_D1_publication_counts.png", "The 2026 value represents a partial year through August.")
    add_figure(story, "Appendix Figure D2. Publications relative to faculty capacity", "output/figures/figure_D2_publications_per_faculty_and_fte.png", "Headcount and research FTE describe the same faculty from different perspectives; 2026 is partial.")

    headers, rows = citation_table()
    add_table(story, "Table 4.2. Citation and journal-placement indicators by publication year, 2021-2026", headers, rows, "Google Scholar citations are cumulative and older publication cohorts have had more time to accrue citations. Journal IF is the publication-weighted average among publications with an available impact factor.", [0.75, 0.7, 0.95, 1.0, 1.0, 0.85, 1.15], 6.1)
    headers, rows = top_journals()
    add_table(story, "Table 4.3. Journals with the largest numbers of DARE publications", headers, rows, "Journals are ranked by the number of unique DARE publications during the review period.", [2.3, 0.65, 0.9, 1.0, 1.15], 6.5)

    data = read_csv("output/table_D3_other_scholarly_outputs.csv")
    spec = [("output_category", "Output category", None)] + [(f"Year_{year}", str(year), integer) for year in range(2021, 2027)] + [("six_year_total", "Six-year total", integer)]
    headers, rows = selected(data, spec)
    add_table(story, "Appendix Table D3. Other scholarly outputs by year, 2021-2026", headers, rows, "These outputs are reported separately from peer-reviewed journal publication totals.", [1.7, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.7], 6.6)

    data = read_csv("output/academic_analytics/table_B1_AA_SRI_peer_comparison.csv")
    headers, rows = selected(data, [("institution", "Institution", None), ("comparison_unit", "Comparison unit", None), ("faculty_count", "Faculty", integer), ("scholarly_research_index", "SRI", lambda x: decimal(x, 1)), ("derived_rank", "Rank", integer), ("derived_percentile", "Percentile", lambda x: decimal(x, 1)), ("comparison_year", "AA release", None), ("comparability_note", "Comparability note", None)])
    add_table(story, "Appendix Table B1. Scholarly Research Index among public land-grant peers", headers, rows, "The comparison includes 36 public land-grant Agricultural Economics units. DARE uses the 21-member Stage 1 research-active roster.", [1.25, 1.55, 0.45, 0.4, 0.4, 0.55, 0.8, 1.35], 5.5)
    add_figure(story, "Appendix Figure B1. Scholarly Research Index distribution among public land-grant peers", "output/figures/academic_analytics/figure_AA_SRI_peer_comparison.png", "DARE's Stage 1 model excludes four faculty without a research role in this benchmark.")

    data = read_csv("output/academic_analytics/table_B2_AA_productivity_peer_comparison.csv")
    headers, rows = selected(data, [("indicator", "Indicator", None), ("department_value", "DARE value", lambda x: decimal(x, 2)), ("peer_median", "Peer median", lambda x: decimal(x, 2)), ("peer_75th_percentile", "Peer 75th percentile", lambda x: decimal(x, 2)), ("peer_maximum", "Peer maximum", lambda x: decimal(x, 2)), ("department_rank", "DARE rank", integer), ("department_percentile", "DARE percentile", lambda x: decimal(x, 1)), ("interpretation", "Interpretation", None)])
    add_table(story, "Appendix Table B2. Academic Analytics productivity indicators among public land-grant peers", headers, rows, "Per-faculty measures account for differences in department size. Academic Analytics applies metric-specific coverage periods.", [1.45, 0.65, 0.7, 0.85, 0.7, 0.55, 0.7, 0.8], 6.2)
    add_figure(story, "Figure 4.1. DARE research productivity relative to public land-grant peers", "output/figures/academic_analytics/figure_AA_productivity_percentiles.png")
    add_figure(story, "Appendix Figure B3. DARE output share relative to faculty share", "output/figures/academic_analytics/figure_AA_market_share_vs_faculty_share.png", "Peer totals replace DARE's unfiltered values with the 21-member Stage 1 unit values.")

    data = read_csv("output/academic_analytics/table_D4_AA_citation_impact_indicators.csv")
    headers, rows = selected(data, [("indicator", "Indicator", None), ("department_value", "DARE value", lambda x: decimal(x, 2)), ("department_rank", "DARE rank", integer), ("department_percentile", "DARE percentile", lambda x: decimal(x, 1)), ("peer_units", "Peer units", integer), ("data_source", "Data source", None), ("data_release", "Data release", None)])
    add_table(story, "Appendix Table D4. Academic Analytics citation-impact indicators", headers, rows, "These indicators support peer comparison and should not be combined with the Google Scholar totals in Table 4.2 because coverage and definitions differ.", [1.6, 0.7, 0.6, 0.75, 0.6, 0.9, 0.95], 6.5)

    headers, rows = sponsored_main()
    add_table(story, "Table 4.4. DARE-led sponsored-project activity by proposal year, 2021-2026", headers, rows, "Awards are assigned to the proposal-submission cohort and multi-year award totals are counted once. The 2026 cohort is partial.", [0.7, 1.05, 1.0, 1.1, 1.1], 7.0)

    data = read_csv("output/table_C1_sponsored_projects_by_year.csv")
    rows = [[year_label(r["year"], r["partial_year"]), integer(r["active_faculty"]), integer(r["proposals_submitted"]), integer(r["awards_received"]), dollars(r["total_awarded_dollars"]), dollars(r["average_award_amount"]), integer(r["faculty_serving_as_pi"]), integer(r["faculty_serving_as_copi"]), integer(r["faculty_serving_as_pi_or_copi"]), percent(r["share_active_faculty_pi_or_copi"], proportion=True)] for r in data]
    headers = ["Year", "Active faculty", "Proposals", "Awards", "Award dollars", "Average award", "Faculty PI", "Faculty co-PI", "Unique PI or co-PI", "Share of active faculty"]
    add_table(story, "Appendix Table C1. Sponsored-project activity and faculty leadership by year, 2021-2026", headers, rows, "Faculty PI/co-PI counts cover active, rostered DARE faculty participating anywhere in the CAS project records. A person serving in both roles is counted once in the combined column.", [0.65, 0.65, 0.6, 0.55, 0.85, 0.8, 0.55, 0.6, 0.75, 0.8], 5.7)

    data = [r for r in read_csv("output/appendix_CAS_awards_department_summary.csv") if r["lead_unit_code"] != "1101"]
    headers, rows = selected(data, [("lead_unit_name", "Department", None), ("proposals_submitted_2021_2026", "Proposals", integer), ("awards_received_2021_2026", "Funded awards", integer), ("recorded_funded_share", "Recorded funded share", lambda x: percent(x, True)), ("total_awarded_dollars_2021_2026", "Award dollars", dollars), ("average_award_amount", "Average award", dollars), ("proposal_count_rank_among_cas_departments", "Proposal rank", integer), ("funded_award_count_rank_among_cas_departments", "Funded-count rank", integer), ("award_dollars_rank_among_cas_departments", "Dollar rank", integer), ("average_award_rank_among_cas_departments", "Average-award rank", integer)])
    add_table(story, "Appendix Table C7. Proposal and award activity across CAS academic departments, 2021-2026", headers, rows, "Ranks compare the five academic departments represented in the source records. Award amounts are not expenditures.", [1.35, 0.55, 0.65, 0.75, 0.85, 0.8, 0.55, 0.65, 0.55, 0.65], 5.4)

    data = read_csv("output/table_C3_awards_by_sponsor.csv")
    headers, rows = selected(data, [("originating_sponsor", "Originating sponsor", None), ("funding_source_type", "Source type", None), ("direct_pass_through_sponsors", "Direct or pass-through sponsor(s)", None), ("number_of_awards", "Awards", integer), ("total_awarded_dollars", "Award dollars", dollars), ("number_of_dare_faculty_involved", "DARE faculty", integer), ("primary_research_area", "Primary research area", None), ("years_represented", "Years", None)])
    add_table(story, "Appendix Table C3. Sponsored awards by originating sponsor", headers, rows, "The originating sponsor is the prime sponsor when present; direct higher-education sponsors are retained as pass-through partners.", [1.35, 0.7, 1.8, 0.45, 0.7, 0.5, 0.7, 0.75], 5.2)

    data = read_csv("output/table_C2_awards_by_source_year_long.csv")
    headers, rows = selected(data, [("funding_source_type", "Funding-source type", None), ("year", "Year", year_value), ("award_count", "Award count", integer), ("award_dollars", "Award dollars", dollars)])
    for source_row, output_row in zip(data, rows):
        if source_row["partial_year"].lower() == "true":
            output_row[1] += " partial"
    add_table(story, "Appendix Table C2. Sponsored awards by funding-source type and year", headers, rows, "Each funded project is counted once in its proposal-submission year.", [1.4, 0.7, 0.8, 1.0], 6.8)

    data = read_csv("output/table_E2_collaboration_across_CSU_units.csv")
    headers, rows = selected(data, [("csu_unit", "CSU unit", None), ("collaboration_direction", "Collaboration direction", None), ("collaborative_project_proposals", "Project proposals", integer), ("funded_collaborative_projects", "Funded projects", integer), ("associated_funded_award_dollars", "Associated award dollars", dollars), ("faculty_or_investigators_involved", "Faculty or investigators", integer), ("years_active", "Years active", None)])
    add_table(story, "Appendix Table E2. Sponsored-project collaboration across CSU units", headers, rows, "The two directions distinguish other-unit investigators on DARE-led projects from DARE faculty participation on projects led by other CAS units.", [1.2, 1.45, 0.7, 0.65, 0.85, 0.75, 0.85], 5.7)
    add_figure(story, "Appendix Figure C1. Distribution of F&A rates by direct-sponsor category", "output/figures/figure_grant_fa_rate_by_sponsor_type.png", "Pass-through institutions are reported separately. Categories labeled Other are excluded from the figure but remain in the underlying grant records.")

    data = read_csv("output/table_E1_within_department_collaboration.csv")
    headers, rows = selected(data, [("year", "Year", year_value), ("unique_qualifying_publications", "Unique publications", integer), ("publications_with_two_or_more_department_faculty", "Publications with 2+ DARE faculty", integer), ("share_with_multiple_department_faculty", "Share with multiple DARE faculty", lambda x: percent(x, True)), ("cross_area_publications", "Cross-area publications", integer), ("faculty_involved", "Faculty involved", integer)])
    for source_row, output_row in zip(data, rows):
        if source_row["partial_year"].lower() == "true":
            output_row[0] += " partial"
    add_table(story, "Appendix Table E1. Within-department research collaboration", headers, rows, "Each publication is counted once in the unique-publication total.", [0.65, 0.85, 1.15, 1.15, 0.9, 0.75], 6.3)

    data = read_csv("output/table_E3_external_collaboration.csv")
    headers, rows = selected(data, [("collaborating_institution_or_partner", "Collaborating institution or partner", None), ("partner_type", "Partner type", None), ("country_code", "Country", None), ("coauthored_publications", "Coauthored publications", integer), ("sponsored_projects", "Sponsored projects", integer), ("research_area", "Research area", None), ("years_active", "Years active", None), ("relationship_basis", "Relationship basis", None)])
    add_table(story, "Appendix Table E3. External and cross-institutional collaboration", headers, rows, "Publication relationships use OpenAlex affiliations; sponsored-project relationships use originating and pass-through sponsor records. Missing affiliation data make publication counts conservative.", [1.55, 0.75, 0.45, 0.65, 0.6, 1.0, 0.8, 0.9], 4.9)
    add_figure(story, "Appendix Figure E1. Academic Analytics collaboration across CSU units", "output/figures/academic_analytics/figure_AA_collaboration_across_CSU_units.png", "Values are faculty-to-collaborator article links, not unique publications, and use the 21-member research-active DARE roster.")
    add_figure(story, "Appendix Figure E2. Academic Analytics external collaboration", "output/figures/academic_analytics/figure_AA_external_collaboration.png", "Values are faculty-to-collaborator article links, not unique publications, and use the 21-member research-active DARE roster.")

    data = read_csv("output/table_F1_faculty_research_awards.csv")
    headers, rows = selected(data, [("award_or_recognition", "Award or recognition", None), ("sponsoring_organization", "Sponsoring organization", None), ("recognition_level", "Level", None), ("year", "Year", year_value), ("competitive_or_elected_designation", "Designation", None)])
    add_table(story, "Appendix Table F1. Faculty research awards, fellowships, and scholarly recognitions", headers, rows, "All listed recognitions were competitive and required an application.", [1.55, 1.45, 0.65, 0.45, 1.0], 6.0)

    data = read_csv("output/table_G1_student_engagement_partial.csv")
    headers, rows = selected(data, [("year", "Year", year_value), ("student_coauthored_publications", "Student-coauthored outputs", integer), ("student_awards", "Student awards", integer), ("other_student_scholarly_products", "Other scholarly products", integer), ("coverage_note", "Coverage note", None)])
    for source_row, output_row in zip(data, rows):
        if source_row["partial_year"].lower() == "true":
            output_row[0] += " partial"
    add_table(story, "Appendix Table G1. Student participation in research and scholarly activity", headers, rows, "The table reports the student-engagement measures available for the current review. Blank or unavailable measures are omitted rather than interpreted as zero.", [0.65, 1.0, 0.75, 1.0, 2.2], 6.2)

    data = read_csv("output/table_G2_student_coauthored_outputs.csv")
    headers, rows = selected(data, [("year", "Year", year_value), ("output_type", "Output type", None), ("undergraduate_or_graduate_participation", "Student level", None), ("research_area", "Research area", None), ("title", "Title", None), ("conference_journal_or_venue", "Venue", None), ("peer_reviewed_or_juried", "Peer reviewed or juried", None), ("external_collaborator", "External collaborator", None), ("resulting_placement_award_or_outcome", "Outcome", None)])
    for source_row, output_row in zip(data, rows):
        if source_row["partial_year"].lower() == "true":
            output_row[0] += " partial"
    add_table(story, "Appendix Table G2. Student-coauthored scholarly outputs", headers, rows, "Student status combines faculty reporting with available degree records. Each row represents one documented output.", [0.45, 0.65, 0.75, 0.7, 1.65, 1.0, 0.7, 0.9, 0.9], 4.8)

    headers, rows = strategic_rows()
    add_table(story, "Appendix Table H1. Research strengths, anticipated challenges, and resource priorities", headers, rows, shares=[1.25, 1.3, 1.45, 1.2], font_size=5.8)

    doc.build(story, onFirstPage=page_decor, onLaterPages=page_decor)
    print(f"Created {OUTPUT}")


if __name__ == "__main__":
    build()
