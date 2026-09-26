# Using the policycraft Longitudinal Application

## Purpose and analytical responsibility

The application helps a policy analyst preserve temporal order,
distinguish routine variation from patterns worth investigating, and
prepare evidence for careful interpretation. A chart signal does not
identify a cause or establish that a policy worked. The analyst must
combine the pattern with data quality, institutional history,
implementation evidence, affected-community knowledge, and plausible
alternative explanations.

Launch the locally installed application with:

``` r

policycraft::launch_longitudinal()
```

Do not use `shiny::runApp("inst/app")` unless the working directory is
the package source repository.
[`launch_longitudinal()`](https://danswart.github.io/policycraft/reference/launch_longitudinal.md)
locates the installed app.

## Choose an input

The upload control accepts CSV, Excel, data-frame RDS, and validated
canonical longitudinal RDS bundles. Ordinary files enter the mapping
workflow. Canonical bundles retain their registered observation grain,
definitions, identifiers, lineage, validation results, and screening
warnings.

For ordinary files:

1.  Select the column that orders the measurements.
2.  Select the numeric value column.
3.  Declare whether the ordering column contains **Calendar dates** or
    an **Observation sequence**.
4.  Click **Apply mapping to app data**.
5.  Confirm that the status begins with `Applied:` and inspect **Data
    Table**.

The proposed mapping does not alter the working data until it is
applied. The uploaded source file is never overwritten. A sequence such
as `1:15` remains an ordinal `observation` axis; it is not presented as
dates in 1970.

## Filter and group deliberately

Filters restrict the rows shared by the analytical tabs. The grouping
selector creates distinct lines or bars from an existing column; it does
not aggregate unrelated series into a new measure. For a canonical
bundle, Run, expectation, and autocorrelation diagnostics require
exactly one existing `series_id`.

Before interpreting a chart, confirm the measure definition, unit,
reporting status, comparable coverage, missing periods, and any changes
in collection or calculation. Missing periods remain gaps and interrupt
moving-range and run calculations.

## Read the chart tabs progressively

### Run Chart

Use the Run Chart as the first view of ordered behavior. It connects the
values and adds the median. Look for direction, clustering, gaps, and
observations that deserve verification before using expectation limits.

### Line Chart

Use the Line Chart for one series or for an explicit comparison selected
in **Line/Bar Chart Grouping**. Grouped lines receive endpoint labels. A
comparison can reveal composition or divergent trajectories, but does
not itself explain the differences.

### Bar Chart

The Bar Chart summarizes the currently selected values by the chosen
grouping column. Use it for comparison, not as a substitute for the
ordered evidence.

### Untrended Expectation Chart

This chart estimates a center line (CL) and upper and lower expectation
limits (UCL and LCL) from successive moving ranges. Limits describe the
system represented by the selected observations; they are not targets,
confidence intervals, or acceptable-performance standards.

The chart reports:

- observations outside UCL or LCL;
- eight consecutive observations on one side of CL;
- six consecutive observations steadily increasing or decreasing; and
- fourteen consecutive observations alternating direction.

Only the endpoint at which a rule becomes satisfied and later qualifying
endpoints are flagged. Ties, missing values, and recalculation
boundaries interrupt the applicable sequence.

Enable recalculation when there is a defensible intervention or
system-change boundary. Select a date for calendar data or an
observation number for ordinal data. Observations before the boundary
establish the original frozen limits; observations beginning at the
boundary establish the recalculated segment.

### Trended Expectation Chart

Use a trended chart only when a sustained direction is a plausible
routine feature of the system. The fitted linear trend becomes the
center line and the limits follow it. For observation sequences, elapsed
position—not artificial days—is used. A trend model can normalize a
meaningful change, so compare it with the untrended chart and explain
why the trended form is appropriate.

## Omit points from center-line and limit calculations

Open **Exclude observations from expectation-limit calculations** below
the filters. Select one or more dates or observation numbers. Each
choice shows the observed value and source row so that you can identify
the point.

Both expectation charts update automatically. Omitted points stay on the
chart as crosses. They are left out of estimation but still checked
against the limits and included in the run rules. The Run, Line, Bar,
and Cohort charts are unchanged.

Remove a selected item to restore that point, or click **Clear
exclusions** to restore all points. Filtering keeps only selections
still visible. A new upload or applied column mapping clears the
selections. The uploaded file is not changed.

### What changes in the calculations

The untrended chart averages the included values for its center line.
Moving ranges use only adjacent included pairs. Suppose the values are
10, 12, 100, 15, and 19, and you omit 100. The new center is 14. The
moving ranges used are 2 and 4, averaging 3. The calculation does not
create a range from 12 to 15.

With recalculation enabled, each segment uses its own included
observations. The original segment needs at least two included
observations and an adjacent included pair. A later segment with fewer
than three included observations keeps the original baseline limits, as
stated in the chart caption. If enough points remain but there is no
adjacent pair, the app asks you to restore a pair.

The trended chart fits its line using included observations, then draws
the fitted line and limits across the full series. It needs at least
three included observations at two or more positions.

The optional autocorrelation modifier uses included adjacent pairs
within each segment. Missing and omitted points interrupt those pairs.
If correlation cannot be calculated, limits use the standard
moving-range calculation. When absolute correlation is at least 0.999,
limits use the existing standard-deviation fallback, calculated from
included values. The caption identifies that fallback. The
**Auto-correlation Analysis** tab continues to describe the full
filtered series.

### Check and record the selection

**Runs Debug** uses the same center lines and signal flags as the
untrended chart and identifies observations omitted from estimation.
Chart captions list the omitted positions. Image downloads retain the
markers and captions, and R downloads retain the original values and
`.limit_excluded` flags. The downloaded trended-chart code fits the
model using included observations.

Use the caption to explain why you omitted a point. The exclusion list
records what changed; it does not explain your reason. These selections
last for the current app session; they are not saved back into an
uploaded file.

These controls belong to the app. The console functions
[`expectation_chart_data()`](https://danswart.github.io/policycraft/reference/expectation_chart_data.md)
and
[`longitudinal_summary()`](https://danswart.github.io/policycraft/reference/longitudinal_summary.md)
do not yet accept an exclusion argument.

## Other diagnostic views

### Cohort and autocorrelation views

The Cohort Chart follows a selected group through successive grades and
years. The autocorrelation tab reports lagged relationships within the
ordered series. Autocorrelation can affect limit estimation, but it is
not evidence that one policy variable caused another.

### Runs Debug

Runs Debug exposes the separate same-side, monotonic-trend, alternating,
and combined flags. Use it to verify which observation triggered a rule
rather than to search repeatedly for a preferred signal.

## Export charts and analysis-ready code

PNG is convenient for routine documents, SVG for scalable editing, and
PDF for print workflows. **Download R Code** creates a compact `.R`
script that can be opened in RStudio, sourced, or pasted into a Quarto
code cell. It includes:

- a `chart_data` data frame, including exclusion flags for expectation
  charts;
- named calculations used by the chart;
- ordinary `ggplot()`, `aes()`, and `geom_*()` calls;
- the visible axis graduations;
- editable CL, UCL, and LCL annotations; and
- the shared
  [`policycraft_chart_theme()`](https://danswart.github.io/policycraft/reference/policycraft_chart_theme.md)
  call.

For example:

``` r

source("expectation_chart_2026-09-04.R")
chart

# Continue editing as a normal ggplot object.
chart + ggplot2::labs(
  title = "Revised analytical title",
  subtitle = "Define the series and period",
  caption = "Source and interpretive caution"
)
```

The generated expectation-chart limit labels default to bold text with
`size = 4`. Major and minor panel grids are removed. These are ordinary
ggplot2 arguments and may be changed in the downloaded script.

## Before sharing a chart

Before presenting a result, confirm that:

1.  the ordering and value mappings are correct and applied;
2.  filters leave the intended analytical series and period;
3.  dates or observation numbers have the intended meaning;
4.  missing values, definition changes, and reasons for exclusions are
    recorded;
5.  the selected chart form is justified;
6.  run-rule and limit signals are described as reasons to investigate;
7.  titles, units, source, and captions make the exported chart
    self-explanatory;
8.  causal language is supported by evidence beyond the chart; and
9.  the final QMD retains the editable code used to generate the figure.
