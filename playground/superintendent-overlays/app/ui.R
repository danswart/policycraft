# UI — relies on helpers/constants from global.R (chart_tab_ui, etc.)
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  # Remove default bootstrap container constraints
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "swart.css"
    )
  ),

  titlePanel("policycraft: Longitudinal Policy Analysis"),

  tags$div(
    class = "alert alert-primary",
    tags$strong("Observe → Reason → Insight. "),
    "Use ordered evidence and analytical tools to develop carefully qualified, decision-useful policy insight. Charts identify patterns worth investigating; they do not establish causes or make policy recommendations."
  ),

  # Row 1: File Upload and Basic Controls
  fluidRow(
    column(
      3,
      fileInput(
        "file",
        "Upload CSV, Excel, or RDS File",
        accept = c(".csv", ".xlsx", ".xls", ".rds")
      )
    ),
    column(2, checkboxInput("header", "Header in first row", value = TRUE)),
    column(
      2,
      selectInput(
        "rows_display",
        "Rows to display:",
        choices = c(30, 40, 50),
        selected = 30
      )
    ),
    column(
      2,
      checkboxInput(
        "format_as_percentage",
        "Y-axis: Show as Percentage (85.2%)",
        value = FALSE
      ),
      tags$small(
        "Check if values are rates/percentages, uncheck for scores/counts",
        style = "color: #6c757d;"
      )
    ),
    column(
      3,
      conditionalPanel(
        condition = "output.data_uploaded",
        selectInput(
          "grouping_var",
          "Line/Bar Chart Grouping:",
          choices = NULL,
          selected = NULL
        ),
        tags$small(
          "Column to group/label lines and bars",
          style = "color: #6c757d;"
        )
      )
    )
  ),

  # Row 2: Filtering Controls
  fluidRow(
    column(
      12,
      div(
        id = "filter_controls",
        style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0px; border-radius: 5px;"
      )
    )
  ),

  # Row 3: Tab Panel for Data Table and Charts
  fluidRow(
    column(
      12,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Column Mapping",
          br(),
          conditionalPanel(
            condition = "output.data_uploaded",
            div(
              style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6; margin-bottom: 15px;",
              h4("Map Date and Value Columns"),
              p(
                "This app needs a Date column first and a Value column second. Pick which uploaded columns those are below — they will be renamed to \"date\" and \"value\" and moved to the front. Every other column is kept as-is. Columns are pre-selected with the app's best guess; change them if that guess is wrong."
              ),
              fluidRow(
                column(4, selectInput("map_date_col", "Date column:", choices = NULL)),
                column(4, selectInput("map_value_col", "Value column:", choices = NULL)),
                column(
                  4,
                  tags$div(
                    style = "margin-top: 26px; font-weight: 600;",
                    textOutput("map_status")
                  )
                )
              )
            ),
            h4("Preview of uploaded file (first 10 rows, before mapping)"),
            DT::dataTableOutput("mapping_preview_table")
          ),
          conditionalPanel(
            condition = "!output.data_uploaded",
            tags$p(
              "Upload a CSV, Excel, or RDS file above to choose your Date and Value columns.",
              style = "color: #6c757d; margin-top: 15px;"
            )
          )
        ),
        tabPanel(
          "Data Table",
          br(),
          DT::dataTableOutput("data_table", width = "100%")
        ),
        chart_tab_ui("Run Chart", "date_info", "run", "Run Chart"),
        chart_tab_ui("Line Chart", "grouping_info", "line", "Line Chart"),
        chart_tab_ui("Bar Chart", "bar_info", "bar", "Bar Chart"),
        chart_tab_ui(
          "Untrended Expectation Chart", "control_info", "control", "Expectation Chart",
          extra_controls = conditionalPanel(
            condition = "output.data_uploaded",
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
              fluidRow(
                column(
                  3,
                  checkboxInput(
                    "enable_recalc",
                    "Enable Recalculation",
                    value = FALSE
                  )
                ),
                column(
                  4,
                  conditionalPanel(
                    condition = "input.enable_recalc",
                    dateInput(
                      "recalc_date",
                      "Recalculate limits starting from:",
                      value = NULL,
                      min = NULL,
                      max = NULL
                    )
                  )
                ),
                column(
                  5,
                  conditionalPanel(
                    condition = "input.enable_recalc",
                    tags$small(
                      "Select date to split process and recalculate expectation limits",
                      style = "color: #6c757d; margin-top: 5px; display: block;"
                    )
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  3,
                  checkboxInput(
                    "use_autocorr_modifier",
                    "Use Auto-correlation Modifier",
                    value = FALSE
                  )
                ),
                column(
                  9,
                  conditionalPanel(
                    condition = "input.use_autocorr_modifier",
                    tags$small(
                      "Adjusts control limits using: σ = R-bar / (d2 × √(1 - r²)), where R-bar is avg moving range, d2 = 1.128, and r is the lag-1 sample correlation. This widens limits relative to the standard moving range method (σ = R-bar / d2) when autocorrelation is present.",
                      style = "color: #6c757d; margin-top: 5px; display: block;"
                    )
                  )
                )
              )
            )
          )
        ),
        chart_tab_ui("Trended Expectation Chart", "trended_info", "trended", "Trended Expectation Chart"),
        chart_tab_ui(
          "Cohort Chart", "cohort_info", "cohort", "Cohort Analysis",
          extra_controls = conditionalPanel(
            condition = "output.data_uploaded",
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
              fluidRow(
                column(
                  2,
                  selectInput(
                    "cohort_grade_var",
                    "Grade Column:",
                    choices = NULL,
                    selected = NULL
                  )
                ),
                column(
                  2,
                  numericInput(
                    "cohort_start_grade",
                    "Starting Grade:",
                    value = 3,
                    min = 1,
                    max = 12,
                    step = 1
                  )
                ),
                column(
                  2,
                  numericInput(
                    "cohort_end_grade",
                    "Ending Grade:",
                    value = 8,
                    min = 1,
                    max = 12,
                    step = 1
                  )
                ),
                column(
                  3,
                  numericInput(
                    "cohort_start_year",
                    "Starting Year:",
                    value = 2018,
                    min = 1900,
                    max = 2030,
                    step = 1
                  )
                ),
                column(
                  3,
                  numericInput(
                    "cohort_end_year",
                    "Ending Year:",
                    value = 2023,
                    min = 1900,
                    max = 2030,
                    step = 1
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  12,
                  tags$small(
                    "Track a demographic cohort's progression through grades over consecutive years. Use filtering controls above to select your cohort (e.g., Asian, Reading).",
                    style = "color: #6c757d;"
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Auto-correlation Analysis",
          br(),
          fluidRow(
            column(12, uiOutput("autocorr_info"))
          ),
          br(),
          fluidRow(
            column(
              12,
              h4("Auto-correlation Coefficients and Sample Correlation"),
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                tags$p(
                  style = "font-weight: bold; color: #0066cc;",
                  "This analysis shows BOTH autocorrelation coefficients (ACF) and sample correlation coefficients (r) for lag-1, lag-2, and lag-3."
                ),
                tags$p(
                  "The Autocorrelation Coefficient (ACF) measures correlation normalized by the overall variance of the series."
                ),
                tags$p(
                  "The Sample Correlation Coefficient (r) is the Pearson correlation coefficient calculated from the paired lag-k observations."
                ),
                tags$p(
                  style = "color: #666;",
                  "Values close to 0 indicate no correlation. Values close to ±1 indicate strong correlation."
                )
              ),
              br(),
              DT::dataTableOutput("autocorr_table")
            )
          )
        ),
        tabPanel(
          "Runs Debug",
          br(),
          fluidRow(
            column(
              12,
              h4("Runs Analysis Debug Information"),
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                tags$p(
                  style = "font-weight: bold; color: #0066cc;",
                  "This tab shows detailed runs analysis for debugging."
                ),
                tags$p(
                  "The runs rule detects 8+ consecutive points on the same side of the centerline."
                ),
                tags$p(
                  style = "color: #666;",
                  "Points marked TRUE have triggered a runs signal (8th point onward in each run)."
                )
              ),
              br(),
              verbatimTextOutput("runs_debug_info"),
              br(),
              h4("Runs Analysis Data Table"),
              DT::dataTableOutput("runs_debug_table")
            )
          )
        )
      )
    )
  )
)
