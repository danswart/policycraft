# Server — relies on helpers/constants from global.R (dashboard_chart_theme,
# empty_chart_message, guess_date_value_columns, safe_numeric, etc.)
server <- function(input, output, session) {
  uploaded_object <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    if (ext == "csv") {
      readr::read_csv(input$file$datapath, col_names = input$header, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(input$file$datapath, col_names = input$header)
    } else if (ext == "rds") {
      readRDS(input$file$datapath)
    } else {
      validate(need(FALSE, "Please upload a .csv, .xlsx, .xls, or .rds file."))
    }
  })

  canonical_bundle <- reactive({
    object <- uploaded_object()
    is_candidate <- is.list(object) && !is.data.frame(object) && !is.null(names(object)) &&
      any(c("schema_version", "observations", "measures", "derivations", "lineage",
            "screening_report", "validation_results") %in% names(object))
    if (!is_candidate) return(NULL)
    tryCatch(
      validate_canonical_bundle(object),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })

  output$is_canonical <- reactive(!is.null(canonical_bundle()))
  outputOptions(output, "is_canonical", suspendWhenHidden = FALSE)

  output$canonical_summary <- renderUI({
    bundle <- canonical_bundle()
    rows <- filtered_data()
    req(bundle, rows)
    series_ids <- unique(as.character(rows$series_id[!is.na(rows$series_id)]))
    definitions <- list_canonical_series(bundle)
    definition <- definitions[definitions$series_id %in% series_ids, , drop = FALSE]
    field <- function(name, fallback = "Not supplied") {
      if (length(series_ids) != 1L || !name %in% names(definition) || !nrow(definition) ||
          is.na(definition[[name]][1]) || !nzchar(as.character(definition[[name]][1]))) {
        fallback
      } else as.character(definition[[name]][1])
    }
    tags$div(
      class = "alert alert-info",
      tags$h4("Canonical explorer"),
      tags$p(paste0(nrow(rows), " filtered observation(s) across ", length(series_ids), " series.")),
      if (length(series_ids) == 1L) tagList(
        tags$p(tags$strong("Selected series: "), series_ids),
        tags$p(paste0(
          "Entity: ", field("entity_name", field("entity_id")),
          " | Geographic level: ", field("geographic_level"),
          " | Subject: ", field("subject"),
          " | Tested grade: ", field("tested_grade"),
          " | Student group: ", field("student_group"),
          " | Unit: ", field("unit")
        )),
        tags$p(paste0("Valid period: ", field("valid_start"), " through ", field("valid_end"))),
        tags$p(tags$strong("Measure definition: "), field("meaning")),
        tags$p(tags$strong("Derivation formula: "), field("definition", "Reported value; no derivation supplied."))
      ) else tags$p("Filter `series_id` to exactly one series before using Run, expectation, or autocorrelation diagnostics."),
      tags$p("Filters and charts inspect existing canonical observations only; policycraft does not create new totals, averages, or ratios.")
    )
  })

  output$canonical_status_table <- DT::renderDataTable({
    rows <- filtered_data()
    columns <- intersect(c("observation_id", "observation_date", "value", "unit", "reporting_status", "source_id", "source_record_id", "source_field", "source_value", "derivation_id"), names(rows))
    DT::datatable(rows[columns], options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$canonical_measure_table <- DT::renderDataTable({
    bundle <- canonical_bundle()
    rows <- filtered_data()
    data <- bundle$measures[bundle$measures$measure_id %in% unique(rows$measure_id), , drop = FALSE]
    DT::datatable(data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$canonical_derivation_table <- DT::renderDataTable({
    bundle <- canonical_bundle()
    rows <- filtered_data()
    ids <- unique(as.character(rows$derivation_id[!is.na(rows$derivation_id)]))
    data <- bundle$derivations[bundle$derivations$derivation_id %in% ids, , drop = FALSE]
    DT::datatable(data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$canonical_lineage_table <- DT::renderDataTable({
    bundle <- canonical_bundle()
    rows <- filtered_data()
    lineage <- bundle$lineage[bundle$lineage$derived_observation_id %in% rows$observation_id, , drop = FALSE]
    source_rows <- bundle$observations[bundle$observations$observation_id %in% lineage$source_observation_id, , drop = FALSE]
    if (nrow(lineage) && nrow(source_rows)) {
      source <- source_rows
      names(source) <- paste0("source_", names(source))
      lineage <- dplyr::left_join(lineage, source, by = c("source_observation_id" = "source_observation_id"))
    }
    DT::datatable(lineage, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$canonical_validation_table <- DT::renderDataTable({
    DT::datatable(canonical_bundle()$validation_results, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$canonical_screening_table <- DT::renderDataTable({
    DT::datatable(canonical_bundle()$screening_report, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Reads the uploaded file as-is (original column names, no date/value
  # standardization). This is what the Column Mapping tab shows and lets the
  # user pick a Date column and a Value column from.
  file_data <- reactive({
    req(input$file)

    # Use the original uploaded filename for extension detection.
    # input$file$datapath is a temporary file path and may not keep the extension.
    object <- uploaded_object()
    data <- if (!is.null(canonical_bundle())) {
      canonical_bundle()$observations
    } else if (is.data.frame(object)) {
      object
    } else {
      extract_data_frame_from_rds(object)
    }

    if (!is.null(data)) {
      data <- tibble::as_tibble(data)
      data <- detect_and_convert_dates(data)
    }

    data
  })

  applied_column_mapping <- reactiveVal(NULL)

  # Whenever a new file is read, pre-select and initialize the mapping with the
  # app's best guess. Later dropdown changes remain proposals until applied.
  observeEvent(file_data(), {
    data <- file_data()
    req(data)

    if (!is.null(canonical_bundle())) return()

    guesses <- guess_date_value_columns(data)
    cols <- names(data)
    guessed_order_type <- if (all(is.na(standardize_to_date(data[[guesses$date_col]])))) {
      "sequence"
    } else {
      "date"
    }

    updateSelectInput(session, "map_date_col", choices = cols, selected = guesses$date_col)
    updateSelectInput(session, "map_value_col", choices = cols, selected = guesses$value_col)
    updateRadioButtons(session, "map_order_type", selected = guessed_order_type)
    applied_column_mapping(list(
      date_col = guesses$date_col,
      value_col = guesses$value_col,
      order_type = guessed_order_type
    ))
  })

  observeEvent(input$apply_column_mapping, {
    data <- file_data()
    req(data)

    date_col <- input$map_date_col
    value_col <- input$map_value_col
    order_type <- input$map_order_type
    if (
      is.null(date_col) || is.null(value_col) ||
        !date_col %in% names(data) || !value_col %in% names(data)
    ) {
      showNotification("Choose a valid Date column and Value column first.", type = "error")
      return()
    }
    if (identical(date_col, value_col)) {
      showNotification("Date and Value must use different columns.", type = "error")
      return()
    }
    if (identical(order_type, "date") && all(is.na(standardize_to_date(data[[date_col]])))) {
      showNotification(
        "The selected order column contains no recognizable calendar dates. Choose Observation sequence or select another column.",
        type = "error"
      )
      return()
    }

    applied_column_mapping(list(
      date_col = date_col,
      value_col = value_col,
      order_type = order_type
    ))
    showNotification("Column mapping applied to the app's working data.", type = "message")
  })

  output$mapping_preview_table <- DT::renderDataTable({
    data <- file_data()
    req(data)

    DT::datatable(
      utils::head(data, 10),
      options = list(dom = 't', scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$map_status <- renderText({
    data <- file_data()
    req(data)

    date_col <- input$map_date_col
    value_col <- input$map_value_col
    if (is.null(date_col) || is.null(value_col) || !date_col %in% names(data) || !value_col %in% names(data)) {
      return("Waiting for column selection...")
    }
    if (identical(date_col, value_col)) {
      return("Observation/Date and Value must be different columns.")
    }

    order_type <- input$map_order_type
    axis_name <- if (identical(order_type, "sequence")) "observation" else "date"

    applied <- applied_column_mapping()
    if (
      is.null(applied) ||
        !identical(date_col, applied$date_col) ||
        !identical(value_col, applied$value_col) ||
        !identical(order_type, applied$order_type)
    ) {
      return(paste0(
        "Proposed: '", date_col, "' -> ", axis_name, " | '", value_col,
        "' -> value. Click Apply mapping to use it."
      ))
    }

    paste0("Applied: '", date_col, "' -> ", axis_name, " | '", value_col, "' -> value")
  })

  # Data with the user's (or the auto-guessed) Date/Value column mapping applied.
  # The rest of the app relies on this having "date" and "value" as its first
  # two columns, so every other reactive downstream keeps calling raw_data().
  raw_data <- reactive({
    data <- file_data()
    req(data)

    if (!is.null(canonical_bundle())) {
      data$date <- data$observation_date
      data$year <- lubridate::year(data$date)
      return(tibble::as_tibble(data))
    }

    mapping <- applied_column_mapping()
    date_override <- mapping$date_col
    value_override <- mapping$value_col
    if (is.null(date_override) || !date_override %in% names(data)) {
      date_override <- NULL
    }
    if (is.null(value_override) || !value_override %in% names(data) || identical(value_override, date_override)) {
      value_override <- NULL
    }

    add_chart_columns(
      data,
      date_col_override = date_override,
      value_col_override = value_override,
      order_type = mapping$order_type
    )
  })

  # Output to control conditional panel visibility
  output$data_uploaded <- reactive({
    return(!is.null(raw_data()))
  })
  outputOptions(output, 'data_uploaded', suspendWhenHidden = FALSE)

  output$uses_observation_axis <- reactive({
    data <- raw_data()
    !is.null(data) && "observation" %in% names(data)
  })
  outputOptions(output, 'uses_observation_axis', suspendWhenHidden = FALSE)

  # Reactive for available grouping columns
  grouping_choices <- reactive({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }
    mapping <- applied_column_mapping()
    mapped_source_columns <- if (is.null(mapping)) {
      character(0)
    } else {
      c(mapping$date_col, mapping$value_col)
    }
    # Exclude date and value columns
    value_patterns <- c(
      "^value$",
      "^pct$",
      "^percent$",
      "^amount$",
      "^count$"
    )
    value_pattern <- paste(value_patterns, collapse = "|")
    potential_groups <- names(data)[
      !names(data) %in% c("date") &
        !grepl(value_pattern, names(data), ignore.case = TRUE)
    ]
    potential_groups <- setdiff(potential_groups, mapped_source_columns)
    potential_groups <- potential_groups[vapply(
      potential_groups,
      function(column) any(!is.na(data[[column]])),
      logical(1)
    )]

    # Put "series" at the top when wide data was auto-pivoted (most common
    # multi-line use case), otherwise fall back to Year as before.
    if ("series" %in% potential_groups) {
      potential_groups <- c("series", base::setdiff(potential_groups, "series"))
    } else if ("series_id" %in% potential_groups) {
      potential_groups <- c("series_id", base::setdiff(potential_groups, "series_id"))
    } else if ("Year" %in% potential_groups) {
      potential_groups <- c("Year", setdiff(potential_groups, "Year"))
    }

    return(potential_groups)
  })

  # Reactive for cohort grade column choices
  cohort_grade_choices <- reactive({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }

    # Look for grade-like columns
    grade_patterns <- c("grade", "level", "year")
    potential_grades <- names(data)

    # Prioritize columns with grade-like names
    grade_like <- potential_grades[grepl(
      paste(grade_patterns, collapse = "|"),
      potential_grades,
      ignore.case = TRUE
    )]

    # If no grade-like columns found, show all non-date/value columns
    if (length(grade_like) == 0) {
      value_patterns <- c(
        "^value$",
        "^pct$",
        "^percent$",
        "^amount$",
        "^count$"
      )
      value_pattern <- paste(value_patterns, collapse = "|")
      grade_like <- names(data)[
        !names(data) %in% c("date") &
          !grepl(value_pattern, names(data), ignore.case = TRUE)
      ]
    }

    return(grade_like)
  })

  # ENHANCED: Reactive for auto-correlation analysis with BOTH ACF and Sample Correlation
  autocorr_data <- reactive({
    data <- analytical_data()
    if (is.null(data) || nrow(data) == 0 || !"value" %in% names(data)) {
      return(data.frame(
        Lag = c("Lag-1", "Lag-2", "Lag-3"),
        Autocorrelation_ACF = c(NA, NA, NA),
        Sample_Correlation_r = c(NA, NA, NA),
        ACF_Interpretation = c("No data", "No data", "No data"),
        r_Interpretation = c("No data", "No data", "No data"),
        stringsAsFactors = FALSE
      ))
    }

    series_problem <- expectation_series_problem(data)
    validate(need(is.null(series_problem), series_problem))

    # Remove rows with NA values and sort by date
    data <- data[!is.na(data$date) & !is.na(data$value), ]
    data <- data[order(data$date), ]

    if (nrow(data) < 4) {
      return(data.frame(
        Lag = c("Lag-1", "Lag-2", "Lag-3"),
        Autocorrelation_ACF = c(NA, NA, NA),
        Sample_Correlation_r = c(NA, NA, NA),
        ACF_Interpretation = c(
          "Insufficient data",
          "Insufficient data",
          "Insufficient data"
        ),
        r_Interpretation = c(
          "Insufficient data",
          "Insufficient data",
          "Insufficient data"
        ),
        stringsAsFactors = FALSE
      ))
    }

    # Calculate BOTH coefficients for each lag
    lag1_results <- safe_autocorr(data$value, lag = 1)
    lag2_results <- safe_autocorr(data$value, lag = 2)
    lag3_results <- safe_autocorr(data$value, lag = 3)

    # Create interpretation based on absolute value of correlation
    interpret_corr <- function(coeff) {
      if (is.na(coeff)) {
        return("Cannot calculate")
      }
      abs_coeff <- abs(coeff)
      if (abs_coeff < 0.1) {
        return("Very weak correlation")
      }
      if (abs_coeff < 0.3) {
        return("Weak correlation")
      }
      if (abs_coeff < 0.5) {
        return("Moderate correlation")
      }
      if (abs_coeff < 0.7) {
        return("Strong correlation")
      }
      return("Very strong correlation")
    }

    autocorr_result <- data.frame(
      Lag = c("Lag-1", "Lag-2", "Lag-3"),
      Autocorrelation_ACF = round(
        c(lag1_results$acf, lag2_results$acf, lag3_results$acf),
        4
      ),
      Sample_Correlation_r = round(
        c(lag1_results$r, lag2_results$r, lag3_results$r),
        4
      ),
      ACF_Interpretation = c(
        interpret_corr(lag1_results$acf),
        interpret_corr(lag2_results$acf),
        interpret_corr(lag3_results$acf)
      ),
      r_Interpretation = c(
        interpret_corr(lag1_results$r),
        interpret_corr(lag2_results$r),
        interpret_corr(lag3_results$r)
      ),
      stringsAsFactors = FALSE
    )

    return(autocorr_result)
  })

  # NEW: Reactive values to store correlation coefficients for use in control limit calculations
  correlation_values <- reactive({
    autocorr_data_result <- autocorr_data()

    # Extract the Sample Correlation Coefficients for future use
    list(
      r_lag1 = if (nrow(autocorr_data_result) >= 1) {
        autocorr_data_result$Sample_Correlation_r[1]
      } else {
        NA
      },
      r_lag2 = if (nrow(autocorr_data_result) >= 2) {
        autocorr_data_result$Sample_Correlation_r[2]
      } else {
        NA
      },
      r_lag3 = if (nrow(autocorr_data_result) >= 3) {
        autocorr_data_result$Sample_Correlation_r[3]
      } else {
        NA
      },
      acf_lag1 = if (nrow(autocorr_data_result) >= 1) {
        autocorr_data_result$Autocorrelation_ACF[1]
      } else {
        NA
      },
      acf_lag2 = if (nrow(autocorr_data_result) >= 2) {
        autocorr_data_result$Autocorrelation_ACF[2]
      } else {
        NA
      },
      acf_lag3 = if (nrow(autocorr_data_result) >= 3) {
        autocorr_data_result$Autocorrelation_ACF[3]
      } else {
        NA
      }
    )
  })

  # Update recalculation date input range when data changes
  observeEvent(raw_data(), {
    data <- raw_data()
    if (!is.null(data) && "date" %in% names(data)) {
      date_range <- range(data$date, na.rm = TRUE)
      # Set default to middle of date range, with available range
      default_date <- date_range[1] + as.numeric(diff(date_range)) * 0.6
      updateDateInput(
        session,
        "recalc_date",
        value = default_date,
        min = date_range[1],
        max = date_range[2]
      )
    }
    if (!is.null(data) && "observation" %in% names(data)) {
      observations <- unique(data$observation[!is.na(data$observation)])
      observations <- sort(observations)
      default_index <- max(1L, ceiling(length(observations) * 0.6))
      updateSelectInput(
        session,
        "recalc_observation",
        choices = observations,
        selected = observations[default_index]
      )
    }
  })

  # Update grouping variable choices when data changes
  observeEvent(grouping_choices(), {
    choices <- grouping_choices()
    if (!is.null(choices) && length(choices) > 0) {
      updateSelectInput(
        session,
        "grouping_var",
        choices = c("No grouping" = "", setNames(choices, choices)),
        selected = ""
      )
    } else {
      updateSelectInput(
        session,
        "grouping_var",
        choices = c("No grouping columns available" = ""),
        selected = ""
      )
    }
  })

  # Update cohort grade variable choices when data changes
  observeEvent(cohort_grade_choices(), {
    choices <- cohort_grade_choices()
    if (!is.null(choices) && length(choices) > 0) {
      # Try to find grade_level_code first, then grade_level_name, then first available
      preferred_selection <- if ("grade_level_code" %in% choices) {
        "grade_level_code"
      } else if ("grade_level_name" %in% choices) {
        "grade_level_name"
      } else {
        choices[1]
      }

      updateSelectInput(
        session,
        "cohort_grade_var",
        choices = setNames(choices, choices),
        selected = preferred_selection
      )
    } else {
      updateSelectInput(
        session,
        "cohort_grade_var",
        choices = c("No grade columns available" = ""),
        selected = ""
      )
    }
  })

  # Display date range information (Run Chart tab)
  output$date_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }

    if ("observation" %in% names(data)) {
      observation_range <- range(data$observation, na.rm = TRUE)
      return(div(
        style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #4682b4;",
        HTML(paste0(
          "<strong>Observation Range:</strong> ",
          observation_range[1], " to ", observation_range[2],
          " <em>(", nrow(data), " rows, ordinal x-axis)</em>"
        ))
      ))
    }

    if (!"date" %in% names(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
    format_used <- if (all_jan_first) {
      "Years only"
    } else {
      "Full dates (YYYY-MM-DD)"
    }

    div(
      style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #4682b4;",
      HTML(paste0(
        "<strong>Date Range:</strong> ",
        format(date_range[1], "%B %d, %Y"),
        " to ",
        format(date_range[2], "%B %d, %Y"),
        " <em>(",
        nrow(data),
        " rows, ",
        format_used,
        " on x-axis)</em>"
      ))
    )
  })

  # Display grouping variable information (Line Chart tab)
  output$grouping_info <- renderUI({
    data <- filtered_data()
    if (is.null(data)) {
      return(NULL)
    }

    if (nrow(data) == 0L) {
      return(div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> No rows remain after applying the current filters.")
      ))
    }

    if (
      is.null(input$grouping_var) ||
        input$grouping_var == "" ||
        !input$grouping_var %in% names(data)
    ) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML(
          "<strong>Note:</strong> No valid grouping variable selected. Please select a column from the 'Line/Bar Chart Grouping' dropdown above to create multiple lines."
        )
      )
    } else {
      group_var <- input$grouping_var
      unique_groups <- unique(data[[group_var]][!is.na(data[[group_var]])])

      div(
        style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; border: 1px solid #81c784;",
        HTML(paste0(
          "<strong>Grouping Variable:</strong> '",
          group_var,
          "' with ",
          length(unique_groups),
          " categories: ",
          paste(head(unique_groups, 5), collapse = ", "),
          if (length(unique_groups) > 5) "..." else "",
          " <em>(Labels will appear at end of each line)</em>"
        ))
      )
    }
  })

  # Display bar chart information (Bar Chart tab)
  output$bar_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }

    if (
      is.null(input$grouping_var) ||
        input$grouping_var == "" ||
        !input$grouping_var %in% names(data)
    ) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML(
          "<strong>Note:</strong> No valid grouping variable selected. Please select a column from the 'Line/Bar Chart Grouping' dropdown above to create bars."
        )
      )
    } else {
      group_var <- input$grouping_var
      unique_groups <- unique(data[[group_var]])
      date_range <- range(data$date, na.rm = TRUE)

      div(
        style = "background-color: #e7f3ff; padding: 10px; border-radius: 5px; border: 1px solid #7fb3d3;",
        HTML(paste0(
          "<strong>Bar Chart Data:</strong> Showing averages across all filtered dates (",
          format(date_range[1], "%B %d, %Y"),
          " to ",
          format(date_range[2], "%B %d, %Y"),
          ") with ",
          length(unique_groups),
          " categories: ",
          paste(head(unique_groups, 5), collapse = ", "),
          if (length(unique_groups) > 5) "..." else ""
        ))
      )
    }
  })

  # Display trended expectation chart information (Trended Expectation Chart tab)
  output$trended_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }

    if ("observation" %in% names(data)) {
      span <- range(data$observation, na.rm = TRUE)
      range_text <- paste0("observations ", span[1], " to ", span[2])
    } else {
      date_range <- range(data$date, na.rm = TRUE)
      range_text <- paste0(
        format(date_range[1], "%B %d, %Y"), " to ",
        format(date_range[2], "%B %d, %Y")
      )
    }

    div(
      style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; border: 1px solid #4caf50;",
      HTML(paste0(
        "<strong>Trended Expectation Chart:</strong> Linear trend-adjusted expectation chart showing process capability from ",
        range_text,
        " <em>(Points colored by sigma signals, centerline shows trend, includes runs analysis)</em>"
      ))
    )
  })

  # Display expectation chart information (Expectation Chart tab)
  output$control_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }

    if ("observation" %in% names(data)) {
      span <- range(data$observation, na.rm = TRUE)
      range_text <- paste0("observations ", span[1], " to ", span[2])
    } else {
      date_range <- range(data$date, na.rm = TRUE)
      range_text <- paste0(
        format(date_range[1], "%B %d, %Y"), " to ",
        format(date_range[2], "%B %d, %Y")
      )
    }

    div(
      style = "background-color: #fff8e1; padding: 10px; border-radius: 5px; border: 1px solid #ffa726;",
      HTML(paste0(
        "<strong>Expectation Chart:</strong> Untrended expectation chart showing process capability from ",
        range_text,
        " <em>(Points colored by sigma signals, center line shows runs signals)</em>"
      ))
    )
  })

  output$cohort_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) {
      return(NULL)
    }

    if (
      is.null(input$cohort_grade_var) ||
        input$cohort_grade_var == "" ||
        !input$cohort_grade_var %in% names(data) ||
        is.null(input$cohort_start_grade) ||
        is.null(input$cohort_end_grade) ||
        is.null(input$cohort_start_year) ||
        is.null(input$cohort_end_year)
    ) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML(
          "<strong>Note:</strong> Please select a grade column and specify grade range and year range for cohort tracking."
        )
      )
    } else {
      grade_var <- input$cohort_grade_var
      start_grade <- input$cohort_start_grade
      end_grade <- input$cohort_end_grade
      start_year <- input$cohort_start_year
      end_year <- input$cohort_end_year

      years_span <- end_year - start_year + 1
      grades_span <- end_grade - start_grade + 1

      div(
        style = "background-color: #f3e5f5; padding: 10px; border-radius: 5px; border: 1px solid #ba68c8;",
        HTML(paste0(
          "<strong>Educational Cohort Tracking:</strong> Using '",
          grade_var,
          "' column, following cohort progression from Grade ",
          start_grade,
          " to Grade ",
          end_grade,
          " over ",
          years_span,
          " years (",
          start_year,
          "-",
          end_year,
          "). ",
          "<em>Use filters above to select your demographic cohort (ethnicity, subject, etc.)</em>"
        ))
      )
    }
  })

  # ENHANCED: Display auto-correlation information with BOTH correlation types
  output$autocorr_info <- renderUI({
    data <- filtered_data()
    if (is.null(data) || !"value" %in% names(data)) {
      return(NULL)
    }

    # Remove rows with NA values and get valid count
    data_clean <- data[!is.na(data$date) & !is.na(data$value), ]
    data_points <- nrow(data_clean)
    date_range <- range(data_clean$date, na.rm = TRUE)

    # Get correlation values for display
    corr_vals <- correlation_values()

    div(
      style = "background-color: #e8f4fd; padding: 10px; border-radius: 5px; border: 1px solid #3498db;",
      HTML(paste0(
        "<strong>Auto-correlation Analysis:</strong> Analyzing temporal correlation patterns in your data from ",
        format(date_range[1], "%B %d, %Y"),
        " to ",
        format(date_range[2], "%B %d, %Y"),
        " <em>(",
        data_points,
        " data points after applying filters)</em><br>",
        "<strong>Stored ACF Values:</strong> ACF₁ = ",
        if (!is.na(corr_vals$acf_lag1)) round(corr_vals$acf_lag1, 4) else "NA",
        ", ACF₂ = ",
        if (!is.na(corr_vals$acf_lag2)) round(corr_vals$acf_lag2, 4) else "NA",
        ", ACF₃ = ",
        if (!is.na(corr_vals$acf_lag3)) round(corr_vals$acf_lag3, 4) else "NA",
        "<br>",
        "<strong>Stored r Values:</strong> r₁ = ",
        if (!is.na(corr_vals$r_lag1)) round(corr_vals$r_lag1, 4) else "NA",
        ", r₂ = ",
        if (!is.na(corr_vals$r_lag2)) round(corr_vals$r_lag2, 4) else "NA",
        ", r₃ = ",
        if (!is.na(corr_vals$r_lag3)) round(corr_vals$r_lag3, 4) else "NA",
        " <em>(Available for control limit calculations)</em>"
      ))
    )
  })

  # NEW: Runs debug information
  output$runs_debug_info <- renderText({
    tryCatch(
      {
        data <- filtered_data()
        if (is.null(data) || nrow(data) == 0) {
          return("No data available for runs analysis.")
        }

        # Remove NA values
        data <- data[!is.na(data$date) & !is.na(data$value), ]
        if (nrow(data) == 0) {
          return("No valid data after removing NA values.")
        }

        # Determine if recalculation is enabled
        recalc_enabled <- !is.null(input$enable_recalc) &&
          input$enable_recalc &&
          !is.null(input$recalc_date) &&
          input$recalc_date >= min(data$date, na.rm = TRUE) &&
          input$recalc_date <= max(data$date, na.rm = TRUE)

        if (recalc_enabled) {
          # Recalculation mode analysis
          recalc_date <- input$recalc_date
          data_before <- data[data$date < recalc_date, ]
          data_after <- data[data$date >= recalc_date, ]

          emp_cl_orig <- safe_mean(data_before$value)
          emp_cl_recalc <- if (nrow(data_after) >= 3) {
            safe_mean(data_after$value)
          } else {
            emp_cl_orig
          }

          # Analyze runs for each segment
          before_above <- if (nrow(data_before) > 0) {
            safe_numeric(data_before$value) > emp_cl_orig
          } else {
            c()
          }
          after_above <- if (nrow(data_after) > 0) {
            safe_numeric(data_after$value) > emp_cl_recalc
          } else {
            c()
          }

          # Find consecutive runs in each segment
          before_runs <- if (length(before_above) > 0) {
            rle(before_above[!is.na(before_above)])$lengths
          } else {
            c()
          }
          after_runs <- if (length(after_above) > 0) {
            rle(after_above[!is.na(after_above)])$lengths
          } else {
            c()
          }

          before_long_runs <- before_runs[before_runs >= 8]
          after_long_runs <- after_runs[after_runs >= 8]
          debug_rules <- detect_run_rule_signals(
            data$value,
            ifelse(data$date < recalc_date, emp_cl_orig, emp_cl_recalc),
            same_side_signals = detect_runs_signals_recalc(
              data$value, emp_cl_orig, emp_cl_recalc, data$date, recalc_date
            ),
            segment = data$date >= recalc_date
          )

          paste(
            "=== FIXED RUNS ANALYSIS (RECALCULATION MODE) ===",
            paste("Total data points:", nrow(data)),
            paste("Recalculation date:", format(recalc_date, "%Y-%m-%d")),
            paste("Points before recalc:", nrow(data_before)),
            paste("Points after recalc:", nrow(data_after)),
            "",
            "=== BEFORE RECALC SEGMENT ===",
            paste("Centerline (original):", round(emp_cl_orig, 3)),
            paste("All run lengths:", paste(before_runs, collapse = ", ")),
            paste(
              "Runs of 8+ points:",
              paste(before_long_runs, collapse = ", ")
            ),
            paste("Runs signals detected:", length(before_long_runs) > 0),
            "",
            "=== AFTER RECALC SEGMENT ===",
            paste("Centerline (recalculated):", round(emp_cl_recalc, 3)),
            paste("All run lengths:", paste(after_runs, collapse = ", ")),
            paste(
              "Runs of 8+ points:",
              paste(after_long_runs, collapse = ", ")
            ),
            paste("Runs signals detected:", length(after_long_runs) > 0),
            "",
            "=== COMBINED RUN-RULE RESULT ===",
            run_rule_summary(debug_rules),
            paste("Six-point trend endpoints:", paste(which(debug_rules$trend_6), collapse = ", ")),
            paste("Fourteen-point alternating endpoints:", paste(which(debug_rules$alternating_14), collapse = ", ")),
            "",
            "=== IMPROVEMENT NOTES ===",
            "• Each segment analyzed separately with appropriate centerline",
            "• No artificial breaks at recalculation boundary",
            "• Proper run length encoding (rle) used for detection",
            "• Only 8th+ points in each run marked as signals",
            sep = "\n"
          )
        } else {
          # Standard mode analysis
          emp_cl <- safe_mean(data$value)
          above_cl <- safe_numeric(data$value) > emp_cl
          above_cl_clean <- above_cl[!is.na(above_cl)]

          runs <- rle(above_cl_clean)$lengths
          long_runs <- runs[runs >= 8]

          # Find actual runs details
          run_details <- rle(above_cl_clean)
          above_runs <- run_details$lengths[run_details$values == TRUE]
          below_runs <- run_details$lengths[run_details$values == FALSE]
          debug_rules <- detect_run_rule_signals(
            data$value,
            rep(emp_cl, nrow(data))
          )

          paste(
            "=== FIXED RUNS ANALYSIS (STANDARD MODE) ===",
            paste("Total data points:", nrow(data)),
            paste("Centerline:", round(emp_cl, 3)),
            "",
            "=== ALL CONSECUTIVE RUNS ===",
            paste("All run lengths:", paste(runs, collapse = ", ")),
            paste("Runs above centerline:", paste(above_runs, collapse = ", ")),
            paste("Runs below centerline:", paste(below_runs, collapse = ", ")),
            "",
            "=== RUNS SIGNALS (8+ CONSECUTIVE) ===",
            paste("Runs of 8+ points:", paste(long_runs, collapse = ", ")),
            paste("Total long runs detected:", length(long_runs)),
            paste("Runs signal triggered:", length(long_runs) > 0),
            "",
            "=== COMBINED RUN-RULE RESULT ===",
            run_rule_summary(debug_rules),
            paste("Six-point trend endpoints:", paste(which(debug_rules$trend_6), collapse = ", ")),
            paste("Fourteen-point alternating endpoints:", paste(which(debug_rules$alternating_14), collapse = ", ")),
            "",
            "=== IMPROVEMENT NOTES ===",
            "• Proper run length encoding (rle) detects ALL consecutive runs",
            "• No boundary condition bugs with rolling windows",
            "• Handles runs of any length (not just exactly 8)",
            "• Only marks 8th+ points in each run as signals",
            sep = "\n"
          )
        }
      },
      error = function(e) {
        paste("ERROR in runs analysis:", e$message)
      }
    )
  })

  # NEW: Runs debug data table
  output$runs_debug_table <- DT::renderDataTable(
    {
      tryCatch(
        {
          data <- filtered_data()
          if (is.null(data) || nrow(data) == 0) {
            return(data.frame(Message = "No data available"))
          }

          # Remove NA values
          data <- data[!is.na(data$date) & !is.na(data$value), ]
          if (nrow(data) == 0) {
            return(data.frame(
              Message = "No valid data after removing NA values"
            ))
          }

          # Calculate runs analysis details
          recalc_enabled <- !is.null(input$enable_recalc) &&
            input$enable_recalc &&
            !is.null(input$recalc_date) &&
            input$recalc_date >= min(data$date, na.rm = TRUE) &&
            input$recalc_date <= max(data$date, na.rm = TRUE)

          if (recalc_enabled) {
            recalc_date <- input$recalc_date
            data_before <- data[data$date < recalc_date, ]
            emp_cl_orig <- safe_mean(data_before$value)
            data_after <- data[data$date >= recalc_date, ]
            emp_cl_recalc <- if (nrow(data_after) >= 3) {
              safe_mean(data_after$value)
            } else {
              emp_cl_orig
            }

            # Use improved runs detection
            runs_signals <- detect_runs_signals_recalc(
              safe_numeric(data$value),
              emp_cl_orig,
              emp_cl_recalc,
              data$date,
              recalc_date
            )
            rule_signals <- detect_run_rule_signals(
              data$value,
              ifelse(data$date < recalc_date, emp_cl_orig, emp_cl_recalc),
              same_side_signals = runs_signals,
              segment = data$date >= recalc_date
            )

            centerline_used <- ifelse(
              data$date < recalc_date,
              emp_cl_orig,
              emp_cl_recalc
            )

            debug_data <- data.frame(
              Row = 1:nrow(data),
              Date = data$date,
              Value = round(safe_numeric(data$value), 3),
              Centerline_Used = round(centerline_used, 3),
              Above_Centerline = safe_numeric(data$value) > centerline_used,
              Same_Side_8 = rule_signals$same_side_8,
              Trend_6 = rule_signals$trend_6,
              Alternating_14 = rule_signals$alternating_14,
              Any_Run_Rule = rule_signals$any_run_rule,
              Segment = ifelse(data$date < recalc_date, "Before", "After")
            )
          } else {
            emp_cl <- safe_mean(data$value)

            # Use improved runs detection
            runs_signals <- detect_runs_signals(
              safe_numeric(data$value),
              rep(emp_cl, nrow(data)),
              data$date
            )
            rule_signals <- detect_run_rule_signals(
              data$value,
              rep(emp_cl, nrow(data)),
              same_side_signals = runs_signals
            )

            debug_data <- data.frame(
              Row = 1:nrow(data),
              Date = data$date,
              Value = round(safe_numeric(data$value), 3),
              Centerline = round(emp_cl, 3),
              Above_Centerline = safe_numeric(data$value) > emp_cl,
              Same_Side_8 = rule_signals$same_side_8,
              Trend_6 = rule_signals$trend_6,
              Alternating_14 = rule_signals$alternating_14,
              Any_Run_Rule = rule_signals$any_run_rule
            )
          }

          return(debug_data)
        },
        error = function(e) {
          return(data.frame(
            Error = paste("Error in runs debug table:", e$message)
          ))
        }
      )
    },
    options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px")
  )

  # ENHANCED: Render auto-correlation table with BOTH ACF and Sample Correlation
  output$autocorr_table <- DT::renderDataTable(
    {
      autocorr_data()
    },
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(
        list(targets = c(1, 2), className = "dt-right"), # Right-align numeric columns
        list(targets = c(3, 4), className = "dt-left") # Left-align interpretation columns
      )
    ),
    rownames = FALSE
  )

  #####  CHUNK 4 and 5  #####

  # Create reactive plot for run chart with IMPROVED data processing
  run_plot <- reactive({
    data <- analytical_data()
    req(data)

    # IMPROVED: Better data validation
    if (!"value" %in% names(data)) {
      return(empty_chart_message("No 'value' column found in data"))
    }

    # Retain explicit missing observations so cancelled/missing periods remain
    # visible and interrupt the moving range instead of being bridged silently.
    data <- data[!is.na(data$date), ]

    if (nrow(data) == 0) {
      return(empty_chart_message("No valid data after removing NA values"))
    }

    # IMPROVED: Safe numeric conversion and validation
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if (all(is.na(data$value))) {
      return(empty_chart_message("No valid numeric values in 'value' column"))
    }

    series_problem <- expectation_series_problem(data)
    if (!is.null(series_problem)) {
      return(empty_chart_message(series_problem))
    }

    run_series <- prepare_grouped_chart_lines(data, input$grouping_var)
    data <- run_series$data
    run_labels <- run_series$labels
    observation_axis <- "observation" %in% names(data)
    data$.chart_x <- if (observation_axis) data$observation else data$date
    run_labels$.label_x <- if (observation_axis) {
      run_labels$observation
    } else {
      run_labels$.label_date
    }

    title_text <- if (is.null(input$run_title) || input$run_title == "") {
      "Run Chart"
    } else {
      input$run_title
    }
    subtitle_text <- if (is.null(input$run_subtitle)) "" else input$run_subtitle
    caption_text <- if (is.null(input$run_caption)) "" else input$run_caption

    # IMPROVED: Safe statistical calculations
    median_value <- safe_median(data$value)

    if (is.na(median_value)) {
      return(empty_chart_message("Cannot calculate median - insufficient data"))
    }

    if (!observation_axis) {
      all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
      if (all_jan_first) {
        date_format <- "%Y"
        date_breaks_interval <- "1 year"
      } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(
        max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE)
      )
      if (date_range_days > 365 * 5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
      }
    }

    # Create run chart with fixed formatting standards
    chart <- ggplot(data, aes(x = .chart_x, y = value)) +
      (if (observation_axis) list() else superintendent_date_layers(data$date)) +

      # Lines: darkgray, linewidth 1.2
      geom_line(aes(group = .plot_group), color = "darkgray", linewidth = 1.2) +

      # Points: blue, size 2.5
      geom_point(color = "blue", size = 2.5) +

      geom_text(
        data = run_labels,
        aes(x = .label_x, y = value, label = .plot_group),
        inherit.aes = FALSE,
        hjust = 0,
        size = 5,
        fontface = "bold"
      ) +

      # Median reference line
      geom_hline(
        yintercept = median_value,
        color = "red",
        linewidth = 1,
        linetype = "solid",
        alpha = 0.8
      ) +

      # Median label above the line
      geom_text(
        aes(x = min(.chart_x), y = median_value, label = "Median"),
        color = "red",
        vjust = -0.5,
        hjust = 0,
        size = 8,
        fontface = "bold"
      ) +

      # Y-axis formatting: conditional based on user selection
      scale_y_continuous(
        labels = if (input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.10))
      ) +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = axis_label_or_default(input$run_x_label, if (observation_axis) "Observation" else "Time Period"),
        y = axis_label_or_default(input$run_y_label, "Value")
      ) +

      dashboard_chart_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1.00)
      )

    if (observation_axis) {
      chart + scale_x_continuous(breaks = sort(unique(data$observation)))
    } else {
      chart + scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.05, 0.25))
      )
    }
  })

  ##### CHUNK 5 #####

  # Render run chart
  chart_plot_server("run_chart", run_plot)

  # Create reactive plot for line chart with IMPROVED data processing
  line_plot <- reactive({
    data <- analytical_data()
    req(data)
    observation_axis <- "observation" %in% names(data)

    if (!"date" %in% names(data) || !"value" %in% names(data)) {
      return(empty_chart_message("Line chart needs standardized 'date' and 'value' columns"))
    }

    # raw_data() has already standardized the selected date column and, when
    # no calendar dates exist, supplied ordered placeholder Dates. Preserve
    # those Date values here instead of rejecting the placeholders as
    # implausible during a second conversion.
    data <- data |>
      dplyr::mutate(value = safe_numeric(value)) |>
      dplyr::filter(!is.na(date), !is.na(value)) |>
      dplyr::arrange(date)
    data$.chart_x <- if (observation_axis) data$observation else data$date

    if (nrow(data) == 0) {
      return(empty_chart_message("No valid date/value pairs for line chart"))
    }

    title_text <- if (is.null(input$line_title) || input$line_title == "") {
      "Line Chart"
    } else {
      input$line_title
    }

    subtitle_text <- if (is.null(input$line_subtitle)) {
      ""
    } else {
      input$line_subtitle
    }

    caption_text <- if (is.null(input$line_caption)) {
      ""
    } else {
      input$line_caption
    }

    if (!observation_axis) {
      all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
      if (all_jan_first) {
        date_format <- "%Y"
        date_breaks_interval <- "1 year"
      } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(
        max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE)
      )

      if (date_range_days > 365 * 5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
      }
    }

    # Resolve grouping variable.  When a valid grouping column is selected
    # (e.g. "series" from auto-pivoted wide data, a numeric tested-grade
    # column, or another discrete column), draw one line per group with
    # end-of-line labels and a colour
    # scale.  When no valid grouping column exists, draw a single blue line
    # exactly as before (group = 1 keeps ggplot from treating every point as
    # a distinct group and refusing to draw any line at all).
    group_var  <- input$grouping_var
    use_groups <- !is.null(group_var) &&
                  group_var != "" &&
                  group_var %in% base::names(data) &&
                  any(!is.na(data[[group_var]]))

    if (use_groups) {

      # Make grouping variable a factor so colour scale is discrete.
      data <- make_discrete_group(data, group_var)

      # Position each label in reserved space to the right of its final point.
      label_data <- grouped_line_endpoints(data, group_var)
      label_data <- offset_line_end_labels(label_data, data$date)
      label_data$.label_x <- if (observation_axis) {
        label_data$observation
      } else {
        label_data$.label_date
      }

      chart <- ggplot2::ggplot(
        data,
        ggplot2::aes(
          x     = .chart_x,
          y     = value,
          colour = .plot_group,
          group  = .plot_group
        )
      ) +
        (if (observation_axis) list() else superintendent_date_layers(data$date)) +
        ggplot2::geom_line(linewidth = 1.3) +
        ggplot2::geom_point(size = 3.5) +
        ggplot2::geom_text(
          data = label_data,
          ggplot2::aes(
            x = .label_x,
            label = .plot_group,
            colour = .plot_group
          ),
          hjust        = 0,
          vjust        = 0.4,
          size         = 5,
          fontface     = "bold",
          check_overlap = FALSE
        ) +
        ggplot2::scale_colour_manual(values = chart_colors) +
        ggplot2::scale_y_continuous(
          labels = if (input$format_as_percentage) {
            scales::percent_format(accuracy = 0.1)
          } else {
            function(y) format(y, scientific = FALSE, big.mark = ",")
          },
          expand = ggplot2::expansion(mult = c(0.10, 0.15))
        ) +
        ggplot2::labs(
          title    = title_text,
          subtitle = subtitle_text,
          caption  = caption_text,
          x        = axis_label_or_default(input$line_x_label, if (observation_axis) "Observation" else "Time Period"),
          y        = axis_label_or_default(input$line_y_label, "Value")
        ) +
        dashboard_chart_theme() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1.00)
        )

      if (observation_axis) {
        chart + ggplot2::scale_x_continuous(
          breaks = sort(unique(data$observation)),
          expand = ggplot2::expansion(mult = c(0.05, 0.15))
        )
      } else {
        chart + ggplot2::scale_x_date(
          date_labels = date_format,
          date_breaks = date_breaks_interval,
          expand = ggplot2::expansion(mult = c(0.05, 0.30))
        )
      }

    } else {

      # ---- Single-line fallback (original behaviour) ----------------------
      # group = 1 is required when no grouping column is used; without it,
      # ggplot treats every (date, value) pair as its own one-point group and
      # draws no connecting line.
      chart <- ggplot2::ggplot(data, ggplot2::aes(x = .chart_x, y = value, group = 1)) +
        (if (observation_axis) list() else superintendent_date_layers(data$date)) +
        ggplot2::geom_line(linewidth = 1.3, color = "steelblue") +
        ggplot2::geom_point(size = 3.5, color = "steelblue") +
        (if (observation_axis) list() else ggplot2::geom_text(
          ggplot2::aes(label = format(date, "%Y")),
          nudge_y = 0.015, size = 6, fontface = "bold",
          color = "black", check_overlap = TRUE
        )) +
        ggplot2::scale_y_continuous(
          labels = if (input$format_as_percentage) {
            scales::percent_format(accuracy = 0.1)
          } else {
            function(y) format(y, scientific = FALSE, big.mark = ",")
          },
          expand = ggplot2::expansion(mult = c(0.10, 0.15))
        ) +
        ggplot2::labs(
          title    = title_text,
          subtitle = subtitle_text,
          caption  = caption_text,
          x        = axis_label_or_default(input$line_x_label, if (observation_axis) "Observation" else "Time Period"),
          y        = axis_label_or_default(input$line_y_label, "Value")
        ) +
        dashboard_chart_theme() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1.00)
        )

      if (observation_axis) {
        chart + ggplot2::scale_x_continuous(breaks = sort(unique(data$observation)))
      } else {
        chart + ggplot2::scale_x_date(
          date_labels = date_format,
          date_breaks = date_breaks_interval,
          expand = ggplot2::expansion(mult = c(0.05, 0.15))
        )
      }

    } # end single-line fallback

  }) # end line_plot reactive

  # Render line chart
  chart_plot_server("line_chart", line_plot)

  # FIXED: Create reactive plot for expectation chart with IMPROVED runs analysis and data processing
  # UPDATED: Added auto-correlation adjustment option with horizontal annotation boxes
  control_plot <- reactive({
    data <- analytical_data()
    req(data)

    # IMPROVED: Better data validation
    if (!"value" %in% names(data)) {
      return(empty_chart_message("No 'value' column found in data"))
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if (nrow(data) == 0) {
      return(empty_chart_message("No valid data after removing NA values"))
    }

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if (all(is.na(data$value))) {
      return(empty_chart_message("No valid numeric values in 'value' column"))
    }

    control_series <- prepare_grouped_chart_lines(data, input$grouping_var)
    data <- control_series$data
    control_labels <- control_series$labels
    observation_axis <- "observation" %in% names(data)
    data$.chart_x <- if (observation_axis) data$observation else data$date
    control_labels$.label_x <- if (observation_axis) {
      control_labels$observation
    } else {
      control_labels$.label_date
    }

    title_text <- if (
      is.null(input$control_title) || input$control_title == ""
    ) {
      "Untrended Expectation Chart"
    } else {
      input$control_title
    }
    subtitle_text <- if (is.null(input$control_subtitle)) {
      ""
    } else {
      input$control_subtitle
    }
    caption_text <- if (is.null(input$control_caption)) {
      ""
    } else {
      input$control_caption
    }

    # Recalculate from either a calendar date or an ordinal observation.
    recalc_point <- if (observation_axis) {
      suppressWarnings(as.numeric(input$recalc_observation))
    } else {
      input$recalc_date
    }
    recalc_enabled <- !is.null(input$enable_recalc) &&
      input$enable_recalc &&
      length(recalc_point) == 1L &&
      !is.na(recalc_point) &&
      recalc_point >= min(data$.chart_x, na.rm = TRUE) &&
      recalc_point <= max(data$.chart_x, na.rm = TRUE)

    # Check if auto-correlation adjustment is enabled
    use_autocorr <- !is.null(input$use_autocorr_modifier) &&
      input$use_autocorr_modifier

    # Get correlation values if needed
    corr_vals <- if (use_autocorr) correlation_values() else NULL
    r_lag1 <- if (!is.null(corr_vals) && !is.na(corr_vals$r_lag1)) {
      corr_vals$r_lag1
    } else {
      NA
    }

    if (recalc_enabled) {
      # RECALCULATION MODE: Split data and calculate separate expectation limits
      recalc_date <- recalc_point

      # Split data into two segments
      data_before <- data[data$.chart_x < recalc_date, ]
      data_after <- data[data$.chart_x >= recalc_date, ]

      if (sum(!is.na(data_before$value)) < 2L) {
        return(empty_chart_message(
          "At least two valid pre-intervention observations are required to calculate frozen baseline limits."
        ))
      }

      # IMPROVED: Calculate original expectation chart statistics with safe functions
      emp_cl_orig <- safe_mean(data_before$value)

      # Calculate sigma based on whether auto-correlation adjustment is enabled
      if (use_autocorr && !is.na(r_lag1)) {
        # Calculate moving ranges for the entire dataset
        avg_mr_orig <- calculate_moving_ranges(data_before$value)
        if (!is.na(avg_mr_orig) && abs(r_lag1) < 0.999) {
          # Avoid division by zero
          # Apply auto-correlation adjustment formula: σ = R-bar / (d2 * √(1 - r²))
          sigma_orig <- avg_mr_orig / (1.128 * sqrt(1 - r_lag1^2))
        } else {
          # Fallback to standard deviation if moving range fails or r is too close to 1
          sigma_orig <- safe_sd(data_before$value)
        }
      } else {
        # Standard calculation using moving range method for consistency
        avg_mr_orig <- calculate_moving_ranges(data_before$value)
        if (!is.na(avg_mr_orig)) {
          # Standard moving range sigma calculation
          sigma_orig <- avg_mr_orig / 1.128
        } else {
          # Fallback to standard deviation if moving range fails
          sigma_orig <- safe_sd(data_before$value)
        }
      }

      emp_ucl_orig <- emp_cl_orig + 3 * sigma_orig
      emp_lcl_orig <- emp_cl_orig - 3 * sigma_orig

      # Calculate recalculated expectation chart statistics
      if (nrow(data_after) >= 3) {
        emp_cl_recalc <- safe_mean(data_after$value)

        # Calculate sigma for recalculated segment
        if (use_autocorr && !is.na(r_lag1)) {
          # Calculate moving ranges for the after-recalc segment
          avg_mr_recalc <- calculate_moving_ranges(data_after$value)
          if (!is.na(avg_mr_recalc) && abs(r_lag1) < 0.999) {
            # Avoid division by zero
            # Apply auto-correlation adjustment formula: σ = R-bar / (d2 * √(1 - r²))
            sigma_recalc <- avg_mr_recalc / (1.128 * sqrt(1 - r_lag1^2))
          } else {
            # Fallback to standard deviation if moving range fails or r is too close to 1
            sigma_recalc <- safe_sd(data_after$value)
          }
        } else {
          # Standard calculation using moving range method for consistency
          avg_mr_recalc <- calculate_moving_ranges(data_after$value)
          if (!is.na(avg_mr_recalc)) {
            # Standard moving range sigma calculation
            sigma_recalc <- avg_mr_recalc / 1.128
          } else {
            # Fallback to standard deviation if moving range fails
            sigma_recalc <- safe_sd(data_after$value)
          }
        }

        emp_ucl_recalc <- emp_cl_recalc + 3 * sigma_recalc
        emp_lcl_recalc <- emp_cl_recalc - 3 * sigma_recalc
      } else {
        emp_cl_recalc <- emp_cl_orig
        sigma_recalc <- sigma_orig
        emp_ucl_recalc <- emp_ucl_orig
        emp_lcl_recalc <- emp_lcl_orig
      }

      # Add expectation chart columns to data
      data$emp_cl_orig <- emp_cl_orig
      data$emp_ucl_orig <- emp_ucl_orig
      data$emp_lcl_orig <- emp_lcl_orig
      data$emp_cl_recalc <- emp_cl_recalc
      data$emp_ucl_recalc <- emp_ucl_recalc
      data$emp_lcl_recalc <- emp_lcl_recalc
      data$recalc_date <- recalc_date

      # Calculate sigma signals using appropriate limits for each segment
      data$sigma_signals <- ifelse(
        data$.chart_x < recalc_date,
        data$value > emp_ucl_orig | data$value < emp_lcl_orig,
        data$value > emp_ucl_recalc | data$value < emp_lcl_recalc
      )

      # Use improved runs analysis for recalculation mode
      same_side_signals <- detect_runs_signals_recalc(
        data$value,
        emp_cl_orig,
        emp_cl_recalc,
        data$.chart_x,
        recalc_date
      )
      run_rules <- detect_run_rule_signals(
        data$value,
        ifelse(data$.chart_x < recalc_date, emp_cl_orig, emp_cl_recalc),
        same_side_signals = same_side_signals,
        segment = data$.chart_x >= recalc_date
      )
      data$same_side_signal <- run_rules$same_side_8
      data$trend_signal <- run_rules$trend_6
      data$alternating_signal <- run_rules$alternating_14
      data$runs_signal <- run_rules$any_run_rule

      # Create annotation data for original limits - HORIZONTAL FORMAT
      annotation_text_orig <- paste0(
        "Original: UCL = ",
        round(emp_ucl_orig, 2),
        " | CL = ",
        round(emp_cl_orig, 2),
        " | LCL = ",
        round(emp_lcl_orig, 2)
      )
      if (use_autocorr && !is.na(r_lag1)) {
        annotation_text_orig <- paste0(
          annotation_text_orig,
          " | σ = ",
          round(sigma_orig, 3),
          " (autocorr-adjusted, r = ",
          round(r_lag1, 3),
          ")"
        )
      }

      annotation_data_orig <- data.frame(
        x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.02,
        y_pos = emp_ucl_orig + (emp_ucl_orig - emp_lcl_orig) * 0.15, # Moved higher
        label = annotation_text_orig
      )

      # Move recalculated annotation box to the right - HORIZONTAL FORMAT
      annotation_text_recalc <- paste0(
        "Recalculated (from ",
        if (observation_axis) recalc_date else format(recalc_date, "%Y-%m-%d"),
        "): UCL = ",
        round(emp_ucl_recalc, 2),
        " | CL = ",
        round(emp_cl_recalc, 2),
        " | LCL = ",
        round(emp_lcl_recalc, 2)
      )
      if (use_autocorr && !is.na(r_lag1)) {
        annotation_text_recalc <- paste0(
          annotation_text_recalc,
          " | σ = ",
          round(sigma_recalc, 3),
          " (autocorr-adjusted)"
        )
      }

      annotation_data_recalc <- data.frame(
        x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.55,
        y_pos = emp_lcl_orig - (emp_ucl_orig - emp_lcl_orig) * 0.08,
        label = annotation_text_recalc
      )

      # Create annotation data for runs analysis
      annotation_data_runs <- data.frame(
        x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.02,
        y_pos = emp_lcl_orig - (emp_ucl_orig - emp_lcl_orig) * 0.15,
        label = run_rule_summary(run_rules)
      )
    } else {
      # STANDARD MODE: Original untrended expectation chart
      # IMPROVED: Safe statistical calculations
      emp_cl <- safe_mean(data$value)

      # Calculate sigma based on whether auto-correlation adjustment is enabled
      if (use_autocorr && !is.na(r_lag1)) {
        # Calculate average moving range
        avg_mr <- calculate_moving_ranges(data$value)
        if (!is.na(avg_mr) && abs(r_lag1) < 0.999) {
          # Avoid division by zero
          # Apply auto-correlation adjustment formula: σ = R-bar / (d2 * √(1 - r²))
          # where d2 = 1.128 for moving range of 2 consecutive points
          sigma <- avg_mr / (1.128 * sqrt(1 - r_lag1^2))
        } else {
          # Fallback to standard deviation if moving range calculation fails or r is too close to 1
          sigma <- safe_sd(data$value)
        }
      } else {
        # Standard calculation using moving range method for consistency
        avg_mr <- calculate_moving_ranges(data$value)
        if (!is.na(avg_mr)) {
          # Standard moving range sigma calculation: σ = R-bar / d2
          sigma <- avg_mr / 1.128
        } else {
          # Fallback to standard deviation if moving range calculation fails
          sigma <- safe_sd(data$value)
        }
      }

      emp_ucl <- emp_cl + 3 * sigma
      emp_lcl <- emp_cl - 3 * sigma

      # Add expectation chart columns to data
      data$emp_cl <- emp_cl
      data$emp_ucl <- emp_ucl
      data$emp_lcl <- emp_lcl

      # Calculate sigma signals
      data$sigma_signals <- data$value > emp_ucl | data$value < emp_lcl

      # Use improved runs analysis
      same_side_signals <- detect_runs_signals(
        data$value,
        rep(emp_cl, nrow(data)),
        data$date
      )
      run_rules <- detect_run_rule_signals(
        data$value,
        rep(emp_cl, nrow(data)),
        same_side_signals = same_side_signals
      )
      data$same_side_signal <- run_rules$same_side_8
      data$trend_signal <- run_rules$trend_6
      data$alternating_signal <- run_rules$alternating_14
      data$runs_signal <- run_rules$any_run_rule

      # Create annotation data for limits - HORIZONTAL FORMAT
      annotation_text <- paste0(
        "UCL = ",
        round(emp_ucl, 2),
        " | CL = ",
        round(emp_cl, 2),
        " | LCL = ",
        round(emp_lcl, 2)
      )
      if (use_autocorr && !is.na(r_lag1)) {
        # Add auto-correlation information to annotation
        avg_mr_display <- if (exists("avg_mr") && !is.na(avg_mr)) {
          round(avg_mr, 3)
        } else {
          "N/A"
        }
        base_sigma <- if (exists("avg_mr") && !is.na(avg_mr)) {
          round(avg_mr / 1.128, 3)
        } else {
          "N/A"
        }
        annotation_text <- paste0(
          annotation_text,
          " | σ = ",
          round(sigma, 3),
          " (autocorr-adjusted) | Base σ = ",
          base_sigma,
          " | r = ",
          round(r_lag1, 3),
          " | avg MR = ",
          avg_mr_display
        )
      } else {
        # Show moving range info for standard calculation too
        avg_mr_display <- if (exists("avg_mr") && !is.na(avg_mr)) {
          round(avg_mr, 3)
        } else {
          "N/A"
        }
        annotation_text <- paste0(
          annotation_text,
          " | σ = ",
          round(sigma, 3),
          " (moving range method) | avg MR = ",
          avg_mr_display
        )
      }

      annotation_data_limits <- data.frame(
        x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.02,
        y_pos = emp_ucl + (emp_ucl - emp_lcl) * 0.15, # Moved higher
        label = annotation_text
      )

      # Create annotation data for runs analysis
      annotation_data_runs <- data.frame(
        x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.02,
        y_pos = emp_lcl - (emp_ucl - emp_lcl) * 0.05,
        label = run_rule_summary(run_rules)
      )
    }

    # Smart date formatting
    all_jan_first <- !observation_axis && all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (!observation_axis && all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else if (!observation_axis) {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(
        max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE)
      )
      if (date_range_days > 365 * 5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create expectation chart plot
    p <- ggplot(data, aes(x = .chart_x, y = value)) +
      (if (observation_axis) list() else superintendent_date_layers(data$date)) +

      # Connect the dots with lines
      geom_line(aes(group = .plot_group), color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
      geom_text(
        data = control_labels,
        aes(x = .label_x, y = value, label = .plot_group),
        inherit.aes = FALSE,
        hjust = 0,
        size = 5,
        fontface = "bold"
      )

    if (recalc_enabled) {
      # RECALCULATION MODE: Show both sets of expectation limits

      # Determine if any runs signal exists for line style
      any_runs_signal <- any(data$runs_signal, na.rm = TRUE)
      centerline_style <- if (any_runs_signal) "dashed" else "solid"

      # Original centerline (before recalc date)
      p <- p +
        geom_line(
          aes(y = ifelse(.chart_x < recalc_date, emp_cl_orig, NA)),
          color = "blue",
          linewidth = 1,
          linetype = centerline_style
        ) +

        # Recalculated centerline (after recalc date)
        geom_line(
          aes(y = ifelse(.chart_x >= recalc_date, emp_cl_recalc, NA)),
          color = "green",
          linewidth = 1,
          linetype = centerline_style
        ) +

        # Original expectation limits (before recalc date)
        geom_line(
          aes(y = ifelse(.chart_x < recalc_date, emp_ucl_orig, NA)),
          color = "red",
          linetype = "solid",
          linewidth = 1
        ) +
        geom_line(
          aes(y = ifelse(.chart_x < recalc_date, emp_lcl_orig, NA)),
          color = "red",
          linetype = "solid",
          linewidth = 1
        ) +

        # Recalculated expectation limits (after recalc date)
        geom_line(
          aes(y = ifelse(.chart_x >= recalc_date, emp_ucl_recalc, NA)),
          color = "red",
          linetype = "dashed",
          linewidth = 1
        ) +
        geom_line(
          aes(y = ifelse(.chart_x >= recalc_date, emp_lcl_recalc, NA)),
          color = "red",
          linetype = "dashed",
          linewidth = 1
        ) +

        # Vertical line at recalculation point
        geom_vline(
          xintercept = recalc_date,
          color = "purple",
          linetype = "dotted",
          linewidth = 1.5,
          alpha = 0.7
        ) +

        # Rich text annotations for both sets of limits - HORIZONTAL FORMAT
        ggtext::geom_richtext(
          data = annotation_data_orig,
          aes(x = x_pos, y = y_pos, label = label),
          size = 4,
          color = "black",
          hjust = 0,
          vjust = 1,
          fill = "lightblue",
          label.color = "black",
          label.padding = grid::unit(c(0.2, 0.5, 0.2, 0.5), "lines")
        ) +

        ggtext::geom_richtext(
          data = annotation_data_recalc,
          aes(x = x_pos, y = y_pos, label = label),
          size = 4,
          color = "black",
          hjust = 0,
          vjust = 1,
          fill = "lightgreen",
          label.color = "black",
          label.padding = grid::unit(c(0.2, 0.5, 0.2, 0.5), "lines")
        ) +

        ggtext::geom_richtext(
          data = annotation_data_runs,
          aes(x = x_pos, y = y_pos, label = label),
          size = 5,
          color = "black",
          hjust = 0,
          vjust = 0,
          fill = "lightyellow",
          label.color = "black"
        ) +

        # Labels for original limits
        geom_text(
          aes(
            x = dplyr::first(.chart_x),
            y = dplyr::first(emp_cl_orig),
            label = "Original Expectation"
          ),
          color = "blue",
          vjust = -1,
          hjust = 1.1,
          size = 5
        ) +

        # Labels for recalculated limits
        geom_text(
          aes(x = recalc_date, y = emp_cl_recalc, label = "Recalc Expectation"),
          color = "green",
          vjust = -1,
          hjust = -0.1,
          size = 5
        ) +

        # Recalculation point label
        geom_text(
          aes(
            x = recalc_date,
            y = max(c(emp_ucl_orig, emp_ucl_recalc), na.rm = TRUE),
            label = "Recalc Point"
          ),
          color = "purple",
          vjust = -0.5,
          hjust = 0.5,
          size = 5,
          fontface = "bold"
        )
    } else {
      # STANDARD MODE: Original expectation chart display

      # Determine if any runs signal exists for line style
      any_runs_signal <- any(data$runs_signal, na.rm = TRUE)
      centerline_style <- if (any_runs_signal) "dashed" else "solid"

      p <- p +
        # Center line with linetype based on runs signals
        geom_line(
          aes(y = emp_cl),
          color = "blue",
          linewidth = 1,
          linetype = centerline_style
        ) +

        # Upper and lower expectation limits
        geom_line(
          aes(y = emp_ucl),
          color = "red",
          linetype = "solid",
          linewidth = 1
        ) +
        geom_line(
          aes(y = emp_lcl),
          color = "red",
          linetype = "solid",
          linewidth = 1
        ) +

        # Rich text annotations - HORIZONTAL FORMAT
        ggtext::geom_richtext(
          data = annotation_data_limits,
          aes(x = x_pos, y = y_pos, label = label),
          size = 4,
          color = "black",
          hjust = 0,
          vjust = 1,
          fill = "lightblue",
          label.color = "black",
          label.padding = grid::unit(c(0.2, 0.5, 0.2, 0.5), "lines")
        ) +

        ggtext::geom_richtext(
          data = annotation_data_runs,
          aes(x = x_pos, y = y_pos, label = label),
          size = 5,
          color = "black",
          hjust = 0,
          vjust = 0,
          fill = "lightyellow",
          label.color = "black"
        ) +

        # Standard labels
        geom_text(
          aes(
            x = dplyr::first(.chart_x),
            y = dplyr::first(emp_cl),
            label = "Avg Expectation"
          ),
          color = "blue",
          vjust = -1,
          hjust = 1.1,
          size = 5
        ) +

        geom_text(
          aes(
            x = dplyr::last(.chart_x),
            y = dplyr::last(emp_cl),
            label = format(round(dplyr::last(emp_cl), 2), nsmall = 2)
          ),
          color = "blue",
          vjust = 0,
          hjust = -0.5,
          size = 5
        ) +

        geom_text(
          aes(
            x = dplyr::first(.chart_x) + 0.5,
            y = dplyr::first(emp_ucl),
            label = "Upper Expectation"
          ),
          color = "red",
          vjust = -1,
          hjust = 1.1,
          size = 5
        ) +

        geom_text(
          aes(
            x = dplyr::last(.chart_x),
            y = dplyr::last(emp_ucl),
            label = format(round(dplyr::last(emp_ucl), 2), nsmall = 2)
          ),
          color = "red",
          vjust = -1,
          hjust = -0.5,
          size = 5
        ) +

        geom_text(
          aes(
            x = dplyr::first(.chart_x) + 0.5,
            y = dplyr::first(emp_lcl),
            label = "Lower Expectation"
          ),
          color = "red",
          vjust = 1.75,
          hjust = 1.1,
          size = 5
        ) +

        geom_text(
          aes(
            x = dplyr::last(.chart_x),
            y = dplyr::last(emp_lcl),
            label = format(round(dplyr::last(emp_lcl), 2), nsmall = 2)
          ),
          color = "red",
          vjust = 1.5,
          hjust = -0.5,
          size = 5
        )
    }

    # Complete the plot with formatting
    p <- p +
      # Y-axis formatting with generous space for labels
      scale_y_continuous(
        labels = if (input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))
      ) +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = axis_label_or_default(input$control_x_label, if (observation_axis) "Observation" else "Time Period"),
        y = axis_label_or_default(input$control_y_label, "Value")
      ) +

      dashboard_chart_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    if (observation_axis) {
      p <- p + scale_x_continuous(breaks = sort(unique(data$observation)))
    } else {
      p <- p + scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.25))
      )
    }

    return(p)
  })

  # Create reactive plot for bar chart with IMPROVED data processing
  bar_plot <- reactive({
    data <- analytical_data()
    req(data)

    # IMPROVED: Better data validation
    if (!"value" %in% names(data)) {
      return(empty_chart_message("No 'value' column found in data"))
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if (nrow(data) == 0) {
      return(empty_chart_message("No valid data after removing NA values"))
    }

    # Use selected grouping variable
    if (
      is.null(input$grouping_var) ||
        input$grouping_var == "" ||
        !input$grouping_var %in% names(data)
    ) {
      return(empty_chart_message("Select a grouping variable above to create a bar chart"))
    }

    group_var <- input$grouping_var

    # Make the grouping variable safe for discrete fill scales.
    data <- make_discrete_group(data, group_var)

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if (all(is.na(data$value))) {
      return(empty_chart_message("No valid numeric values in 'value' column"))
    }

    # Calculate averages across all user-selected dates for each group using safe functions
    bar_data <- data %>%
      group_by(.plot_group) %>%
      summarise(value = safe_mean(value), .groups = 'drop')

    title_text <- if (is.null(input$bar_title) || input$bar_title == "") {
      "Bar Chart"
    } else {
      input$bar_title
    }
    subtitle_text <- if (is.null(input$bar_subtitle)) "" else input$bar_subtitle
    caption_text <- if (is.null(input$bar_caption)) "" else input$bar_caption

    # Create bar chart
    ggplot(
      bar_data,
      aes(
        x = reorder(.plot_group, value),
        y = value,
        fill = .plot_group
      )
    ) +
      geom_col(width = 0.7, alpha = 0.8) +

      # Add value labels at the end of each bar
      geom_text(
        aes(
          label = if (input$format_as_percentage) {
            scales::percent(value, accuracy = 0.1)
          } else {
            format(round(value, 2), big.mark = ",")
          }
        ),
        hjust = -0.1,
        vjust = 0.5,
        size = 6,
        color = "black",
        fontface = "bold"
      ) +

      scale_fill_manual(values = chart_colors) +
      scale_y_continuous(
        labels = if (input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.15)) # Increased right margin for labels
      ) +
      scale_x_discrete(expand = expansion(add = 0.6)) +
      # Flip coordinates so values are horizontal
      coord_flip() +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = axis_label_or_default(input$bar_x_label, tools::toTitleCase(group_var)),
        y = axis_label_or_default(input$bar_y_label, "Average Value")
      ) +

      dashboard_chart_theme() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5), # No angle needed for horizontal values
        axis.text.y = element_text(angle = 0, hjust = 1.0) # Groups on y-axis
      )
  })

  # Render bar chart
  chart_plot_server("bar_chart", bar_plot)

  # Create reactive plot for trended expectation chart with IMPROVED data processing
  trended_plot <- reactive({
    data <- analytical_data()
    req(data)

    # IMPROVED: Better data validation
    if (!"value" %in% names(data)) {
      return(empty_chart_message("No 'value' column found in data"))
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if (nrow(data) == 0) {
      return(empty_chart_message("No valid data after removing NA values"))
    }

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if (all(is.na(data$value))) {
      return(empty_chart_message("No valid numeric values in 'value' column"))
    }

    trended_series <- prepare_grouped_chart_lines(data, input$grouping_var)
    data <- trended_series$data
    trended_labels <- trended_series$labels
    observation_axis <- "observation" %in% names(data)
    data$.chart_x <- if (observation_axis) data$observation else data$date
    trended_labels$.label_x <- if (observation_axis) {
      trended_labels$observation
    } else {
      trended_labels$.label_date
    }

    title_text <- if (
      is.null(input$trended_title) || input$trended_title == ""
    ) {
      "Trended Expectation Chart"
    } else {
      input$trended_title
    }
    subtitle_text <- if (is.null(input$trended_subtitle)) {
      ""
    } else {
      input$trended_subtitle
    }
    caption_text <- if (is.null(input$trended_caption)) {
      ""
    } else {
      input$trended_caption
    }

    # Use elapsed days for dated series and ordinal position for sequences.
    data$date_numeric <- if (observation_axis) {
      data$observation - min(data$observation)
    } else {
      as.numeric(data$date - min(data$date))
    }

    # IMPROVED: Fit linear model with robust error handling
    trend_model <- NULL
    bias_corrected_sd <- 0

    tryCatch(
      {
        trend_model <- lm(value ~ date_numeric, data = data)

        # Calculate residuals standard deviation with bias correction
        residuals_sd <- safe_sd(residuals(trend_model))
        bias_corrected_sd <- residuals_sd / 1.128

        # Calculate trended centerline and expectation limits
        data$trended_cl <- as.numeric(predict(trend_model, newdata = data))
        data$trended_ucl <- data$trended_cl + 3 * bias_corrected_sd
        data$trended_lcl <- data$trended_cl - 3 * bias_corrected_sd
      },
      error = function(e) {
        # Fallback to simple mean if linear model fails
        emp_cl <- safe_mean(data$value)
        data$trended_cl <<- rep(emp_cl, nrow(data))
        data$trended_ucl <<- rep(emp_cl + 3 * safe_sd(data$value), nrow(data))
        data$trended_lcl <<- rep(emp_cl - 3 * safe_sd(data$value), nrow(data))
        bias_corrected_sd <<- safe_sd(data$value)
      }
    )

    # Calculate CAGR from trend line endpoints (uses predicted values so
    # single-point noise at the extremes does not distort the rate).
    # Falls back to NA when the model failed or start value is zero/negative.
    cagr_value <- tryCatch({
      if (!observation_axis && !is.null(trend_model) && nrow(data) >= 2) {
        start_val <- data$trended_cl[which.min(data$date)]
        end_val   <- data$trended_cl[which.max(data$date)]
        n_years   <- as.numeric(
          max(data$date) - min(data$date)
        ) / 365.25
        if (start_val > 0 && n_years > 0) {
          (end_val / start_val)^(1 / n_years) - 1
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)

    # Calculate sigma signals (points outside expectation limits)
    data$sigma_signals <- data$value > data$trended_ucl |
      data$value < data$trended_lcl

    # Use improved runs analysis for trended chart
    same_side_signals <- detect_runs_signals(
      data$value,
      data$trended_cl,
      data$date
    )
    run_rules <- detect_run_rule_signals(
      data$value,
      data$trended_cl,
      same_side_signals = same_side_signals
    )
    data$same_side_signal <- run_rules$same_side_8
    data$trend_signal <- run_rules$trend_6
    data$alternating_signal <- run_rules$alternating_14
    data$runs_signal <- run_rules$any_run_rule

    # Create annotation data for limits
    annotation_data_limits <- data.frame(
      x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.02,
      y_pos = max(data$trended_ucl) +
        (max(data$trended_ucl) - min(data$trended_lcl)) * 0.02,
      label = if (!is.null(trend_model)) {
        cagr_line <- if (!is.na(cagr_value)) {
          paste0(
            "<br>CAGR = ",
            base::format(round(cagr_value * 100, 2), nsmall = 2),
            "% per year"
          )
        } else {
          ""
        }
        paste0(
          "Trend Model: y = ",
          round(coef(trend_model)[1], 2),
          " + ",
          round(coef(trend_model)[2], 4),
          if (observation_axis) " × observations<br>" else " × days<br>",
          "Bias-corrected σ = ",
          round(bias_corrected_sd, 2),
          cagr_line
        )
      } else {
        paste0(
          "Fallback Model (No Trend)<br>",
          "σ = ",
          round(bias_corrected_sd, 2)
        )
      }
    )

    # Create annotation data for runs analysis
    annotation_data_runs <- data.frame(
      x_pos = min(data$.chart_x) + as.numeric(diff(range(data$.chart_x))) * 0.02,
      y_pos = min(data$trended_lcl) -
        (max(data$trended_ucl) - min(data$trended_lcl)) * 0.05,
      label = run_rule_summary(run_rules)
    )

    # Smart date formatting
    all_jan_first <- !observation_axis && all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (!observation_axis && all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else if (!observation_axis) {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(
        max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE)
      )
      if (date_range_days > 365 * 5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create trended expectation chart

    # Determine if any runs signal exists for line style
    any_runs_signal <- any(data$runs_signal, na.rm = TRUE)
    centerline_style <- if (any_runs_signal) "dashed" else "solid"

    chart <- ggplot(data, aes(x = .chart_x, y = value)) +
      (if (observation_axis) list() else superintendent_date_layers(data$date)) +

      # Connect the dots with lines
      geom_line(aes(group = .plot_group), color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals (red = outside limits, blue = within)
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +

      geom_text(
        data = trended_labels,
        aes(x = .label_x, y = value, label = .plot_group),
        inherit.aes = FALSE,
        hjust = 0,
        size = 5,
        fontface = "bold"
      ) +

      # Trended centerline with linetype based on runs signals
      geom_line(
        aes(y = trended_cl),
        color = "blue",
        linewidth = 1,
        linetype = centerline_style
      ) +

      # Upper and lower expectation limits (red dotted lines for trended)
      geom_line(
        aes(y = trended_ucl),
        color = "red",
        linetype = "dotted",
        linewidth = 1
      ) +
      geom_line(
        aes(y = trended_lcl),
        color = "red",
        linetype = "dotted",
        linewidth = 1
      ) +

      # Rich text annotation for trend model info
      ggtext::geom_richtext(
        data = annotation_data_limits,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,
        color = "black",
        hjust = 0,
        vjust = 1,
        fill = "lightblue",
        label.color = "black"
      ) +

      # Rich text annotation for runs analysis
      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,
        color = "black",
        hjust = 0,
        vjust = 0,
        fill = "lightyellow",
        label.color = "black"
      ) +

      # Label for trended centerline at start
      geom_text(
        aes(
          x = dplyr::first(.chart_x),
          y = dplyr::first(trended_cl),
          label = "Trended Expectation"
        ),
        color = "blue",
        vjust = -1,
        hjust = 1.1,
        size = 5
      ) +

      # Value for trended centerline at end
      geom_text(
        aes(
          x = dplyr::last(.chart_x),
          y = dplyr::last(trended_cl),
          label = format(round(dplyr::last(trended_cl), 2), nsmall = 2)
        ),
        color = "blue",
        vjust = 0,
        hjust = -0.5,
        size = 5
      ) +

      # Label for upper limit at start
      geom_text(
        aes(
          x = dplyr::first(.chart_x) + 0.5,
          y = dplyr::first(trended_ucl),
          label = "Upper Trend Limit"
        ),
        color = "red",
        vjust = -1,
        hjust = 1.1,
        size = 5
      ) +

      # Value for upper limit at end
      geom_text(
        aes(
          x = dplyr::last(.chart_x),
          y = dplyr::last(trended_ucl),
          label = format(round(dplyr::last(trended_ucl), 2), nsmall = 2)
        ),
        color = "red",
        vjust = -1,
        hjust = -0.5,
        size = 5
      ) +

      # Label for lower limit at start
      geom_text(
        aes(
          x = dplyr::first(.chart_x) + 0.5,
          y = dplyr::first(trended_lcl),
          label = "Lower Trend Limit"
        ),
        color = "red",
        vjust = 1.75,
        hjust = 1.1,
        size = 5
      ) +

      # Value for lower limit at end
      geom_text(
        aes(
          x = dplyr::last(.chart_x),
          y = dplyr::last(trended_lcl),
          label = format(round(dplyr::last(trended_lcl), 2), nsmall = 2)
        ),
        color = "red",
        vjust = 1.5,
        hjust = -0.5,
        size = 5
      ) +

      # Y-axis formatting with generous space for labels
      scale_y_continuous(
        labels = if (input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))
      ) +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = axis_label_or_default(input$trended_x_label, if (observation_axis) "Observation" else "Time Period"),
        y = axis_label_or_default(input$trended_y_label, "Value")
      ) +

      dashboard_chart_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    if (observation_axis) {
      chart + scale_x_continuous(breaks = sort(unique(data$observation)))
    } else {
      chart + scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.25))
      )
    }
  })

  # Create reactive plot for educational cohort tracking with IMPROVED data processing
  cohort_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if (!"value" %in% names(data)) {
      return(empty_chart_message("No 'value' column found in data"))
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if (nrow(data) == 0) {
      return(empty_chart_message("No valid data after removing NA values"))
    }

    # Check if required inputs are available
    if (
      is.null(input$cohort_grade_var) ||
        input$cohort_grade_var == "" ||
        !input$cohort_grade_var %in% names(data) ||
        is.null(input$cohort_start_grade) ||
        is.null(input$cohort_end_grade) ||
        is.null(input$cohort_start_year) ||
        is.null(input$cohort_end_year)
    ) {
      return(empty_chart_message("Select a grade column and grade/year ranges above for cohort tracking"))
    }

    grade_var <- input$cohort_grade_var
    start_grade <- input$cohort_start_grade
    end_grade <- input$cohort_end_grade
    start_year <- input$cohort_start_year
    end_year <- input$cohort_end_year

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if (all(is.na(data$value))) {
      return(empty_chart_message("No valid numeric values in 'value' column"))
    }

    title_text <- if (is.null(input$cohort_title) || input$cohort_title == "") {
      "Educational Cohort Analysis"
    } else {
      input$cohort_title
    }
    subtitle_text <- if (is.null(input$cohort_subtitle)) {
      ""
    } else {
      input$cohort_subtitle
    }
    caption_text <- if (is.null(input$cohort_caption)) {
      ""
    } else {
      input$cohort_caption
    }

    # Validate inputs
    if (end_grade < start_grade || end_year < start_year) {
      return(empty_chart_message("Ending grade/year must be greater than or equal to the starting grade/year"))
    }

    # Create cohort progression data
    cohort_data <- data.frame()

    current_year <- start_year
    current_grade <- start_grade

    while (current_year <= end_year && current_grade <= end_grade) {
      # Check if data has 'year' column, otherwise extract from 'date'
      year_matches <- if ("year" %in% names(data)) {
        # Extract year from date objects if needed
        if (inherits(data$year, c("Date", "POSIXct", "POSIXlt"))) {
          year(data$year) == current_year
        } else {
          data$year == current_year
        }
      } else if ("date" %in% names(data)) {
        year(data$date) == current_year
      } else {
        rep(TRUE, nrow(data)) # If no year info, include all
      }

      # Use user-selected grade column
      grade_matches <- if (is.numeric(data[[grade_var]])) {
        # If numeric grade column (like grade_level_code: 3,4,5,6,7,8)
        data[[grade_var]] == current_grade
      } else {
        # If text grade column (like grade_level_name: "Grade 3", "Grade 4", etc.)
        # Try to extract number from text or match text patterns
        grade_text_patterns <- paste0("\\b", current_grade, "\\b") # Word boundary match
        grepl(grade_text_patterns, data[[grade_var]], ignore.case = TRUE)
      }

      # Get data for this year-grade combination
      year_grade_data <- data[year_matches & grade_matches, ]

      if (nrow(year_grade_data) > 0) {
        # Extract the single value directly (no averaging needed)
        cohort_value <- safe_numeric(year_grade_data$value[1]) # Take first/only value

        # Add to cohort progression
        cohort_data <- rbind(
          cohort_data,
          data.frame(
            year = current_year,
            grade = current_grade,
            cohort_year = current_year - start_year + 1,
            value = cohort_value
          )
        )
      }

      current_year <- current_year + 1
      current_grade <- current_grade + 1
    }

    if (nrow(cohort_data) == 0) {
      return(empty_chart_message("No data found for the selected grade column and year range"))
    }

    cohort_group_label <- grouping_category_label(data, input$grouping_var)
    cohort_label_data <- utils::tail(cohort_data, 1)
    cohort_label_data$group_label <- cohort_group_label

    # Create the cohort progression line chart
    ggplot(cohort_data, aes(x = grade, y = value)) +
      superintendent_cohort_layers(cohort_data) +

      # Connect dots with line
      geom_line(color = "#2E86AB", linewidth = 1.5) +

      # Points for each grade
      geom_point(color = "#A23B72", size = 3.5) +

      geom_text(
        data = cohort_label_data,
        aes(x = grade, y = value, label = group_label),
        inherit.aes = FALSE,
        color = "#2E86AB",
        hjust = -0.1,
        size = 5,
        fontface = "bold"
      ) +

      # Add value labels above points (with better precision)
      geom_text(
        aes(label = format(round(value, 3), nsmall = 3)),
        color = "black",
        vjust = -0.8,
        size = 5,
        fontface = "bold"
      ) +

      # Add year labels below points (larger and more distinct)
      geom_text(
        aes(label = paste0("(", year, ")")),
        color = "darkgreen",
        vjust = 2.5,
        size = 7,
        fontface = "bold"
      ) +

      scale_x_continuous(
        breaks = cohort_data$grade,
        labels = paste("Grade", cohort_data$grade),
        expand = expansion(mult = c(0.1, 0.25))
      ) +

      scale_y_continuous(
        labels = if (input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.15, 0.15))
      ) +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = axis_label_or_default(input$cohort_x_label, "Grade Level"),
        y = axis_label_or_default(input$cohort_y_label, "Performance Value")
      ) +

      dashboard_chart_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1.0),
        axis.text.y = element_text(angle = 0, hjust = 1.0),
        axis.title.y = element_text(margin = margin(r = 20)),
        panel.grid.minor = element_blank()
      )
  })

  # Cohort data reactive function (for compatibility with existing chart render code)
  cohort_data <- reactive({
    # Check if required inputs are available
    if (
      is.null(input$cohort_grade_var) ||
        input$cohort_grade_var == "" ||
        is.null(input$cohort_start_grade) ||
        is.null(input$cohort_end_grade) ||
        is.null(input$cohort_start_year) ||
        is.null(input$cohort_end_year)
    ) {
      return(data.frame())
    }

    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }

    # Remove rows with NA values
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if (nrow(data) == 0) {
      return(data.frame())
    }

    grade_var <- input$cohort_grade_var
    start_grade <- as.numeric(input$cohort_start_grade)
    end_grade <- as.numeric(input$cohort_end_grade)
    start_year <- as.numeric(input$cohort_start_year)
    end_year <- as.numeric(input$cohort_end_year)

    # Calculate years to track
    usable_years <- seq(from = start_year, to = end_year)
    years_to_track <- length(usable_years)
    grades <- seq(from = start_grade, by = 1, length.out = years_to_track)

    # Build cohort progression data
    final_result <- data.frame()

    for (i in 1:length(usable_years)) {
      current_year <- usable_years[i]
      current_grade <- grades[i]

      # Handle year matching - extract year from date objects
      year_matches <- if ("year" %in% names(data)) {
        # Extract year from date objects if needed
        if (inherits(data$year, c("Date", "POSIXct", "POSIXlt"))) {
          year(data$year) == current_year
        } else {
          data$year == current_year
        }
      } else if ("date" %in% names(data)) {
        year(data$date) == current_year
      } else {
        rep(TRUE, nrow(data))
      }

      # Handle grade matching using selected column
      grade_matches <- if (is.numeric(data[[grade_var]])) {
        as.numeric(data[[grade_var]]) == current_grade
      } else {
        grade_text_patterns <- paste0("\\b", current_grade, "\\b")
        grepl(grade_text_patterns, data[[grade_var]], ignore.case = TRUE)
      }

      # Filter for this year-grade combination
      year_data <- data[year_matches & grade_matches, ]

      if (nrow(year_data) > 0) {
        final_result <- rbind(final_result, year_data)
      }
    }

    if (nrow(final_result) == 0) {
      return(data.frame())
    }

    # Add progression column for x-axis
    final_result$progression <- paste0(
      "Year ",
      final_result$year,
      " (Grade ",
      final_result[[grade_var]],
      ")"
    )

    # Sort by year to ensure correct order
    final_result <- final_result %>%
      arrange(if ("year" %in% names(final_result)) year else date)

    return(final_result)
  })

  chart_plot_server("control_chart", control_plot)
  chart_plot_server("trended_chart", trended_plot)
  chart_plot_server("cohort_chart", cohort_plot)

  chart_download_server("run_download", run_plot, "run_chart")
  chart_download_server("line_download", line_plot, "line_chart")
  chart_download_server("control_download", control_plot, "expectation_chart")
  chart_download_server("bar_download", bar_plot, "bar_chart")
  chart_download_server("trended_download", trended_plot, "trended_expectation_chart")
  chart_download_server("cohort_download", cohort_plot, "cohort_chart")

  # Observe uploaded data and create filtering controls
  observeEvent(raw_data(), {
    data <- raw_data()
    req(data)

    # Clear existing filter controls
    removeUI(selector = "#filter_controls > *", multiple = TRUE)

    # Get column names excluding value columns
    value_patterns <- c(
      "^value$",
      "^pct$",
      "^percent$",
      "^amount$",
      "^count$"
    )
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[
      !grepl(value_pattern, names(data), ignore.case = TRUE)
    ]
    if ("observation" %in% names(data)) filter_columns <- setdiff(filter_columns, "date")

    # Populated for any categorical column whose choice list is large enough to
    # need server-side selectize; applied via updateSelectizeInput() below,
    # after the placeholder controls exist in the DOM.
    large_dropdown_choices <- list()

    # Create dynamic filter controls
    filter_controls <- map(filter_columns, function(col) {
      if (inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
        # Date columns get date sliders, but only when at least one real date exists.
        date_values <- data[[col]][!is.na(data[[col]])]

        if (length(date_values) == 0) {
          return(NULL)
        }

        min_date <- min(date_values, na.rm = TRUE)
        max_date <- max(date_values, na.rm = TRUE)

        column(
          4,
          sliderInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            min = min_date,
            max = max_date,
            value = c(min_date, max_date),
            timeFormat = "%Y-%m-%d",
            step = 1
          )
        )
      } else if (is.numeric(data[[col]])) {
        # Numeric columns get range sliders, but only when at least one real number exists.
        numeric_values <- data[[col]][!is.na(data[[col]])]

        if (length(numeric_values) == 0) {
          return(NULL)
        }

        min_num <- min(numeric_values, na.rm = TRUE)
        max_num <- max(numeric_values, na.rm = TRUE)
        step_num <- if (isTRUE(all.equal(min_num, max_num))) 1 else (max_num - min_num) / 100

        column(
          2,
          sliderInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            min = min_num,
            max = max_num,
            value = c(min_num, max_num),
            step = step_num,
            sep = ""
          )
        )
      } else {
        # Categorical columns get multi-select
        unique_values <- unique(data[[col]][!is.na(data[[col]])])
        unique_values <- sort(as.character(unique_values))

        choices_list <- c(
          "All" = "ALL_VALUES",
          "None" = "NO_VALUES",
          setNames(unique_values, unique_values)
        )

        input_id <- paste0("filter_", make.names(col))
        is_large <- length(choices_list) > LARGE_DROPDOWN_THRESHOLD

        if (is_large) {
          large_dropdown_choices[[input_id]] <<- choices_list
        }

        column(
          2,
          selectizeInput(
            inputId = input_id,
            label = col,
            # Large columns start with just the two sentinel choices; the full
            # value list is loaded server-side just after insertUI() below, so
            # the browser never receives thousands of <option> values up front.
            choices = if (is_large) choices_list[c("All", "None")] else choices_list,
            selected = "ALL_VALUES",
            multiple = TRUE
          )
        )
      }
    })

    # Insert filter controls
    insertUI(
      selector = "#filter_controls",
      where = "beforeEnd",
      ui = fluidRow(filter_controls)
    )

    for (input_id in names(large_dropdown_choices)) {
      updateSelectizeInput(
        session,
        input_id,
        choices = large_dropdown_choices[[input_id]],
        selected = "ALL_VALUES",
        server = TRUE
      )
    }
  })

  # Observer for "All" and "None" selection logic
  observe({
    data <- raw_data()
    req(data)

    value_patterns <- c(
      "^value$",
      "^pct$",
      "^percent$",
      "^amount$",
      "^count$"
    )
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[
      !grepl(value_pattern, names(data), ignore.case = TRUE)
    ]
    if ("observation" %in% names(data)) filter_columns <- setdiff(filter_columns, "date")

    for (col in filter_columns) {
      if (
        !is.numeric(data[[col]]) &&
          !inherits(data[[col]], "Date") &&
          !lubridate::is.Date(data[[col]])
      ) {
        filter_input_name <- paste0("filter_", make.names(col))

        observeEvent(
          input[[filter_input_name]],
          {
            current_selection <- input[[filter_input_name]]

            if (!is.null(current_selection)) {
              # updateSelectizeInput (not updateSelectInput) is required here:
              # some of these filters were initialized with server-side
              # selectize (see large_dropdown_choices above), and only
              # updateSelectizeInput reliably updates the selected value for
              # those.
              if (
                "ALL_VALUES" %in%
                  current_selection &&
                  length(current_selection) > 1
              ) {
                updateSelectizeInput(
                  session,
                  filter_input_name,
                  selected = "ALL_VALUES"
                )
              } else if (
                "NO_VALUES" %in%
                  current_selection &&
                  length(current_selection) > 1
              ) {
                updateSelectizeInput(
                  session,
                  filter_input_name,
                  selected = "NO_VALUES"
                )
              }
            }
          },
          ignoreInit = TRUE
        )
      }
    }
  })

  apply_current_filters <- function(data) {
    value_patterns <- c(
      "^value$",
      "^pct$",
      "^percent$",
      "^amount$",
      "^count$"
    )
    value_pattern <- paste(value_patterns, collapse = "|")
    filter_columns <- names(data)[
      !grepl(value_pattern, names(data), ignore.case = TRUE)
    ]
    if ("observation" %in% names(data)) filter_columns <- setdiff(filter_columns, "date")

    filtered <- data

    for (col in filter_columns) {
      filter_input_name <- paste0("filter_", make.names(col))
      filter_value <- input[[filter_input_name]]

      if (!is.null(filter_value)) {
        # Date filtering
        if (inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
          if (is_full_filter_range(data[[col]], filter_value)) {
            next
          }
          start_date <- filter_value[1]
          end_date <- filter_value[2]
          filtered <- filtered %>%
            filter(!!sym(col) >= start_date & !!sym(col) <= end_date)
        } else if (is.numeric(data[[col]])) {
          if (is_full_filter_range(data[[col]], filter_value)) {
            next
          }
          # Numeric filtering
          filtered <- filtered %>%
            filter(
              !!sym(col) >= filter_value[1] & !!sym(col) <= filter_value[2]
            )
        } else {
          # Categorical filtering
          if ("ALL_VALUES" %in% filter_value) {
            next
          } else if (
            "NO_VALUES" %in% filter_value && length(filter_value) == 1
          ) {
            filtered <- filtered %>% filter(FALSE)
          } else {
            actual_values <- filter_value[
              !filter_value %in% c("ALL_VALUES", "NO_VALUES")
            ]
            if (length(actual_values) > 0) {
              filtered <- filtered %>%
                filter(as.character(!!sym(col)) %in% actual_values)
            } else {
              filtered <- filtered %>% filter(FALSE)
            }
          }
        }
      }
    }

    filtered
  }

  # Reactive filtered data with IMPROVED data processing
  filtered_data <- reactive({
    data <- raw_data()
    req(data)
    apply_current_filters(data)
  })

  # One shared analytical input. Canonical and flat-file tools receive the exact
  # filtered rows; downstream tools may calculate statistics but do not perform
  # additional filtering, regrouping, or aggregation.
  analytical_data <- reactive({
    filtered_data()
  })

  # Render data table
  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    req(data)

    if ("observation" %in% names(data)) {
      data <- dplyr::select(data, -date, -dplyr::any_of("Year"))
    }

    DT::datatable(
      data,
      options = list(
        pageLength = as.numeric(input$rows_display),
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
}
