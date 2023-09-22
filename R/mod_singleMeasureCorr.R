utils::globalVariables("where")
# singleMeasureCorr UI Function
mod_singleMeasureCorr_ui <- function(module_name, config, module_config) {
  var_list <- module_config$measures_variables %||%
    names(config$data$measures_data %>% dplyr::select(where(is.numeric)))

  singleMeasureCorr_tab(
    sampleCategoryInputs(config$sample_categories,
                         module_name,
                         module_config$subset_categories),
    varsSelectInput(var_list, module_name),
    module_config$advanced,
    module_config$title,
    module_config$description,
    module_name
  )
}
#' All genes correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param vars_select select input for measures
#' @param advanced  boolean flag to show or hide advanced options
#' @param title optional module title
#' @param description optional module description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
singleMeasureCorr_tab <- function(sample_select,
                                   vars_select,
                                   advanced = NULL,
                                   title = NULL,
                                   description = NULL,
                                   id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "Single measure",
    value = "singleMeasureCorr",
    tags$h5(description %||%
      "Correlation between all genes and a selected measure"),
    splitLayout(
      verticalLayout(
        wellPanel(
          vars_select,
          tags$hr(),
          tags$b("Sample selection"),
          sample_select,
          advanced_settings_inputs(advanced, id)
        )
      ),
      verticalLayout(
        conditionalPanel(
          "input[\'selected_variable\'] == ''",
          ns = ns,
          tags$span("No measure selected", style = "color: gray")
        ),
        DTOutput(ns("fulltable")),
        conditionalPanel(
          "input[\'selected_variable\'] != ''",
          ns = ns,
          downloadButton(ns("fulltable_download"), "Download Table CSV")
        )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}
#' singleMeasureCorr Server Function
#'
#' @importFrom stats na.omit
#' @noRd
mod_singleMeasureCorr_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    measures <- config$data$measures
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    sample_categories <- config$sample_categories

    cores <- config$nthreads

    adjust_method <- config$adjust_method

    default_measures_outliers <- config$default_measures_outliers
    default_expr_outliers <- config$default_expression_outliers
    default_corr_method <- config$default_correlation_method

    subset_categories <- module_config$subset_categories

    link_to <- module_config$link_to

    user_selection <- reactive({
      getSelectedSampleCategories(sample_categories, input, subset_categories)
    })

    selected_lookup <- reactive({
      sel_lookup <- selectMatchingValues(sample_lookup, user_selection())
      validate(need(
        nrow(sel_lookup) > 0,
        "No data for selected parameters."
      ))
      sel_lookup
    })

    expression_from_lookup <- reactive({
      samples <- selected_lookup()[[sample_var]]
      samples <- samples[!is.na(samples)]
      expression_matrix[, samples]
    })

    measures_from_lookup <- reactive({
      req(input$selected_variable)
      selectFromLookup(measures,
        selected_lookup(),
        matching_col = subject_var,
        return_col = input$selected_variable
      )
    })

    correlation_table <- reactive({
      # Compute correlation table, steps:
      # 1) Apply outlier filters
      # 2) Compute correlation matrix

      measures_outliers <-
        input$measures_outliers %||% default_measures_outliers %||% "No"
      expression_outliers <-
        input$expression_outliers %||% default_expr_outliers %||% "No"
      correlation_method <-
        input$correlation_method %||% default_corr_method %||% "pearson"

      list_of_values <- user_selection()
      selected_measures <- measures_from_lookup()
      selected_expression <- expression_from_lookup()
      validate(need(
        is.numeric(selected_measures),
        "Selected variable is not numeric"
      ))

      # Apply outlier filters
      # And transpose expression matrix
      selected_expression <-
        replaceFalseWithNA(
          t(selected_expression),
          outlier_functions(expression_outliers)
        )
      selected_measures <-
        replaceFalseWithNA(
          selected_measures,
          outlier_functions(measures_outliers)
        )
      validate(need(!all(is.na(selected_measures)),
                    "Selected subset has no data for chosen variable"))
      # Use the transposed expression to multiple columns vs vector
      correlation_df <- correlateMatrices(
        y = selected_expression,
        x = selected_measures,
        adjust_method = adjust_method,
        method = correlation_method,
        cores = cores
      )
      names(correlation_df) <- c("Gene", "Estimate", "P", "P_adj")

      if (!is.null(link_to)) {
        baseURL <- buildURL(list_of_values, paste0("?tab=", link_to))
        correlation_df$Gene <- urlVector(correlation_df$Gene, "gene", baseURL)
      }
      correlation_df
    }) %>% bindCache(
      input$selected_variable,
      input$measures_outliers,
      input$expression_outliers,
      input$correlation_method,
      selected_lookup()
    )

    output$fulltable <- renderDT({
        correlation_table()
      },
      filter = "top",
      rownames = FALSE,
      escape = FALSE,
      options = list(
        order = list(list(2, "asc"))
      )
    )

    output$fulltable_download <- downloadHandler(
      filename = function() {
        paste(c(user_selection(), "correlation_table"), collapse = "_")
      },
      content = function(file) {
        utils::write.csv(correlation_table(), file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
  })
}
