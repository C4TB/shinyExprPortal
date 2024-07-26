# multiMeasureCorr UI Function
mod_multiMeasureCorr_ui <- function(module_name, config, module_config) {
  multiMeasureCorr_tab(
    sample_select =
      sampleCategoryInputs(config$sample_categories,
                           module_name,
                           module_config$subset_categories),
    measures_variables = names(module_config$heatmap_variables),
    advanced = module_config$advanced,
    title = module_config$title,
    description = module_config$description,
    id = module_name
  )
}

#' Multiple measure corr tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param measures_variables subsets of measures variables for heatmap
#' @param advanced advanced options
#' @param title optional module title
#' @param description optional module description
#' @param id optional module ID
#'
#' @return a tab panel
#' @noRd
#'
multiMeasureCorr_tab <-
  function(sample_select,
           measures_variables,
           advanced = NULL,
           title = NULL,
           description = NULL,
           id = NULL) {
    ns <- NS(id)
    tabPanel(
      title = title %||% "Multiple measures",
      value = "multiMeasureCorr",
      tags$h5(
        description %||% "Correlation between all genes and measures"
      ),
      splitLayout(
        verticalLayout(
          wellPanel(
            selectizeInput(
              ns("heatmap_variables"),
              label = with_red_star("Select set of measures:"),
              choices = measures_variables,
              options = list(
                dropdownParent = "body",
                onInitialize = I('function(){this.setValue("");}')
              )
            ) %>%
              shinyhelper::helper(content = "multiMeasureCorr", size = "l"),
            tags$hr(),
            tags$b("Sample selection"),
            sample_select,
            tags$hr(),
            tags$b("Plot options"),
            numericInput(
              ns("max_pvalue"),
              label = "Maximum p-value for label: ",
              min = 0.0,
              max = 0.2,
              value = 0.05,
              step = 0.01,
              # dragRange = FALSE,
              width = "150px"
            ),
            checkboxInput(ns("use_padj"), "Use adjusted p-value?"),
            numericInput(
              ns("min_corr"),
              label = "Minimum correlation for label:",
              min = 0,
              max = 1,
              value = 0.25,
              step = 0.05,
              # dragRange = FALSE,
              width = "150px"
            ),
            advanced_settings_inputs(advanced, id)
          )
        ),
        verticalLayout(
          conditionalPanel(
            "input[\'heatmap_variables\'] == ''",
            ns = ns,
            tags$span(
              "Select a set of measures to view heatmap and table",
              style = "color: gray"
            )
          ),
          conditionalPanel(
            "input[\'heatmap_variables\'] != ''",
            ns = ns,
            vegawidget::vegawidgetOutput(
              ns("heatmap"),
              width = 800,
              height = 800
            ),
            hr(),
            DTOutput(ns("table")),
            downloadButton(
              ns("fulltable_download"),
              "Download as CSV with p-values"
            )
          )
        ),
        cellWidths = c("20%", "80%"),
        cellArgs = list(style = "white-space: normal;")
      )
    )
  }
#' multiMeasureCorr Server Function
#'
#' @noRd
mod_multiMeasureCorr_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    measures_data <- config$data$measures_data
    expression_matrix <- config$data$expression
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
    heatmap_variables <- module_config$heatmap_variables
    custom_heatmap_scheme <- module_config$custom_heatmap_scheme

    user_selection <- reactive({
      getSelectedSampleCategories(sample_categories, input, subset_categories)
    })

    selected_lookup <- reactive({
      sel_lookup <- selectMatchingValues(sample_lookup, user_selection())
      validate(need(
        nrow(sel_lookup) > 0,
        "No data for selected parameters."
      ))
      sel_lookup %>%
        dplyr::arrange(.data[[subject_var]])
    })

    expression_from_lookup <- reactive({
      samples <- selected_lookup()[[sample_var]]
      samples <- samples[!is.na(samples)]
      expression_matrix[, samples]
    })

    measures_from_lookup <- reactive({
      sel_lookup <- selected_lookup()
      selectFromLookup(measures_data, sel_lookup,
        matching_col = subject_var
      ) %>%
        dplyr::arrange(.data[[subject_var]])
    })

    rank_suffix <- reactive({
      if (input$use_padj) "padj" else "pvalue"
    })

    heatmap_data <- reactive({
      req(input$heatmap_variables)

      measures_outliers <-
        input$measures_outliers %||% default_measures_outliers %||% "No"
      expression_outliers <-
        input$expression_outliers %||% default_expr_outliers %||% "No"
      correlation_method <-
        input$correlation_method %||% default_corr_method %||% "pearson"

      validate(need(
        nrow(selected_lookup()) > 0,
        "No data for selected parameters."
      ))
      selected_measures <- measures_from_lookup()
      selected_expression <- expression_from_lookup()

      # Get subset of variables selected by user
      selected_measures_vars <- heatmap_variables[[input$heatmap_variables]]
      cols_lv <- colnames(selected_measures) %in% selected_measures_vars
      subset_measures <- selected_measures[, cols_lv]

      all_na_lv <-
        vapply(colnames(subset_measures),
               function(x) all(is.na(subset_measures[[x]])),
               logical(1))
      subset_measures <- subset_measures[, !all_na_lv]
      # Apply outlier functions to measures
      subset_measures <-
        replaceFalseWithNA(
          subset_measures,
          outlier_functions(measures_outliers)
        )
      # Apply outlier functions to expression
      selected_expression <-
        replaceFalseWithNA(
          t(selected_expression),
          outlier_functions(expression_outliers)
        )

      corr_df <- correlateMatrices(
        x = subset_measures,
        y = selected_expression,
        adjust_method = adjust_method,
        method = correlation_method,
        rowname_var = "Gene",
        cores = cores
      )
      rank_suffix <- if (input$use_padj) "padj" else "pvalue"
      pvaluesrank <-
        do.call(pmin, c(corr_df[, endsWith(colnames(corr_df), rank_suffix), drop = FALSE],
          na.rm = TRUE
        ))
      combined_df <- cbind(corr_df, pvaluesrank)
      combined_df <- combined_df[order(combined_df$pvaluesrank), ]
      combined_df
    }) %>% bindCache(
      input$heatmap_variables,
      input$measures_outliers,
      input$expression_outliers,
      input$correlation_method,
      selected_lookup()
    )

    output$heatmap <- vegawidget::renderVegawidget({
      hm <- heatmap_data()[seq_len(50), ] %>%
        correlationResultsToLong("Gene", "Measures", TRUE)
      vega_heatmap(
        hm,
        "Measures",
        "Gene",
        "estimate",
        input$max_pvalue,
        input$min_corr,
        input$use_padj,
        custom_heatmap_scheme
      ) %>%
        vegawidget::vw_autosize(800, 800)
    })

    output$table <- DT::renderDataTable({
        df <-
          corrResultsToTable(heatmap_data(), input$max_pvalue, input$use_padj,
                             rowname_col = "Gene")
        if (!is.null(link_to)) {
          isolate({
            list_of_values <- user_selection()
            baseURL <- buildURL(list_of_values, paste0("?tab=", link_to))
            df$Gene <- urlVector(df$Gene, "gene", baseURL)
          })
        }
        df
      },
      options = list(scrollX = TRUE),
      caption = "Significant correlations highlighted in bold",
      escape = FALSE,
      rownames = FALSE
    )

    output$fulltable_download <- downloadHandler(
      filename = function() {
        list_of_values <-
          getSelectedSampleCategories(sample_categories, input)
        paste(c(list_of_values, "heatmap_data"), collapse = "_")
      },
      content = function(file) {
        utils::write.csv(heatmap_data(), file, row.names = FALSE)
      }, contentType = "text/csv"
    )
  })
}
