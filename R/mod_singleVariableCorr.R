utils::globalVariables("where")
# singleVariableCorr UI Function
mod_singleVariableCorr_ui <- function(module_name, config, module_config) {
    
    var_list <- module_config$clinical_variables %||%
      names(config$data$clinical %>% dplyr::select(where(is.numeric)))
    
    singleVariableCorr_tab(
      sampleCategoryInputs(config$sample_categories, module_name),
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
#' @param vars_select select input for clinical variables
#' @param advanced  boolean flag to show or hide advanced options
#' @param title optional module title
#' @param description optional module description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
singleVariableCorr_tab <- function(sample_select,
                                   vars_select,
                                   advanced = NULL,
                                   title = NULL,
                                   description = NULL,
                                   id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "Single variable",
    value = "singleVariableCorr",
    tags$h5(description %||% 
              "Correlation between all genes and a selected clinical variable"),
    splitLayout(
      verticalLayout(
        wellPanel(
          vars_select,
          sample_select,
          advanced_settings_inputs(advanced, id)
        )
      ),
      verticalLayout(
        conditionalPanel(
          paste0('input[\'', ns('selected_variable'), "\'] == ''"),
          tags$span("No clinical variable selected", style = "color: gray")
        ),
        DTOutput(ns("fulltable")),
        conditionalPanel(
          paste0('input[\'', ns('selected_variable'), "\'] != ''"),
          downloadButton(ns("fulltable_download"), "Download Table CSV")
        )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = 'white-space: normal;')
    )
  )
}
#' singleVariableCorr Server Function
#'
#' @importFrom stats na.omit
#' @noRd 
mod_singleVariableCorr_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns 
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    sample_categories <- config$sample_categories
    
    link_to <- module_config$link_to
    
    outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                           "IQR" = valuesInsideTukeyFences,
                           "No" = function(x) TRUE)
    
    user_selection <- reactive({
      getSelectedSampleCategories(sample_categories, input)
    })
    
    selected_lookup <- reactive({
      sel_lookup <- selectMatchingValues(sample_lookup, user_selection())
      validate(need(nrow(sel_lookup) > 0,
                    "No data for selected parameters."))
      sel_lookup
    })
    
    expression_from_lookup <- reactive({
      expression_matrix[, selected_lookup()[[sample_var]]]
    })
    
    clinical_from_lookup <- reactive({
      req(input$selected_variable)
      selectFromLookup(clinical,
                       selected_lookup(),
                       matching_col = subject_var,
                       return_col = input$selected_variable)
    })
    
    correlation_table <- reactive({
      # Compute correlation table, steps:
      # 1) Apply outlier filters
      # 2) Compute correlation matrix
      
      clinical_outliers <- input$clinical_outliers %||% "No"
      expression_outliers <- input$expression_outliers %||% "No"
      correlation_method <- input$correlation_method %||% "pearson"
      
      list_of_values <- user_selection()
      selected_clinical <- clinical_from_lookup()
      selected_expression <- expression_from_lookup()
      validate(need(is.numeric(selected_clinical),
                    "Selected variable is not numeric"))
      
      # Apply outlier filters
      selected_expression <-
        replaceFalseWithNA(na.omit(selected_expression),
                           outlier_functions[[expression_outliers]])
      selected_clinical <- 
        replaceFalseWithNA(selected_clinical,
                           outlier_functions[[clinical_outliers]])

      # Use the transposed expression to multiple columns vs vector
      correlation_df <- correlateMatrices(t(selected_expression),
                                                  selected_clinical,
                                                  method = correlation_method)
      names(correlation_df) <- c("Gene", "Estimate", "P", "FDR")

      if (not_null(link_to)) {
        baseURL <- buildURL(list_of_values, paste0("?tab=", link_to))
        correlation_df$Gene <- urlVector(correlation_df$Gene, "gene", baseURL)
      }
      correlation_df
    })
    
    output$fulltable <- renderDT({
        correlation_table()
      },
      filter = "top",
      rownames = FALSE,
      escape = FALSE
    )
    
    output$fulltable_download <- downloadHandler(
      filename = function() {
        paste(c(user_selection(), "correlation_table"), collapse = "_") 
        },
      content = function(file) {
        utils::write.csv(correlation_table(), file, row.names = FALSE) 
      },
      contentType = 'text/csv'
    )
    
    
  })
}
