utils::globalVariables("where")
# singleVariableCorr UI Function
mod_singleVariableCorr_ui <-
  function(module_name,
           appdata,
           global,
           module_config)  {
    singleVariableCorr_tab(
      sampleClassInputs(global$sample_classes, module_name),
      varsSelectInput(names(
        appdata$clinical %>% dplyr::select(where(is.numeric))
        ),
        module_name),
      module_config$advanced,
      module_name
    )
  }
#' All genes correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param vars_select select input for clinical variables
#' @param advanced  boolean flag to show or hide advanced options
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
singleVariableCorr_tab <- function(sample_select,
                                   vars_select,
                                   advanced = NULL,
                                   id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "Single variable",
    value = "singleVariableCorr",
    tags$h5("Correlation between all genes and a selected clinical variable"),
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
        DT::DTOutput(ns("fulltable")),
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
#' @noRd 
mod_singleVariableCorr_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns 
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression_matrix
    sample_lookup <- appdata$sample_lookup
    sample_classes <- global$sample_classes
    subject_col <- global$subject_col
    sample_col <- global$sample_col
    
    link_to <- module_config$link_to
    
    outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                           "IQR" = valuesInsideTukeyFences,
                           "No" = function(x) TRUE)
    
    
    user_selection <- reactive({
      getSelectedSampleClasses(sample_classes, input)
    })
    
    correlation_table <- reactive({
      req(input$selected_variable)
      selected_variable <- input$selected_variable
      clinical_outliers <- input$clinical_outliers %||% "No"
      expression_outliers <- input$expression_outliers %||% "No"
      correlation_method <- input$correlation_method %||% "pearson"
      
      list_of_values <- user_selection()
    # Return subset of lookup based on the user selection of sample classes
      selected_lookup <- selectMatchingValues(sample_lookup, list_of_values)
      validate(need(nrow(selected_lookup) > 0, "No data for selected parameters."))
      selected_clinical <- selectFromLookup(clinical, selected_lookup,
                                            matching_col = subject_col,
                                            return_col = selected_variable)
      validate(need(is.numeric(selected_clinical),
                    "Selected variable is not numeric"))
      
      # Apply outlier filters
      selected_expression <- 
        replaceFalseWithNA(na.omit(expression_matrix[, selected_lookup[[sample_col]]]),
                           outlier_functions[[expression_outliers]])
      selected_clinical <- 
        replaceFalseWithNA(selected_clinical,
                           outlier_functions[[clinical_outliers]])

      correlation_df <- correlateMatrices(t(selected_expression),
                                                  selected_clinical,
                                                  method = correlation_method)
      names(correlation_df) <- c("Gene", "Estimate", "p value", "q value")

      if (not_null(link_to)) {
        baseURL <- buildURL(list_of_values, paste0("?tab=", link_to))
        correlation_df$Gene <- unlist(sapply(correlation_df$Gene,
                    function(x) paste0('<a href="',
                                       appendToURL(baseURL, "gene", x),
                                       '">',x,'</a>'), simplify = FALSE))
      }
      correlation_df
    })
    
    output$fulltable <- DT::renderDT({
      correlation_table()
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE)
    
    output$fulltable_download <- downloadHandler(
      filename = function() {
        list_of_values <-
          getSelectedSampleClasses(sample_classes, input)
        paste(c(list_of_values, "correlation_table"), collapse = "_") 
        },
      content = function(file) {
        utils::write.csv(correlation_table(), file, row.names = FALSE) 
      },contentType = 'text/csv')
    
    
  })
}