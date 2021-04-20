# singleVariableCorr UI Function
mod_singleVariableCorr_ui <- function(module_name, appdata, global, module_config)  {
  singleVariableCorr_tab(sampleClassInputs(global$sample_classes, module_name), 
                        names(appdata$clinical %>% dplyr::select(where(is.numeric))),
                        module_config$advanced,
                        module_name)
}
#' All genes correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param clinical_variables select input for clinical variables
#' @param advanced  boolean flag to show or hide advanced options
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @export
#'
singleVariableCorr_tab <- function(sample_select,
                                   clinical_variables,
                                   advanced = TRUE,
                                   id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "Single variable",
    value = "singleVariableCorr",
    splitLayout(
      verticalLayout(
        wellPanel(
          sample_select,
          selectizeInput(
            ns("selected_variable"),
            label = with_red_star("Select a clinical variable"),
            choices = clinical_variables,
            options = list(
              dropdownParent = "body",
              onInitialize = I("function(){this.setValue(''); }")
            )
          ),
          if (advanced) {
            tagList(
              radioButtons(
                ns("correlation_method"),
                label = "Correlation method:",
                choices = c(
                  "Pearson" = "pearson",
                  "Spearman" = "spearman",
                  "Kendall" = "kendall"
                ),
                selected = "pearson"
              ),
              outlier_inputs(id)
            )
          } else
            NULL
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
    
    outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                           "IQR" = valuesInsideTukeyFences,
                           "No" = function(x) TRUE)
    
    correlation_table <- reactive({
      req(input$selected_variable)
      selected_variable <- input$selected_variable
      clinical_outliers <- input$clinical_outliers
      expression_outliers <- input$expression_outliers
      correlation_method <- input$correlation_method %||% "spearman"
      
      list_of_values <- getSelectedSampleClasses(sample_classes,
                                                 input)
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
      correlation_df
    })
    
    output$fulltable <- DT::renderDT({
      correlation_table()
    },
    caption = "Correlation between genes and selected variable",
    filter = "top",
    rownames = FALSE)
    
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