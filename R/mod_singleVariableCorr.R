#' singleVariableCorr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_singleVariableCorr_ui <- function(id, appdata){
  singleVariableCorr_tab(sampleClassInputs(appdata$config$sample_classes, id), 
                        names(appdata$data$clinical),
                        id)
}
#' All genes correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param clinical_variables select input for clinical variables
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @export
#'
singleVariableCorr_tab <- function(sample_select,
                                   clinical_variables,
                                   id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "Single Variable",
    value = "singleVariableCorr",
    splitLayout(
      verticalLayout(wellPanel(
        sample_select,
        selectizeInput(
          ns("selected_variable"),
          label = with_red_star("Select a clinical variable"),
          choices = clinical_variables,
          options = list(
            dropdownParent = "body",
            onInitialize = I("function(){this.setValue(''); }")
          )
        )
      )),
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
mod_singleVariableCorr_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns 
    
    clinical <- appdata$data$clinical
    expression_matrix <- appdata$data$expression_matrix
    sample_lookup <- appdata$data$sample_lookup
    subject_col <- appdata$config$subject_col
    sample_col <- appdata$config$sample_col
    
    correlation_table <- reactive({
      req(input$selected_variable)
      selected_variable <- input$selected_variable
      list_of_values <- getSelectedSampleClasses(appdata$config$sample_classes,
                                                 input)
    # Return subset of lookup based on the user selection of sample classes
      selected_lookup <- selectMatchingValues(sample_lookup, list_of_values)
      selected_clinical <- selectFromLookup(clinical, selected_lookup,
                                            matching_col = subject_col,
                                            return_col = selected_variable)
      selected_expression <- expression_matrix[, selected_lookup[[sample_col]]]

      validate(need(is.numeric(selected_clinical),
                    "Selected variable is not numeric"))
      correlation_df <- correlateMatrices(t(selected_expression),
                                                  selected_clinical,
                                                  method = "spearman")
      names(correlation_df) <- c("Gene", "Estimate", "p value", "FDR")
      correlation_df
    })
    
    output$fulltable <- DT::renderDT({
      correlation_table()
    },
    caption = "Correlation between genes and selected variable",
    filter = "top")
    
    output$fulltable_download <- downloadHandler(
      filename = function() {
        list_of_values <-
          getSelectedSampleClasses(appdata$config$sample_classes, input)
        paste(c(list_of_values, "correlation_table"), collapse = "_") 
        },
      content = function(file) {
        utils::write.csv(correlation_table(), file, row.names = FALSE) 
      },contentType = 'text/csv')
    
    
  })
}