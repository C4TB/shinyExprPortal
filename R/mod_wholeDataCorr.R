# wholeDataCorr UI Function
mod_wholeDataCorr_ui <- function(module_name, appdata, global, module_config) {
  wholeDataCorr_tab(
    sampleClassInputs(global$sample_classes, module_name),
    clinical_variables = names(module_config$heatmap_variables),
    advanced = module_config$advanced,
    module_name
  )
}

#' Whole data correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param clinical_variables 
#' @param advanced advanced options
#' @param id optional module ID
#'
#' @return a tab panel
#' @noRd
#'
#' @importFrom shinycssloaders withSpinner
wholeDataCorr_tab <- function(sample_select,
                              clinical_variables,
                              advanced = NULL,
                              id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "Whole data",
    value = "wholeDataCorr",
    tags$h5("Correlation between all genes and clinical variables"),
    splitLayout(
      verticalLayout(
        wellPanel(
          selectizeInput(
            ns("heatmap_variables"),
            label = with_red_star("Select set of clinical variables:"),
            choices = clinical_variables,
            options = list(
              dropdownParent = "body",
              onInitialize = I('function(){this.setValue("");}')
            )
          ),
          sample_select,
          numericInput(
            ns("max_pvalue"),
            label = "Maximum p-value for label: ",
            min = 0.0,
            max = 0.2,
            value = 0.05,
            step = 0.01,
            #dragRange = FALSE,
            width = "150px"
          ),
          numericInput(
            ns("min_corr"),
            label = "Minimum correlation for label:",
            min = 0,
            max = 1,
            value = 0.25,
            step = 0.05,
            #dragRange = FALSE,
            width = "150px"
          ),
          advanced_settings_inputs(advanced, id)
        )
      ),
      verticalLayout(
        plotOutput(ns("heatmap"), height = 800) %>% withSpinner(),
        conditionalPanel(
          paste0('input[\'', ns('heatmap_variables'), "\'] != ''"),
          downloadButton(ns("fulltable_download"), "Download Table CSV")
        )),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}
#' wholeDataCorr Server Function
#'
#' @noRd 
mod_wholeDataCorr_server <- function(module_name,
                                     appdata,
                                     global,
                                     module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression
    sample_lookup <- appdata$sample_lookup
    
    subject_col <- global$subject_col
    sample_col <- global$sample_col
    sample_classes <- global$sample_classes
    
    heatmap_variables <- module_config$heatmap_variables
    
    outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                           "IQR" = valuesInsideTukeyFences,
                           "No" = function(x) TRUE)

    heatmap_data <- reactive({
      req(input$heatmap_variables)
      
      clinical_outliers <- input$clinical_outliers %||% "No"
      expression_outliers <- input$expression_outliers %||% "No"
      correlation_method <- input$correlation_method %||% "spearman"
      
      list_of_values <- getSelectedSampleClasses(sample_classes, input)
      # Return subset of lookup conditional on the user selection of sample classes
      selected_lookup <- selectMatchingValues(sample_lookup, list_of_values)
      validate(need(nrow(selected_lookup) > 0,
                    "No data for selected parameters."))
      subset_clinical <- selectFromLookup(clinical, selected_lookup,
                                          matching_col = subject_col)
      
      # Get subset of variables selected by user
      selected_clinical_vars <- heatmap_variables[[input$heatmap_variables]]
      
      selected_clinical <-
        subset_clinical[, colnames(subset_clinical) %in% selected_clinical_vars]
      selected_expression <- expression_matrix[, selected_lookup[[sample_col]]]
      
      #Apply outlier functions to clinical
      selected_clinical  <- 
        replaceFalseWithNA(selected_clinical,
                           outlier_functions[[clinical_outliers]])
      #Apply outlier functions to expression
      selected_expression <-
        replaceFalseWithNA(t(selected_expression),
                           outlier_functions[[expression_outliers]])
      corr_df <- correlateMatrices(selected_expression,
                                   selected_clinical,
                                   method = correlation_method,
                                   rowname_var = "Gene")
      pvalues_rank <- do.call(pmin,
                            c(corr_df[, endsWith(colnames(corr_df), "pvalue")],
                                   na.rm = TRUE)
                              )
      combined_df <- cbind(corr_df, pvalues_rank)
      combined_df <- combined_df[order(combined_df$pvalues_rank),]
      combined_df[1:50, ]
      
    })
    
    output$heatmap <- renderPlot({ 
      hm <- heatmap_data()
      plotCorrelationHeatmap(hm, -log10(input$max_pvalue), input$min_corr)
    })
    
    output$fulltable_download <- downloadHandler(
      filename = function() {
        list_of_values <-
          getSelectedSampleClasses(sample_classes, input)
        paste(c(list_of_values, "heatmap_data"), collapse = "_") 
      },
      content = function(file) {
        utils::write.csv(heatmap_data(), file, row.names = FALSE) 
      },contentType = 'text/csv')
    
  })
}