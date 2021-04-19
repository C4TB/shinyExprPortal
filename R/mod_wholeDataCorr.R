#' wholeDataCorr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_wholeDataCorr_ui <- function(id, appdata, global, module_config) {
  wholeDataCorr_tab(
    sampleClassInputs(global$sample_classes, id),
    clinical_variables = names(module_config$heatmap_variables),
    advanced = module_config$advanced,
    id
  )
}

#' Title
#'
#' @param sample_select  a
#' @param clinical_variables  b
#' @param advanced advanced options?
#' @param id optional module ID
#'
#' @return a tab panel
#' @export
#'
#' @importFrom shinycssloaders withSpinner
wholeDataCorr_tab <- function(sample_select, clinical_variables, advanced,
                                      id = NULL) {
  ns <- NS(id)
  tabPanel(title = "Whole Data Correlations", value = "wholeDataCorr",
           splitLayout(
             verticalLayout(
               wellPanel(
                 sample_select,
                 selectizeInput(
                   ns("heatmap_variables"),
                   label = with_red_star("Select set of clinical variables:"),
                   choices = clinical_variables,
                   options = list(dropdownParent = "body",
                                  onInitialize = I(
                                    'function(){this.setValue("");}'))
                 ),
                 numericInput(ns("min_pvalue"),
                                 label = "P-value (-log10) label threshold: ",
                                 min = 0.0,
                                 max = 50,
                                 value = 2,
                                 step = 1,
                                 #dragRange = FALSE,
                                 width = "150px"
                 ),
                 numericInput(ns("min_corr"),
                             label = "Correlation label threshold:",
                             min = 0,
                             max = 1,
                             value = 0,
                             step = 0.05,
                             #dragRange = FALSE,
                             width = "150px"
                 ),
                 if (advanced) { 
                   tagList(
                     radioButtons(ns("correlation_method"),
                                  label = "Correlation method:",
                                  choices = c("Pearson"= "pearson",
                                              "Spearman" = "spearman",
                                              "Kendall"= "kendall"),
                                  selected = "pearson"),
                     radioButtons(ns("clinical_outliers"), 
                                  label= "Remove clinical outliers?", 
                                  choices = c("5/95 percentiles", "IQR", "No"), 
                                  selected = "No"),
                     radioButtons(ns("expression_outliers"), 
                                  label = "Remove expression outliers?", 
                                  choices = c("5/95 percentiles", "IQR", "No"), 
                                  selected = "No")
                   )
                 } else NULL
               )
             ),
             verticalLayout(
               plotOutput(ns("heatmap"), height = 800) %>% withSpinner()
             ),
             cellWidths = c("20%", "80%"),
             cellArgs = list(style = "white-space: normal;")
           )
  )

  
}
    
#' wholeDataCorr Server Function
#'
#' @noRd 
mod_wholeDataCorr_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression
    sample_lookup <- appdata$sample_lookup
    
    subject_col <- global$subject_col
    sample_col <- global$sample_col
    sample_classes <- global$sample_classes
    
# Module configuration
# This indicates which variable should be used to match a suffix 
# (likely timepoint) of clinical variables with expression
# Variables in the clinical data should contain a suffix matching e.g. 
#    timepoint
    subset_clinical_variable <- module_config$subset_clinical_variable
    heatmap_variables <- module_config$heatmap_variables
    
    outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                           "IQR" = valuesInsideTukeyFences,
                           "No" = function(x) TRUE)

    heatmap_data <- reactive({
      req(input$heatmap_variables)
      
      clinical_outliers <- input$clinical_outliers
      expression_outliers <- input$expression_outliers
      correlation_method <- input$correlation_method %||% "spearman"
      
      list_of_values <- getSelectedSampleClasses(sample_classes, input)
      # Return subset of lookup conditional on the user selection of sample classes
      selected_lookup <- selectMatchingValues(sample_lookup, list_of_values)
      subset_clinical <- selectFromLookup(clinical, selected_lookup,
                                          matching_col = subject_col)
      
      subset_values <- getSubsetSampleClasses(c(subset_clinical_variable),
                                               sample_classes, input)
      # Get subset of variables selected by user
      selected_clinical_vars <-
        paste(heatmap_variables[[input$heatmap_variables]],
              subset_values,
              sep="_")
      
      selected_clinical <- subset_clinical[, selected_clinical_vars]
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
                              corr_df[, endsWith(colnames(corr_df), "pvalue")])
      combined_df <- cbind(corr_df, pvalues_rank)
      combined_df <- combined_df[order(combined_df$pvalues_rank),]
      combined_df[1:50, ]
      
    })
    
    output$heatmap <- renderPlot({ 
      hm <- heatmap_data()
      plotCorrelationHeatmap(hm, input$min_pvalue, input$min_corr)
      
    })
    
  })
}