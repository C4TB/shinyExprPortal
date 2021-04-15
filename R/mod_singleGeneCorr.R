#' singleGeneCorr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_singleGeneCorr_ui <- function(id, appdata) {
  singleGeneCorr_tab(sampleClassInputs(appdata$config$sample_classes, id),
                     geneSelectInput(NULL, id),
                     appdata$modules$singleGeneCorr$colour_variables,
                     appdata$modules$singleGeneCorr$tabs,
                     appdata$modules$singleGeneCorr$advanced,
                     id)
}
#' Single gene correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param gene_select select input with gene symbols
#' @param colours list of variables for colour selection
#' @param outputs configuration of tabs with plots
#' @param advanced boolean flag to show or hide advanced options 
#'    such as outlier removal
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @export
#'
#' @examples
#' if (interactive()) {
#'  sample_select <- radioButtons("sample_1", "Select sample", samples)
#'  gene_select <- selectizeInput("gene_selection", "Select gene", gene_list)
#'  output_list <- list("Vars" = c("Var1", "Var2"))
#'  singleGeneCorr_tab(sample_select, gene_select, c("Var1", "Var2"),
#'   output_list)
#' }
singleGeneCorr_tab <-
   function(sample_select,
            gene_select,
            colours,
            outputs,
            advanced = TRUE,
            id = NULL) {

  ns <- NS(id)
  tabPanel(title = "Single gene", value = "singleGeneCorr",
           splitLayout(
             verticalLayout(
               wellPanel(
               sample_select,
               gene_select,
               selectizeInput(
                 ns("colour_variable"),
                 label = "Select colour:",
                 choices = c("None" = "", colours),
                 options = list(allowEmptyOption = TRUE)
               ),
               if (advanced) {
                  tagList(
                     radioButtons(ns("correlation_method"),
                                  label = "Correlation method:",
                                  choices = c("Pearson" = "pearson",
                                              "Spearman" = "spearman",
                                              "Kendall" = "kendall"),
                                  selected = "pearson"),
                     radioButtons(ns("clinical_outliers"),
                                 label = "Remove clinical outliers?",
                                 choices = c("5/95 percentiles", "IQR", "No"),
                                 selected = "No"),
                     radioButtons(ns("expression_outliers"),
                                 label = "Remove expression outliers?",
                                 choices = c("5/95 percentiles", "IQR", "No"),
                                 selected = "No"),
                     radioButtons(ns("fit_method"),
                                  label = "Fitting method:",
                                  choices = c("Linear" = "linear",
                                              "Cubic" = "cubic"))
                     )
               } else NULL
               )
             ),
             verticalLayout(
                conditionalPanel(
                   paste0("output[\'", ns('error_message'), "\'] == true"),
                   #textOutput(ns("error_message"))
                   tags$span("Transcript not found in subset or
                             subset combination does not exist.",
                             style = "color: gray")
                ),
                conditionalPanel(
                   paste0("input[\'", ns('selected_gene'), "\'] == ''"),
                   tags$span("No gene selected", style = "color: gray")
                ),
                conditionalPanel(
                   paste0("input[\'",
                          ns('selected_gene'),
                          "\'] != ''",
                          "&& output[\'",
                          ns('error_message'),
                          "\'] == false"),
                   do.call(tabsetPanel, plotsTabPanels(outputs, ns))
                )
             ),
             cellWidths = c("20%", "80%"),
             cellArgs = list(style = "white-space: normal;")
           )
  )
}
#' singleGeneCorr Server Function
#'
#' @noRd
mod_singleGeneCorr_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session) {
   ns <- session$ns

   clinical <- appdata$data$clinical
   expression_matrix <- appdata$data$expression_matrix
   sample_lookup <- appdata$data$sample_lookup
   subject_col <- appdata$config$subject_col
   sample_col <- appdata$config$sample_col
   
   # Load genes server side
   updateSelectizeInput(session,
                        "selected_gene",
                        choices = rownames(expression_matrix),
                        selected = "",
                        server = TRUE)
   
   module_config <- appdata$modules$singleGeneCorr

   outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                          "IQR" = valuesInsideTukeyFences,
                          "No" = function(x) TRUE)
   
   user_selection <- reactive({
      getSelectedSampleClasses(appdata$config$sample_classes, input)
   })
   
   # selected_expression <- reactive({
   #    
   # })
   
   observe({
     req(input$selected_gene)
     if (isTruthy(input$colour_variable)) {
       colour_var <- input$colour_variable
     } else {
       colour_var <- NULL
     }
     selected_gene <- input$selected_gene
     clinical_outliers <- input$clinical_outliers
     expression_outliers <- input$expression_outliers
     correlation_method <- input$correlation_method %||% "spearman"
     fit_method <- input$fit_method
     
     list_of_values <- user_selection()
     # Return subset of lookup based on the user selection of sample classes
     selected_lookup <- selectMatchingValues(sample_lookup, list_of_values)
     subset_clinical <- selectFromLookup(clinical, selected_lookup,
                                         matching_col = subject_col)
     selected_expression <- expression_matrix[selected_gene,
                                              selected_lookup[[sample_col]]]
     if (all(is.na(selected_expression)) | length(selected_expression) == 0) {
        output$error_message <- reactive({ TRUE })
        outputOptions(output, "error_message", suspendWhenHidden = FALSE)
     } else {
        output$error_message <- reactive({ FALSE })
        outputOptions(output, "error_message", suspendWhenHidden = FALSE)
     }
     req(all(not_na(selected_expression)) & (length(selected_expression) > 0))  
     tab_output_list <- module_config$tabs
     

     # We go through the list of outputs defined in the configuration file
     # as they were also used to create pairs of tabPanel-plotOutput
     # Local scope is required otherwise the last tab_output will override 
     # previous ones
     for(i in seq_along(tab_output_list)) {
       local({
         tab_output <- tab_output_list[[i]]
         output_name <- tab_output$name
         output_scale <- tab_output$scale
         output_vars <- unique(tab_output$variables)
         
         # If a colour variable was provided AND it's not in subset yet, add it
         if (not_null(colour_var)) {
           if (!colour_var %in% output_vars) {
            subset_vars <- c(output_vars, colour_var)
           } else {
              subset_vars <- output_vars
           }
         } else {
            subset_vars <- output_vars
         }
         selected_clinical <- subset_clinical[, subset_vars]
         combined_df <- cbind(Expression = selected_expression,
                              selected_clinical)
         combined_df[, output_vars] <- 
           replaceFalseWithNA(combined_df[, output_vars],
                              outlier_functions[[clinical_outliers]])
         combined_df[, "Expression"] <-
           replaceFalseWithNA(combined_df[, "Expression"],
                              outlier_functions[[expression_outliers]])
         corr_df <- correlateMatrices(x = combined_df[, output_vars],
                                      y = combined_df$Expression,
                                      method = correlation_method)
         
# By default the first column returned by function above is named "variable"
# We need to match it with the data frame below for the plotting function
         colnames(corr_df)[1] <- "ClinicalVariable"
         
         if (not_null(colour_var)) {
           combined_df <- combined_df %>%
             pivot_longer(c(-.data$Expression, -.data[[colour_var]]),
                          names_to = "ClinicalVariable", values_to = "Value")
         } else {
           combined_df <- combined_df %>% 
             pivot_longer(c(-.data$Expression),
                          names_to = "ClinicalVariable", values_to = "Value")
         }
         plotHeight <- ((length(output_vars) %/% 5) + 1) * 200
         plotWidth <- { 
            if (length(output_vars) < 4) length(output_vars)*200 else 800 
            }
         output[[output_name]] <- renderPlot({ 
           plotClinExpScatterplot(combined_df,
                                  x = "Value",
                                  y = "Expression",
                                  facet_var = "ClinicalVariable",
                                  correlation_df = corr_df,
                                  scales = output_scale,
                                  fit_method = fit_method,
                                  correlation_method = correlation_method,
                                  gene_name = input$selected_gene,
                                  ncol = 4, colour_variable = colour_var)
         }, width = plotWidth , height = plotHeight)
       })
     }
   })
  })
}