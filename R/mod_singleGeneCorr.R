# singleGeneCorr UI Function
mod_singleGeneCorr_ui <- function(module_name, appdata, global, module_config) {
  singleGeneCorr_tab(sampleClassInputs(global$sample_classes, module_name),
                     geneSelectInput(NULL, module_name),
                     module_config$colour_variables,
                     module_config$tabs,
                     module_config$advanced,
                     module_name)
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
#' @noRd
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
            advanced = NULL,
            id = NULL) {

  ns <- NS(id)
  tabPanel(
     title = "Single gene",
     value = "singleGeneCorr",
     tags$h5("Correlation between a selected gene and clinical variables"),
     splitLayout(
       verticalLayout(
         wellPanel(
            gene_select,
            sample_select,
            selectizeInput(
              ns("colour_variable"),
              label = "Select colour:",
              choices = c("None" = "", colours),
              options = list(allowEmptyOption = TRUE)
            ),
            advanced_settings_inputs(advanced, id)
         )
       ),
       verticalLayout(
          conditionalPanel(
             paste0("output[\'", ns('error_message'), "\'] == true"),
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
mod_singleGeneCorr_server <-function(module_name,
                                     appdata,
                                     global,
                                     module_config) {
  moduleServer(module_name, function(input, output, session) {
   ns <- session$ns

   clinical <- appdata$clinical
   expression_matrix <- appdata$expression_matrix
   sample_lookup <- appdata$sample_lookup
   
   subject_col <- global$subject_col
   sample_col <- global$sample_col
   sample_classes <- global$sample_classes
   
   # Load genes server side
   updateSelectizeInput(session,
                        "selected_gene",
                        choices = rownames(expression_matrix),
                        selected = "",
                        server = TRUE)
   # UI updates from URL
   observeEvent(session$userData$singleGeneCorr, {
      params <- session$userData$singleGeneCorr
      for (sample_class in global$sample_classes) {
         sc_name <- sample_class$name
         if (not_null(params[[sc_name]])) {
            updateSelectizeInput(session,
                                 sc_name,
                                 selected = params[[sc_name]])
         }
      }
      if (not_null(params$gene)) {
         updateSelectizeInput(session,
                              "selected_gene",
                              choices = rownames(expression_matrix),
                              selected = params$gene,
                              server = TRUE)
      }
   })
   
   outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                          "IQR" = valuesInsideTukeyFences,
                          "No" = function(x) TRUE)
   
   user_selection <- reactive({
      getSelectedSampleClasses(sample_classes, input)
   })
   
   observe({
     req(input$selected_gene)
     if (isTruthy(input$colour_variable)) {
       colour_var <- input$colour_variable
     } else {
       colour_var <- NULL
     }
     selected_gene <- input$selected_gene
     clinical_outliers <- input$clinical_outliers %||% "No"
     expression_outliers <- input$expression_outliers %||% "No"
     correlation_method <- input$correlation_method %||% "pearson"
     fit_method <- input$fit_method %||% "linear"
     
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
         corr_df[["ClinicalVariable"]] <- factor(corr_df[["ClinicalVariable"]],
                                                 levels = output_vars)
         
         if (not_null(colour_var)) {
           combined_df <- combined_df %>%
             pivot_longer(c(-.data$Expression, -.data[[colour_var]]),
                          names_to = "ClinicalVariable", values_to = "Value")
         } else {
           combined_df <- combined_df %>% 
             pivot_longer(c(-.data$Expression),
                          names_to = "ClinicalVariable", values_to = "Value")
         }
         
         combined_df[["ClinicalVariable"]] <-
            factor(combined_df[["ClinicalVariable"]], levels = output_vars)
         plotHeight <- ((length(output_vars) %/% 5) + 1) * 200
         plotWidth <- { 
            if (length(output_vars) < 4) length(output_vars)*200 else 800 
            }
         output[[output_name]] <- renderPlot({ 
           scatterplot <- plotClinExpScatterplot(combined_df,
                                  x = "Value",
                                  y = "Expression",
                                  facet_var = "ClinicalVariable",
                                  scales = output_scale,
                                  gene_name = input$selected_gene,
                                  ncol = 4, colour_variable = colour_var)
          scatterplot + 
             ggAnnotateCorr(corr_df, correlation_method) +
             ggAddFit(fit_method)
         }, width = plotWidth , height = plotHeight, bg = "transparent")
       })
     }
   })
  })
}