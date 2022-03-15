# multiVariableCorr UI Function
mod_multiVariableCorr_ui <- function(module_name, config, module_config) {
  multiVariableCorr_tab(
    sample_select = sampleClassInputs(config$sample_classes, module_name),
    clinical_variables = names(module_config$heatmap_variables),
    advanced = module_config$advanced,
    title = module_config$title,
    description = module_config$description,
    id = module_name
  )
}

#' Whole data correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param clinical_variables 
#' @param advanced advanced options
#' @param title optional module title
#' @param description optional module description
#' @param id optional module ID
#'
#' @return a tab panel
#' @noRd
#'
multiVariableCorr_tab <-
  function(sample_select,
           clinical_variables,
           advanced = NULL,
           title = NULL,
           description = NULL,
           id = NULL) {
    
  ns <- NS(id)
  tabPanel(
    title = title %||% "Multiple variables",
    value = "multiVariableCorr",
    tags$h5(
      description %||% "Correlation between all genes and clinical variables"),
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
      
        bsplus::bs_accordion("multiVariableCorr_acc") %>% 
          bsplus::bs_append(title = "Heatmap Top 50 significant genes",
            plotly::plotlyOutput(ns("heatmap"), height = 800) %>% 
              shinycssloaders::withSpinner()
          ) %>%
          bsplus::bs_append(title = "Table",
          verticalLayout(
            DTOutput(ns("table")),
            conditionalPanel(
              paste0('input[\'', ns('heatmap_variables'), "\'] != ''"),
              downloadButton(ns("fulltable_download"), "Download Table CSV")
            )
          )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}
#' multiVariableCorr Server Function
#'
#' @noRd 
mod_multiVariableCorr_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression
    sample_lookup <- config$data$sample_lookup
    
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    sample_classes <- config$sample_classes
    
    link_to <- module_config$link_to
    heatmap_variables <- module_config$heatmap_variables
    
    outlier_functions <- c("5/95 percentiles" = valuesInsideQuantileRange,
                           "IQR" = valuesInsideTukeyFences,
                           "No" = function(x) TRUE)

    user_selection <- reactive({
      getSelectedSampleClasses(sample_classes, input)
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
      sel_lookup <- selected_lookup()
      selectFromLookup(clinical, sel_lookup,
                       matching_col = subject_var)
    })
    
    heatmap_data <- reactive({
      req(input$heatmap_variables)
      
      clinical_outliers <- input$clinical_outliers %||% "No"
      expression_outliers <- input$expression_outliers %||% "No"
      correlation_method <- input$correlation_method %||% "spearman"
      
      validate(need(nrow(selected_lookup()) > 0,
                    "No data for selected parameters."))
      selected_clinical <- clinical_from_lookup()
      selected_expression <- expression_from_lookup()
      
      # Get subset of variables selected by user
      selected_clinical_vars <- heatmap_variables[[input$heatmap_variables]]
      cols_lv <- colnames(selected_clinical) %in% selected_clinical_vars
      subset_clinical <- selected_clinical[, cols_lv]
      
      #Apply outlier functions to clinical
      subset_clinical  <- 
        replaceFalseWithNA(subset_clinical,
                           outlier_functions[[clinical_outliers]])
      #Apply outlier functions to expression
      selected_expression <-
        replaceFalseWithNA(t(selected_expression),
                           outlier_functions[[expression_outliers]])
      
      corr_df <- correlateMatrices(selected_expression,
                                   subset_clinical,
                                   method = correlation_method,
                                   rowname_var = "Gene")
      pvalues_rank <- do.call(pmin,
                            c(corr_df[, endsWith(colnames(corr_df), "pvalue")],
                                   na.rm = TRUE)
                              )
      combined_df <- cbind(corr_df, pvalues_rank)
      combined_df <- combined_df[order(combined_df$pvalues_rank),]
      combined_df
      
    })
    
    output$table <- DT::renderDataTable({
        df <- corrResultsToTable(heatmap_data(), input$max_pvalue)
        if (not_null(link_to)) {
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
      escape = F,
      rownames = FALSE)
    
    output$heatmap <- plotly::renderPlotly({ 
      hm <- heatmap_data()[1:50, ]
      plotly::ggplotly(
        plotCorrelationHeatmap(hm, -log10(input$max_pvalue), input$min_corr),
        tooltip = "text"
      ) %>%
        plotly::layout(xaxis = list(automargin = TRUE,
                                    tickfont = list(size = 9),
                                    side = "top"),
                       font = list(size = 5))
    })
    
    output$fulltable_download <- downloadHandler(
      filename = function() {
        list_of_values <-
          getSelectedSampleClasses(sample_classes, input)
        paste(c(list_of_values, "heatmap_data"), collapse = "_") 
      },
      content = function(file) {
        utils::write.csv(heatmap_data(), file, row.names = FALSE) 
      }, contentType = "text/csv")
    
  })
}