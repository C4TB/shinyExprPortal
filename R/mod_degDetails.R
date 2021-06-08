# degDetails UI Function
mod_degDetails_ui <- function(module_name, appdata, global, module_config) {
  models <- appdata$models
  category_variable <- module_config$category_variable
  categories <- unique(unlist(models[, category_variable]))
  degDetails_tab(categories,
                 module_config$title,
                 module_name)
}
#' Differentially expressed genes tab UI
#'
#' @param categories model categories
#' @param title optional title
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
degDetails_tab <- function(categories, title = NULL, id = NULL) {
  ns <- NS(id)
  tabPanel("Model results", value= "degDetails", 
           tags$h5(title %||% "Individual model results"),
           splitLayout(
             verticalLayout(
               wellPanel(
               radioButtons(
                 ns("model_category"),
                 label = "Select category:",
                 choices = categories,
                 selected = categories[[1]]
               ),
               radioButtons(
                 ns("selected_model"),
                 label = "Select model:",
                 choices = c("1")
               ),
               numericInput(
                 ns("fc_threshold"),
                 label = "FC threshold:",
                 value = 1,
                 min = 0.5,
                 max = 10,
                 step = 0.25
               ),
               numericInput(
                 ns("pvalue_threshold"),
                 label = "Significance threshold:",
                 value = 0.05,
                 min = 0,
                 max = 1,
                 step = 0.01
               ),
               radioButtons(
                 ns("pvalue_adjusted_flag"),
                    label = "FDR adjusted p-values?",
                    choices = list("No" = "p.value", "Yes" = "q.value"),
                    selected = "p.value"
               )
             )),
             bsplus::bs_accordion("deg_results") %>%
               bsplus::bs_append(title = "Plot",
                         content = splitLayout(
                                    style = "font-size: 75%;",
                                    plotly::plotlyOutput(ns("results_plot"),
                                        width = "700px",
                                        height = "500px") %>%
                                      shinycssloaders::withSpinner(),
                                    cellWidths = c(700, 200)
                        )) %>%
                bsplus::bs_append(title = "Table",
                         content = verticalLayout(
                           uiOutput(ns("ui_table_checkbox")),
                           DT::DTOutput(ns("deg_table")))
                         ),
             cellWidths = c("20%", "80%"),
             cellArgs = list(style = 'white-space: normal;')
           )
  )
}
#' degDetails Server Function
#'
#' @noRd 
mod_degDetails_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session){
    ns <- session$ns

    models <- appdata$models
    
    category_variable <- module_config$category_variable
    link_to <- module_config$link_to
    
    exc_columns <- c(category_variable,
                     c("pSignif", "qSignif", "File", "Data"))
    table_subset <- dplyr::select(models, -exc_columns)
    
    model_update <- reactiveVal(FALSE)
    
    # Update list of models after a category is selected
    observeEvent(input$model_category, { 
      selected_category_models <- models[
        models[, category_variable] == input$model_category, ] %>%
        dplyr::select(-exc_columns)
      model_update(TRUE)
      updateRadioButtons(
        session,
        "selected_model",
        choiceNames = do.call(paste, c(selected_category_models)),
        choiceValues = do.call(paste, c(selected_category_models, sep = "_"))
      )
    })
    
    observeEvent(input$selected_model, {
      model_update(TRUE)
    })
    
    # Combine selected model category with model
    # Model is a string separated by "_" that is split into a vector
    condition_list <- eventReactive(c(model_update(), input$selected_model), {
      model_update(FALSE)
      isolate({
        condition <- stats::setNames(unlist(strsplit(input$selected_model, "_")),
                              colnames(table_subset))
        condition[[category_variable]] <- input$model_category
      })
      condition
    })
    
    # Retrieve results by matching the columns from the table
    model_results <- reactive({
      condition <- condition_list()
      model_res <- list()
      # Iterate through multiple conditions to match rows
      for (var_name in names(condition)) {
        model_cond_res <- models[models[, var_name] == condition[var_name], ]
        model_res[[var_name]] <- model_cond_res
      }
      selected_model <- Reduce(
        function(x,y) inner_join(x, y, by = colnames(x)), model_res)
      req(nrow(selected_model) > 0)
      selected_model$Data[[1]]
    })
    
    signif_labels <- list("not significant", "log FC",
                          "%s", "log FC and %s")
    pvalue_labels <- list("p.value" = "p-value",
                         "q.value" = "q-value")
    
    vp_table <- reactive({
      table <- model_results()
      prepareModelResultsTable(
        table,
        input$fc_threshold,
        input$pvalue_threshold,
        input$pvalue_adjusted_flag
        )
    })
    
    output$results_plot <- plotly::renderPlotly({
      table <- vp_table()
      gene_column <- { if ("Gene" %in% colnames(table)) "Gene" else "GeneSymbol"}
      if ("logFC" %in% colnames(table)) {
        plotly_volcano_plot(table,
                        input$fc_threshold,
                        -log10(input$pvalue_threshold),
                        input$pvalue_adjusted_flag,
                        gene_column)
      } else {
        plotly_avgexpr_plot(table,
                        -log10(input$pvalue_threshold),
                        input$pvalue_adjusted_flag,
                        gene_column)
      }
    })

    current_URL <- reactive({
      conditions <- condition_list()
      # Last condition is the model name/category so we remove it
      conditions <- conditions[-length(conditions)]
      buildURL(conditions, paste0("?tab=", link_to))
    })
    
    # Create checkbox from color column
    output$ui_table_checkbox <- renderUI({
      req(vp_table())
      checkboxGroupInput(ns("deg_table_checkbox"),
                         label = "Filter genes:",
                         choices = levels(as.factor(vp_table()[["color"]])),
                         selected = levels(as.factor(vp_table()[["color"]])))
    })

    output$deg_table <- DT::renderDT({
      req(input$deg_table_checkbox)
        model_table <- vp_table()
        gene_column <- { 
          if ("Gene" %in% colnames(model_table)) "Gene" else "GeneSymbol"
        }
        # Optional link
        if (not_null(link_to)) {
          model_table[[gene_column]] <- urlVector(model_table[[gene_column]],
                                                  "gene",
                                                  isolate({ current_URL() }))
        }
        view_cols <- 
          c("EnsemblID", "Gene", "GeneSymbol", "logFC", "AvgExpr", "p.value", "q.value")
        model_table[model_table$color %in% input$deg_table_checkbox, ] %>%
          dplyr::select(any_of(view_cols))
        },
      filter = "top",
      escape = FALSE,
      rownames = FALSE,
      options = list(scrollX = TRUE)
    )
  })
}
