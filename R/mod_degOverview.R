# degOverview UI Function
mod_degOverview_ui <- function(module_name, appdata, global, module_config) {
  models <- appdata$models
  category_variable <- module_config$category_variable
  categories <- unique(unlist(models[, category_variable]))
  degOverview_tab(categories,
                  module_name)
}
#' Differentially expressed genes tab UI
#'
#' @param categories model categories
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @export
#'
degOverview_tab <- function(categories, id = NULL) {
  ns <- NS(id)
  tabPanel("Model results", value= "degOverview", 
           splitLayout(
             verticalLayout(
               wellPanel(
               radioButtons(
                 ns("model_category"),
                 label = "Select category:",
                 choices = categories,
                 selected = categories[[1]]
               ),
               # uiOutput(ns("selected_model")),
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
                 label = "Significance threshold (-log10):",
                 value = 1.3 ,
                 min = 1 ,
                 max = 50,
                 step = 0.1
               ),
               radioButtons(
                 ns("pvalue_adjusted"),
                    label = "Adjusted p-values?",
                    choices = list("No" = "p.value", "Yes" = "q.value"),
                    selected = "p.value"
               )
             )),
             bsplus::bs_accordion("deg_results") %>%
               bsplus::bs_append(title = "Plot and list of genes",
                         content = splitLayout(
                                    style = "font-size: 75%;",
                                    plotOutput(ns("results_plot"),
                                        width = "700px",
                                        height = "500px"),
                                    tags$div(uiOutput(ns("genelist")),
                                             style = "max-height: 500px"),
                                    cellWidths = c(700, 200)
                        )) %>%
                bsplus::bs_append(title = "Table",
                         content = DT::DTOutput(ns("deg_table"))),
             cellWidths = c("20%", "80%"),
             cellArgs = list(style = 'white-space: normal;')
           )
  )
}
#' degOverview Server Function
#'
#' @noRd 
mod_degOverview_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session){
    ns <- session$ns

    models <- appdata$models
    category_variable <- module_config$category_variable
    exc_columns <- c(category_variable,
                     c("pSignif", "qSignif", "File", "Data"))
    table_subset <- dplyr::select(models, -exc_columns)
    
    model_update <- reactiveVal(FALSE)
    
    observeEvent(input$model_category, { 
      selected_category_models <- models[
        models[, category_variable] == input$model_category, ] %>%
        dplyr::select(-exc_columns)
      # taglist_args <- lapply(colnames(table_subset), function(column_name) {
      #   radioButtons(ns(paste0("selected_", column_name)),
      #                label = paste("Select", column_name),
      #                choices = unique(unlist(selected_category[, column_name]))
      #   )
      # })
      # output$model_controls <- renderUI({
        #tagList(taglist_args)
      # })
      model_update(TRUE)
      updateRadioButtons(
        session,
        "selected_model",
        choiceNames = do.call(paste, c(selected_category_models)),
        choiceValues = do.call(paste, c(selected_category_models, sep = "_"))
      )

    })
    
    # selected_model_cond <- reactive({
    #   input_values <- sapply(colnames(table_subset), function(column_name, input) {
    #     input_name <- paste0("selected_", column_name)
    #     input()[[input_name]]
    #   }, input = reactive(input), USE.NAMES = TRUE)
    #   input_values[[category_variable]] <- input$model_category
    #   input_values
    # })
    
    observeEvent(input$selected_model, {
      model_update(TRUE)
    })

    condition_list <- eventReactive(c(model_update(), input$selected_model), {
      model_update(FALSE)
      isolate({
        condition <- setNames(unlist(strsplit(input$selected_model, "_")),
                              colnames(table_subset))
        condition[[category_variable]] <- input$model_category
      })
      condition
    })
    
    model_results <- reactive({
      condition <- condition_list()
      model_res <- list()
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
      pvalue_label <- pvalue_labels[input$pvalue_adjusted]
      # Compute p-value significance
      table$pvalue_signif <-
        as.numeric(
          table[[input$pvalue_adjusted]] < 10 ^ (-input$pvalue_threshold)
          )
      if ("logFC" %in% colnames(table)) {
        table$fc_signif <-
          as.numeric(abs(table$logFC) > abs(input$fc_threshold))
        table$signif <- table$fc_signif + 2 * table$pvalue_signif  
      } else {
        table$signif <-2 * table$pvalue_signif  
      }
      table$color <- sprintf(as.character(signif_labels[table$signif+1]), pvalue_label)
      # Apply log transformation to p and q value
      table$p.value <- -log10(table$p.value)
      table$q.value <- -log10(table$q.value)
      table
    })

    output$results_plot <- renderPlot({
      table <- vp_table()
      gene_column <- { if ("Gene" %in% colnames(table)) "Gene" else "GeneSymbol"}
      if ("logFC" %in% colnames(table)) {
        gg_volcano_plot(table,
                        input$fc_threshold,
                        input$pvalue_threshold,
                        input$pvalue_adjusted,
                        gene_column)
      } else {
        gg_avgexpr_plot(table,
                        input$pvalue_threshold,
                        input$pvalue_adjusted,
                        gene_column)
      }
    })

    current_URL <- reactive({
      conditions <- condition_list()
      # Last condition is the model name/category so we remove it
      conditions <- conditions[-length(conditions)]
      buildURL(conditions, "/?tab=singleGeneCorr")
    })
    
    gene_as_itemURL <- function(row, gene_column) {
      itemURL(row[gene_column],
              appendToURL(isolate({ current_URL() }),
                          "gene",
                          row[gene_column]))
    }
    
    output$genelist <- renderUI({
      table <- vp_table()
      gene_column <- { if ("Gene" %in% colnames(table)) "Gene" else "GeneSymbol"}
      if ("logFC" %in% colnames(table)) {
        pvalue_label <- isolate({ pvalue_labels[input$pvalue_adjusted] })
        fc_header <- sprintf("logFC and %s significant genes:",
                             pvalue_label)
        pvalue_header <- sprintf("%s significant genes:",
                             pvalue_label)
        tagList(
          fc_header,
          tags$ul(apply(
            table[table$signif ==  3,], 1,
            gene_as_itemURL, gene_column = gene_column
          )),
          pvalue_header,
          tags$ul(apply(
            table[table$signif ==  2,], 1,
            gene_as_itemURL, gene_column = gene_column
          ))
        )
      } else {
        tagList(
          p("Significant genes: "),
          tags$ul(apply(
            table[table$signif ==  2, ], 1,
            gene_as_itemURL, gene_column = gene_column
          ))
        )
      }
    })

    output$deg_table <- DT::renderDT({
        model_results()
      },
      filter = "top",
      options = list(scrollX = TRUE)
    )
    
  })
}
