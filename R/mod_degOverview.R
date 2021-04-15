#' degOverview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_degOverview_ui <- function(id, appdata) {
  module_config <- appdata$modules$degModules$modules$degOverview
  models <- appdata$modules$degModules$models
  category_variable <- module_config$category_variable
  categories <- unique(unlist(models[, category_variable]))
  degOverview_tab(categories,
                  id)
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
  tabPanel("Model Results", value= "degOverview", 
           splitLayout(
             verticalLayout(
               wellPanel(
               radioButtons(
                 ns("model_category"),
                 label = "Select category:",
                 choices = categories,
                 selected = categories[[1]]
               ),
               #uiOutput(ns("model_controls")),
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
                 value = 5 ,
                 min = 1 ,
                 max = 50,
                 step = 1
               ),
               radioButtons(
                 ns("pvalue_adjusted"),
                    label = "Adjusted p-values?",
                    choices = list("Yes" = "q.value", "No" = "p.value"),
                    selected = "q.value"
               )
             )),
             bsplus::bs_accordion("deg_results") %>%
               bsplus::bs_append(title = "Overview plot",
                         content = splitLayout(
                                    style = "font-size: 75%;",
                                    plotOutput(ns("volcanoplot"),
                                        width = "700px",
                                        height = "500px"),
                                    tags$div(uiOutput(ns("genelist"))),
                                    cellWidths = c(700, 150)
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
mod_degOverview_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session){
    ns <- session$ns

    models <- appdata$modules$degModules$models
    module_config <- appdata$modules$degModules$modules$degOverview
    category_variable <- module_config$category_variable
    exc_columns <- c(category_variable,
                     c("pSignif", "qSignif", "File", "Data"))
    table_subset <- dplyr::select(models, -exc_columns)
    
    observeEvent(input$model_category, { 
      selected_category <- models[
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
      updateRadioButtons(session, "selected_model",
                   choiceNames = do.call(paste, c(selected_category)),
                   choiceValues = do.call(paste, c(selected_category, sep = "_")))
    })
    
    # selected_model_cond <- reactive({
    #   input_values <- sapply(colnames(table_subset), function(column_name, input) {
    #     input_name <- paste0("selected_", column_name)
    #     input()[[input_name]]
    #   }, input = reactive(input), USE.NAMES = TRUE)
    #   input_values[[category_variable]] <- input$model_category
    #   input_values
    # })
  
    
    model_results <- reactive({
      req(input$selected_model)
      condition <- setNames(unlist(strsplit(input$selected_model, "_")),
                            colnames(table_subset))
      condition[[category_variable]] <- input$model_category
      model_res <- list()
      for (var_name in names(condition)) {
        model_cond_res <- models[models[, var_name] == condition[var_name], ]
        model_res[[var_name]] <- model_cond_res
      }
      
      browser()
      selected_model <- Reduce(
        function(x,y) dplyr::inner_join(x, y, by = colnames(x)), model_res)
      selected_model$Data[[1]]
    })
    
    signif_labels <- list("not significant", "log FC",
                          "p-value", "log FC and p-value")
    # 
    # model_results <- reactive({ 
    #   req(input$selected_model)
    #   isolate({
    #     model_cat <- appdata$modules$degOverview[[input$model_category]]
    #     table <- model_cat[[input$selected_model]] 
    #   })
    #   validate(need(not_null(table), "..."))
    #   table
    # })
    # 
    vp_table <- reactive({
      table <- model_results()
      table$pvalue_signif <-
        as.numeric(
          table[[input$pvalue_adjusted]] < 10 ^ (-input$pvalue_threshold))
      if ("logFC" %in% colnames(table)) {
        table$fc_signif <- as.numeric(abs(table$logFC) > abs(input$fc_threshold))
        table$signif <- table$fc_signif + 2 * table$pvalue_signif  
      } else {
        table$signif <- 2 * table$pvalue_signif  
      }
      table$color <- signif_labels[table$signif+1]
      table$p.value <- -log10(table$p.value)
      table$q.value <- -log10(table$q.value)
      table
    })

    output$volcanoplot <- renderPlot({
      table <- vp_table()
      browser()
      if ("logFC" %in% colnames(table)) {
        max_x_data <- max(abs(min(table$logFC)), max(table$logFC))
        gg_volcano_plot(table,
                        input$fc_threshold,
                        input$pvalue_threshold,
                        input$pvalue_adjusted)
      } else {
        max_x_data <- max(abs(min(table$AvgExpr)), max(table$AvgExpr))
        ggplot(table, aes(y = .data$p.value,
                           x = .data$AvgExpr)) +
          geom_point() + 
          xlim(-max_x_data, max_x_data)
      }

    })

    output$genelist <- renderUI({
      table <- vp_table()
      if ("logFC" %in% colnames(table)) {
        signif_table <- table[table$signif ==  3,]
      } else {
        signif_table <- table[table$signif ==  2,]
      }
      
      tagList(p("Significant genes: "),
              tags$ul(apply(
                signif_table, 1,
                function(x) tags$li(x["Gene"] %||% x["GeneSymbol"]))))
    })

    output$deg_table <- DT::renderDT({
        model_results()
      },
      filter = "top",
      options = list(scrollX = TRUE)
    )
    
  })
}
