#' degVolcano UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_degVolcano_ui <- function(id, appdata) {
  degVolcano_tab(appdata$modules$degModules$modules$degVolcano, id)
}
#' Differentially expressed genes tab UI
#'
#' @param model_names list of model names
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @export
#'
degVolcano_tab <- function(model_names, id = NULL) {
  ns <- NS(id)
  tabPanel("DEA Results", value= "deg", 
           splitLayout(
             verticalLayout(
               wellPanel(
               radioButtons(
                 ns("model_category"),
                 label = "Select category:",
                 choices = names(model_names),
                 selected = names(model_names)[1]
               ),
               radioButtons(ns("selected_model"), label = "Select model:",
                            choices = names(model_names[[1]])),
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
               bsplus::bs_append(title = "Volcano plot",
                         content = splitLayout(
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
#' degVolcano Server Function
#'
#' @noRd 
mod_degVolcano_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session){
    ns <- session$ns

    observeEvent(input$model_category, { 
      choices <- names(appdata$modules$degVolcano[[input$model_category]])
      updateRadioButtons(session, "selected_model",
                   choices = choices)
    })
    
    signif_labels <- list("not significant", "log FC", 
                          "p-value", "log FC and p-value")
    
    model_results <- reactive({ 
      req(input$selected_model)
      isolate({
        model_cat <- appdata$modules$degVolcano[[input$model_category]]
        table <- model_cat[[input$selected_model]] 
      })
      validate(need(not_null(table), "..."))
      table
    })
    
    vp_table <- reactive({
      table <- model_results()
      table$fc_signif <- as.numeric(abs(table$logFC) > abs(input$fc_threshold))
      table$pvalue_signif <-
        as.numeric(
          table[[input$pvalue_adjusted]] < 10 ^ (-input$pvalue_threshold))
      table$signif <- table$fc_signif + 2 * table$pvalue_signif
      table$color <- signif_labels[table$signif+1]
      table$p.value <- -log10(table$p.value)
      table$q.value <- -log10(table$q.value)
      table
    })
    
    output$volcanoplot <- renderPlot({ 
      table <- vp_table() 
      max_x_data <- max(abs(min(table$logFC)), max(table$logFC))
      gg_volcano_plot(table,
                      input$fc_threshold,
                      input$pvalue_threshold,
                      input$pvalue_adjusted)
      
    })
    
    output$genelist <- renderUI({
      table <- vp_table()
      tagList(p("Significant genes: "),
              tags$ul(apply(
                table[table$signif ==  3,], 1, 
                function(x) tags$li(x["Gene"]))))
    })
      
    output$deg_table <- DT::renderDT({
      model_results()
    },
    filter = "top",
    options = list(scrollX = TRUE))
    
  })
}
