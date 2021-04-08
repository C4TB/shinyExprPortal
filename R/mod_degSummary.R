#' module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_degSummary_ui <- function(id, appdata) {
  degSummary_tab(appdata$modules$degSummary, id)
}

degSummary_tab <- function(model_names, id = NULL) {
  ns <- NS(id)
  tabPanel(title = "DEG Summary", value = "degSummary",
           splitLayout(
             verticalLayout(
               wellPanel(
                 radioButtons(
                   ns("model_category"),
                   label = "Select category:",
                   choices = c("a", "b")
                 )
               )
             ),
             verticalLayout(
               #OUTPUTS
               plotOutput(ns("degHeatmap"))
             ),
             cellWidths = c("20%", "80%"),
             cellArgs = list(style = "white-space: normal;")
           )
  )
}

mod_degSummary_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$data$clinical
    expression_matrix <- appdata$data$expression_matrix
    sample_lookup <- appdata$data$sample_lookup
     
    # REST OF CODE HERE
    output$degHeatmap <- renderPlot({
      req(input$model_category)
      browser() 
      a <- flattenlist(models)
      count_signif <- lapply(a, function(x) nrow(x[which(x$p.value < 0.05),]))
      count_signifq <- lapply(a,function(x) nrow(x[which(x$q.value < 0.05),]))
    })
    
    
    
  })
}
