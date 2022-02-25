mod_networkViewer_ui <- function(module_name, appdata, global, module_config) {
  networkViewer_tab(module_config$network_names,
                    module_config$node_types,
                    module_config$overlap,
                    module_config$title,
                    module_name)
}

#' Network viewer tab
#'
#' @param network_names 
#' @param node_types 
#' @param overlap 
#' @param title 
#' @param id 
#'
#' @return a tab panel
#' @noRd
#'
#' @importFrom visNetwork visNetworkOutput
networkViewer_tab <- function(network_names,
                              node_types,
                              overlap,
                              title = NULL,
                              id = NULL) {

  ns <- NS(id)
  tabPanel(
    title = "View Networks",
    value = "networkViewer",
    tags$h5(title %||% "Choose a node type and a name to search for
            networks containing it"),
    splitLayout(
      verticalLayout(
        wellPanel(
          ## INPUTS
          #selectizeInput(ns("node_type"), "Select node type:", choices = node_types),
          selectizeInput(ns("network1"),
                         "Select first network:",
                         network_names,
                         selected = network_names[[1]],
                         options = list(
                           dropdownParent = "body")),
          selectizeInput(ns("network2"),
                         "Select second network:",
                         network_names,
                         selected = network_names[[2]],
                         options = list(
                           dropdownParent = "body")),
          selectizeInput(
            ns("node_name"),
            "Find node:",
            choices = NULL,
            options = list(
              dropdownParent = "body",
              onInitialize = I('function(){this.setValue(""); }'),
              placeholder = ""
            )
          )
        )
      ),
      # fluidRow({
      #   if (overlap)
      #     column(2, DTOutput(ns("network_list"), width = "100%"))
      # },
        fluidRow(
          column(5,
                 visNetworkOutput(ns("network_output1"), height = "600px")), 
          column(5,
                 visNetworkOutput(ns("network_output2"), height = "600px"))
        ),
        cellWidths = c("20%", "80%"),
        cellArgs = list(style = "white-space: normal;")
      )
  )
}

mod_networkViewer_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression_matrix
    sample_lookup <- appdata$sample_lookup
    
    overlap <- module_config$overlap
    nodes_table <- module_config$nodes_table
    network_list <- module_config$network_list
    
    #updateSelectizeInput(session, "node_name", choices = nodes_table$name, selected = "", server = TRUE)
    observeEvent(input$network1, {
      nodes <- nodes_table[[input$network1]]
      updateSelectizeInput(
        session,
        "node_name",
        choices = nodes$name,
        selected = "",
        server = TRUE
      )
    })
    selected_network_list <- reactive({
      network_list[[input$network1]]
    })
    
    selected_network_list2 <- reactive({
      network_list[[input$network2]]
    })
    
    results_list <- reactive({ 
      req(input$node_name)
      Filter(function(x) input$node_name %in% names(igraph::V(x)),
             selected_network_list())
    })
    
    results_list2 <- reactive({
      req(input$node_name)
      Filter(function(x) input$node_name %in% names(igraph::V(x)),
             selected_network_list2())
    })
    
    # if (overlap) {
    #   output$network_list <- renderDT({
    #     search_results <- results_list()
    #     data.frame(Network = seq_along(search_results))
    #   },
    #   options = list(
    #     dom = "t",
    #     ordering = FALSE,
    #     paging = FALSE,
    #     scrollY = "600px",
    #     scrollCollapse = TRUE
    #   ),
    #   filter = "top",
    #   class = "compact hover",
    #   selection = "single",
    #   rownames = FALSE)
    # }
    
    # There will only be 1 result if the subnetworks are connected components
    # This is defined by the 'overlap' setting in the configuration
    selected_network <- reactive({
      result_number <- 1
      # if (overlap) {
      #   req(input$network_list_row_last_clicked)
      #   result_number <- input$network_list_row_last_clicked
      # }
      search_results <- results_list()
      search_results[[result_number]]
    })
    
    selected_network2 <- reactive({
      result_number <- 1
      # if (overlap) {
      #   req(input$network_list_row_last_clicked)
      #   result_number <- input$network_list_row_last_clicked
      # }
      search_results <- results_list2()
      validate(need(length(search_results) > 0,"Node not found"))
      search_results[[result_number]]
    })
    
    output$network_output1 <- visNetwork::renderVisNetwork({
      vnd <- visNetwork::toVisNetworkData(selected_network())
      # Get additional info from nodes_table
      vnd$nodes <- merge(vnd$nodes, nodes_table[[input$network1]], by.x = "id", by.y = "name")
      vnd$nodes$font.size <- 14
      net <- visNetwork::visNetwork(vnd$nodes, vnd$edges) %>%
        visNetwork::visIgraphLayout() %>%
        visNetwork::visOptions(
          nodesIdSelection = list(selected = input$node_name, main = "Select node to highlight:"),
          highlightNearest = list(enabled = T, degree = 1, hover = T))
        #visNetwork::visLegend(width = 0.1, position = "right", main = "Type")
      #visNetwork::visNetworkProxy("network_output") %>%
      #  visNetwork::visSelectNodes(input$node_name, highlightEdges = TRUE)
      net
    })
    
    output$network_output2 <- visNetwork::renderVisNetwork({
      vnd <- visNetwork::toVisNetworkData(selected_network2())
      # Get additional info from nodes_table
      vnd$nodes <- merge(vnd$nodes, nodes_table[[input$network2]], by.x = "id", by.y = "name")
      vnd$nodes$font.size <- 14
      net <- visNetwork::visNetwork(vnd$nodes, vnd$edges) %>%
        visNetwork::visIgraphLayout() %>%
        visNetwork::visOptions(
          nodesIdSelection = list(selected = input$node_name, main = "Select node to highlight:"),
          highlightNearest = list(enabled = T, degree = 1, hover = T))
      #visNetwork::visLegend(width = 0.1, position = "right", main = "Type")
      #visNetwork::visNetworkProxy("network_output") %>%
      #  visNetwork::visSelectNodes(input$node_name, highlightEdges = TRUE)
      net
    })
    
    
  })
}