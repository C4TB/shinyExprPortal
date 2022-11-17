mod_networkViewer_ui <- function(module_name, config, module_config) {
  networkViewer_tab(
    module_config$network_names,
    module_config$title,
    module_config$description,
    module_name
  )
}

#' Network viewer tab
#'
#' @param network_names names of networks for selection
#' @param title optional module title
#' @param description optional module description
#' @param id module id
#'
#' @return a tab panel
#' @noRd
#'
networkViewer_tab <- function(network_names,
                              title = NULL,
                              description = NULL,
                              id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "View Networks",
    value = "networkViewer",
    tags$h5(description %||% "Choose a node type and a name to search for
            networks containing it"),
    splitLayout(
      verticalLayout(
        wellPanel(
          ## INPUTS
          # selectizeInput(ns("node_type"), "Select node type:", choices = node_types),
          selectizeInput(ns("network1"),
            "Select first network:",
            network_names,
            selected = network_names[[1]],
            options = list(
              dropdownParent = "body"
            )
          ),
          selectizeInput(ns("network2"),
            "Select second network:",
            network_names,
            selected = network_names[[2]],
            options = list(
              dropdownParent = "body"
            )
          ),
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
      fluidRow(
        column(
          5,
          tabsetPanel(
            tabPanel(
              "Network",
              visNetwork::visNetworkOutput(
                ns("network_output1"),
                height = "600px"
              )
            ),
            tabPanel(
              "Heatmap",
              iheatmaprOutput(
                ns("heatmap_output1")
              )
            )
          )
        ),
        column(
          5,
          tabsetPanel(
            tabPanel(
              "Network",
              visNetwork::visNetworkOutput(
                ns("network_output2"),
                height = "600px"
              )
            ),
            tabPanel(
              "Heatmap",
              iheatmaprOutput(
                ns("heatmap_output2")
              )
            )
          )
        )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}

mod_networkViewer_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    measures_data <- config$data$measures_data
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup

    sample_col <- config$sample_variable
    sample_categories <- config$sample_categories

    sample_category <- module_config$sample_category
    network_files <- module_config$network_files
    nodes_table <- module_config$nodes_table
    network_list <- module_config$network_list
    custom_node_colors <- module_config$custom_node_colors
    custom_font_colors <- module_config$custom_font_colors
    heatmap_palette <- module_config$custom_heatmap_palette
    
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
      Filter(
        function(x) input$node_name %in% names(igraph::V(x)),
        igraph::decompose(selected_network_list())
      )
    })

    results_list2 <- reactive({
      req(input$node_name)
      # Remove vertices that are NOT in the first network
      igraph::delete_vertices(selected_network_list2(),
            !names(igraph::V(selected_network_list2())) %in%
              names(igraph::V(results_list()[[1]])))
    })

    # There will only be 1 result if the subnetworks are connected components
    selected_network1 <- reactive({
      results_list()[[1]]
    })

    selected_network2 <- reactive({
      search_results <- results_list2()
      validate(need(igraph::vcount(search_results) > 0, "No nodes found"))
      search_results
    })

    subset_network1 <- reactive({
      req(input$node_name)

      # Get nodes correspondening to selected network
      vnd <- visNetwork::toVisNetworkData(selected_network1())
      nt <- nodes_table[[input$network1]]
      nt <- filter(nt, nt$name %in% vnd$nodes[["id"]])

      # Identify the types that are valid for the samples
      # E.g Cell types prefixes
      sample_cat_values <- Filter(
        function(x) x$name == sample_category,
        sample_categories
      )[[1]][["values"]]
      valid_categories <- intersect(unique(nt$group), sample_cat_values)
      vc_list <- list(valid_categories)
      names(vc_list) <- c(sample_category)

      # Get the remaining sample categories from the network_info
      # (Exclude name and file)
      network_info <- Filter(
        function(x) x$name == input$network1,
        network_files
      )[[1]]
      lookup_info <- network_info[!names(network_info) %in% c("name", "file")]

      # Combine both filters
      values_list <- c(vc_list, lookup_info)

      # Select subset of samples for this network
      selected_samples <-
        selectMatchingMultipleValues(sample_lookup, values_list)
      # Select subset of genes for this network
      gene_list <-
        selectMatchingMultipleValues(
          nt,
          list("group" = valid_categories),
          "symbol"
        )
      # Select subset of expression matrix
      expmat <- expression_matrix[unique(gene_list),
        selected_samples[[sample_col]],
        drop = FALSE
      ]
      annots <- data.frame(selected_samples[[sample_category]])
      names(annots) <- sample_category
      list(expression = expmat, annots = annots)
    })

    output$heatmap_output1 <- renderIheatmap({
      samples_network1 <- subset_network1()
      nrows <- nrow(samples_network1$expression)
      req(nrows > 0, cancelOutput = TRUE)
      hm <- iheatmap(
        samples_network1$expression,
        colors = rev(
          RColorBrewer::brewer.pal(11, heatmap_palette)
        ),
        row_labels = TRUE,
        col_labels = FALSE,
        scale = "rows",
        scale_method = "standardize",
        name = "Expression z-scores",
        layout = list(
          font = list(size = 9),
          plot_bgcolor = "transparent",
          paper_bgcolor = "transparent"
        )
      ) %>%
        add_col_annotation(samples_network1$annots)
      if (nrows > 1) {
        hm <- hm %>% add_row_clustering()
      }
      hm <- hm %>% add_col_clustering()
      hm
    })

    subset_network2 <- reactive({
      req(input$node_name)
      # Get nodes correspondening to selected network
      vnd <- visNetwork::toVisNetworkData(selected_network2())
      nt <- nodes_table[[input$network2]]
      nt <- filter(nt, nt$name %in% vnd$nodes[["id"]])

      # Identify the node types (prefix) that correspond to valid
      # samples in the expression matrix
      # E.g. measures or micro RNA nodes are not part of the expression matrix
      sample_cat_values <- Filter(
        function(x) x$name == sample_category,
        sample_categories
      )[[1]][["values"]]
      valid_categories <- intersect(unique(nt$group), sample_cat_values)
      vc_list <- list(valid_categories)
      names(vc_list) <- c(sample_category)

      # Get the remaining sample categories from the network_info
      # (Exclude name and file)
      network_info <- Filter(
        function(x) x$name == input$network2,
        network_files
      )[[1]]
      lookup_info <- network_info[!names(network_info) %in% c("name", "file")]

      # Combine both filters
      values_list <- c(vc_list, lookup_info)

      # Select subset of samples for this network
      selected_samples <-
        selectMatchingMultipleValues(sample_lookup, values_list)
      # Select subset of genes for this network
      gene_list <-
        selectMatchingMultipleValues(nt,
                                     list("group" = valid_categories),
                                     "symbol")
      # Select subset of expression matrix
      expmat <- expression_matrix[unique(gene_list),
        selected_samples[[sample_col]],
        drop = FALSE
      ]
      annots <- data.frame(selected_samples[[sample_category]])
      names(annots) <- sample_category
      list(expression = expmat, annots = annots)
    })

    output$heatmap_output2 <- renderIheatmap({
      samples_network2 <- subset_network2()
      nrows <- nrow(samples_network2$expression)
      req(nrows > 0, cancelOutput = TRUE)
      hm <- iheatmap(
        samples_network2$expression,
        colors = rev(
          RColorBrewer::brewer.pal(11, heatmap_palette)
        ),
        row_labels = TRUE,
        col_labels = FALSE,
        scale = "rows",
        scale_method = "standardize",
        name = "Expression z-scores",
        layout = list(
          font = list(size = 9),
          plot_bgcolor = "transparent",
          paper_bgcolor = "transparent"
        )
      ) %>%
        add_col_annotation(samples_network2$annots)
      if (nrows > 1) {
        hm <- hm %>% add_row_clustering()
      }
      hm <- hm %>% add_col_clustering()
      hm
    })

    output$network_output1 <- visNetwork::renderVisNetwork({
      plotNetwork(
        selected_network1(),
        nodes_table[[input$network1]],
        colors = custom_node_colors,
        font_colors = custom_font_colors,
        input$node_name
      )
    })

    output$network_output2 <- visNetwork::renderVisNetwork({
      plotNetwork(
        selected_network2(),
        nodes_table[[input$network2]],
        colors = custom_node_colors,
        font_colors = custom_font_colors
      )
    })
  })
}
