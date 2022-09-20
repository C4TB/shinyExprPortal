#' @noRd
networkViewer_config <- function(config, data_folder = "") {
  message("Checking networkViewer configuration")

  required_packages <- c("igraph", "visNetwork")
  stopIfNotInstalled(required_packages, "networkViewer")

  if (is.null(config$network_files)) {
    stop_nice(
      "networkViewer: ",
      "'network_files' property is missing"
    )
  }

  file_type <- config$network_file_type %||% "edge_list"
  read_nf <- function(x) read_file(x$file, file_type, data_folder)
  network_dfs <- lapply(config$network_files, read_nf)
  # Custom name separator for node types and names
  sep <- config$name_separator %||% "_"

  config$network_names <- lapply(config$network_files, function(x) x$name)
  config$overlap <- TRUE

  if (file_type == "edge_list") {
    # Return a pair of table of nodes and full network for each file loaded
    networks <- lapply(network_dfs, function(network_df) {
      edge_graph <- igraph::graph_from_data_frame(network_df)
      nodes_table <- igraph::as_data_frame(edge_graph, "vertices")
      # Set node type as group for color 
      nodes_table$group <- sub(paste0(sep, ".*"), "", nodes_table$name)
      nodes_table$symbol <- sub(paste0(".*", sep), "", nodes_table$name)
      list(nodes_table, edge_graph)
    })
    config$nodes_table <- lapply(networks, function(x) x[[1]])
    names(config$nodes_table) <- config$network_names
    config$network_list <- lapply(networks, function(x) x[[2]])
    names(config$network_list) <- config$network_names
    config$overlap <- FALSE
  }
  
  if (!is.null(config$custom_node_colors) &
      !is.list(config$custom_node_colors)) {
    stop_nice("networkViewer: 'custom_node_colors' must be a ",
      "list of node types and colors")
  }
  
  if (!is.null(config$custom_font_colors) &
      !is.list(config$custom_font_colors)) {
    stop_nice("networkViewer: 'custom_font_colors' must be a ",
              "list of node types and colors")
  }
  
  if (!is.null(config$custom_heatmap_palette)) {
    if (length(config$custom_heatmap_palette) == 1) {
      stopIfNotInstalled(c("RColorBrewer"), "networkViewer")
      if (!config$custom_heatmap_palette
          %in% rownames(RColorBrewer::brewer.pal.info)) {
        stop_nice(
          "networkViewer: 'custom_heatmap_palette' provided is not a valid ",
          "RColorBrewer palette"
        )
      }
    } else {
      stop_nice("networkViewer: ",
        "'custom_heatmap_palette' must be a valid RColorBrewer palette name")
    } 
  } else {
    config$custom_heatmap_palette <- "RdBu"
  }
  
  config
}
