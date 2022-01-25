#' @noRd
networkViewer_config <- function(config, data_folder = "") { 
  message("Checking networkViewer configuration")
  
  requiredPackages <- c("igraph")
  stopIfNotInstalled(requiredPackages, "networkViewer")
  
  if (is.null(config$network_file)) {
    stop("networkViewer: 
         'network_file' is missing")
  }
  
  network_df <- readFile(config$network_file, "", data_folder)
  
  file_type <- config$network_file_type %||% "edge_list"
  sep <- config$name_separator %||% "_"
  
  config$overlap <- TRUE
  
  if (file_type == "edge_list") {
    edge_graph <- igraph::graph_from_data_frame(network_df)
    nodes_table <-  igraph::as_data_frame(edge_graph, "vertices")
    nodes_table$group <- sub(paste0(sep,".*"),"",nodes_table$name)
    config$nodes_table <- nodes_table
    config$network_list <- igraph::decompose(edge_graph)
    config$overlap <- FALSE
  }
  
  if (is.null(config$node_types)) {
    stop("networkViewer:
         ''node_types' list is missing")
  }
  
  config
}