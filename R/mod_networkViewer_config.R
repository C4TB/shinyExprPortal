#' @noRd
networkViewer_config <- function(config, data_folder = "") {
  message("Checking networkViewer configuration")

  required_packages <- c("igraph", "visNetwork")
  stopIfNotInstalled(required_packages, "networkViewer")

  if (is.null(config$network_files)) {
    stop_nice(paste(
      "networkViewer:",
      "'network_files' property is missing"
    ))
  }

  file_type <- config$network_file_type %||% "edge_list"
  read_nf <- function(x) read_file(x$file, file_type, data_folder)
  network_dfs <- lapply(config$network_files, read_nf)
  sep <- config$name_separator %||% "_"

  config$network_names <- lapply(config$network_files, function(x) x$name)
  config$overlap <- TRUE

  if (file_type == "edge_list") {
    networks <- lapply(network_dfs, function(network_df) {
      edge_graph <- igraph::graph_from_data_frame(network_df)
      nodes_table <- igraph::as_data_frame(edge_graph, "vertices")
      nodes_table$group <- sub(paste0(sep, ".*"), "", nodes_table$name)
      nodes_table$symbol <- sub(paste0(".*", sep), "", nodes_table$name)
      subnetworks <- igraph::decompose(edge_graph)
      list(nodes_table, subnetworks)
    })
    config$nodes_table <- lapply(networks, function(x) x[[1]])
    names(config$nodes_table) <- config$network_names
    config$network_list <- lapply(networks, function(x) x[[2]])
    names(config$network_list) <- config$network_names
    config$overlap <- FALSE
  }

  if (is.null(config$node_types)) {
    stop_nice(paste(
      "networkViewer:",
      "''node_types' list is missing"
    ))
  }

  config
}
