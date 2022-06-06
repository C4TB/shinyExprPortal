geneModulesHeatmap_config <- function(config, data_folder = "") {
  message("Checking geneModulesHeatmap configuration")

  required_packages <- c("RColorBrewer", "DT")
  stopIfNotInstalled(required_packages, "geneModulesHeatmap")

  if (!is.null(config$modules_table)) {
    message("Loading file: modules_table")
    config$modules_table <- readFile(config$modules_table, "table", data_folder)
  } else {
    stop_nice(paste(
      "geneModulesHeatmap:",
      "a 'modules_table' file is required for this module"
    ))
  }

  if (is.null(config$category_variable)) {
    stop(paste(
      "geneModulesHeatmap:",
      "'category_variable' to identify models is missing"
    ))
  }

  if (is.null(config$modules_variable)) {
    stop(paste(
      "geneModulesHeatmap:",
      "'modules_variable' to identify modules is missing"
    ))
  }

  if (is.null(config$genes_variable)) {
    stop(paste(
      "geneModulesHeatmap:",
      "'genes_variable' to identify genes is missing"
    ))
  }

  # Unlist because YAML will produce lists instead of named vectors
  if (!is.null(config$annotation_colours)) {
    config$annotation_colours <- lapply(config$annotation_colours, unlist)
  }

  config
}
