geneModulesHeatmap_config <- function(config, data_folder = "") {
  message("Checking geneModulesHeatmap configuration")

  required_packages <- c("RColorBrewer", "DT")
  stopIfNotInstalled(required_packages, "geneModulesHeatmap")

  if (!is.null(config$modules_table)) {
    message("Loading file: modules_table")
    config$modules_table <-
      read_file(config$modules_table, "table", data_folder)
  } else {
    stop_nice(
      "geneModulesHeatmap: ",
      "a 'modules_table' file is required for this module"
    )
  }

  if (is.null(config$category_variable)) {
    stop_nice(
      "geneModulesHeatmap: ",
      "'category_variable' to identify models is missing"
    )
  }

  if (is.null(config$modules_variable)) {
    stop_nice(
      "geneModulesHeatmap:",
      "'modules_variable' to identify modules is missing"
    )
  }

  if (is.null(config$genes_variable)) {
    stop_nice(
      "geneModulesHeatmap:",
      "'genes_variable' to identify genes is missing"
    )
  }

  # Unlist because YAML will produce lists instead of named vectors
  if (!is.null(config$custom_annotation_colors)) {
    config$custom_annotation_colors <-
      lapply(config$custom_annotation_colors, unlist)
  }
  
  if (!is.null(config$custom_heatmap_palette)) {
    if (length(config$custom_heatmap_palette) == 1) {
      if (!config$custom_heatmap_palette
          %in% rownames(RColorBrewer::brewer.pal.info)) {
        stop_nice(
        "geneModulesHeatmap: 'custom_heatmap_palette' provided is not a valid ",
        "RColorBrewer palette"
        )
      }
    } else {
      stop_nice("geneModulesHeatmap: ",
          "'custom_heatmap_palette' must be a valid RColorBrewer palette name")
    } 
  } else {
    config$custom_heatmap_palette <- "RdBu"
  }

  config
}
