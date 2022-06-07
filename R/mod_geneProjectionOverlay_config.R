# Config template

#' @noRd
geneProjectionOverlay_config <- function(config, data_folder = "") {
  message("Checking geneProjectionOverlay configuration")

  required_packages <- c("RColorBrewer", "iheatmapr")
  stopIfNotInstalled(required_packages, "geneProjectionOverlay")

  if (is.null(config$coordinates_file)) {
    stop_nice(paste(
      "geneProjectionOverlay:",
      "'coordinates_file' is missing"
    ))
  }

  if (is.null(config$label_column)) {
    stop_nice(paste(
      "geneProjectionOverlay:",
      "'label_column' is missing"
    ))
  }

  config$coordinates_data <-
    read_file(config$coordinates_file, data_folder = data_folder)

  if (!is.character(config$coordinates_data[[1]])) {
    stop_nice(paste(
      "geneProjectionOverlay:",
      "first column must be text"
    ))
  }

  if (!is.numeric(config$coordinates_data[[2]]) |
    !is.numeric(config$coordinates_data[[3]])) {
    stop_nice(paste(
      "geneProjectionOverlay:",
      "second and third columns must be numeric"
    ))
  }

  if (!config$label_column %in% colnames(config$coordinates_data)) {
    stop_nice(paste(
      "geneProjectionOverlay",
      "`label_column` not found in coordinates file"
    ))
  }

  # Unlist because YAML will produce lists instead of named vectors
  if (!is.null(config$annotation_colours)) {
    config$annotation_colours <- lapply(config$annotation_colours, unlist)
  }
  
  # Create numeric-based names for cluster colors
  if (!is.null(config$cluster_colors) & is.null(names(config$cluster_colors))) {
    names(config$cluster_colors) <- seq_along(config$cluster_colors)
  }

  config
}
