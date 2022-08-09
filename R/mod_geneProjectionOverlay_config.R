# Config template

#' @noRd
geneProjectionOverlay_config <- function(config, data_folder = "") {
  message("Checking geneProjectionOverlay configuration")

  required_packages <- c("RColorBrewer", "iheatmapr")
  stopIfNotInstalled(required_packages, "geneProjectionOverlay")

  if (is.null(config$coordinates_file)) {
    stop_nice("geneProjectionOverlay: 'coordinates_file' is missing")
  }

  if (is.null(config$group_variable)) {
    stop_nice("geneProjectionOverlay: 'group_variable' is missing")
  }

  config$coordinates_data <-
    read_file(config$coordinates_file, data_folder = data_folder)

  if (!is.character(config$coordinates_data[[1]])) {
    stop_nice("geneProjectionOverlay: first column must be text")
  }

  if (!is.numeric(config$coordinates_data[[2]]) |
    !is.numeric(config$coordinates_data[[3]])) {
    stop_nice("geneProjectionOverlay: second and third columns must be numeric")
  }

  if (!config$group_variable %in% colnames(config$coordinates_data)) {
    stop_nice(
      "geneProjectionOverlay",
      "`group_variable` not found in coordinates file"
    )
  }

  # Unlist because YAML will produce lists instead of named vectors
  if (!is.null(config$custom_annotation_colors)) {
    config$custom_annotation_colors <-
      lapply(config$custom_annotation_colors, unlist)
  }
  
  # Create numeric-based names for cluster colors
  if (!is.null(config$custom_group_colors) 
      & is.null(names(config$custom_group_colors))) {
    names(config$custom_group_colors) <-
      seq_along(config$custom_group_colors)
  }

  config
}
