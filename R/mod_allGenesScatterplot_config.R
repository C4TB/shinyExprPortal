# Config template

#' @noRd
allGenesScatterplot_config <- function(config, data_folder = "") {
  message("Checking allGenesScatterplot configuration")

  required_packages <- c("iheatmapr")
  stopIfNotInstalled(required_packages, "allGenesScatterplot")

  if (is.null(config$coordinates_file)) {
    stop_nice(paste(
      "allGenesScatterplot:",
      "'coordinates_file' is missing"
    ))
  }

  if (is.null(config$label_column)) {
    stop_nice(paste(
      "allGenesScatterplot:",
      "'label_column' is missing"
    ))
  }

  config$coordinates_data <-
    readFile(config$coordinates_file, data_folder = data_folder)

  if (!is.character(config$coordinates_data[[1]])) {
    stop_nice(paste(
      "allGenesScatterplot:",
      "first column must be text"
    ))
  }

  if (!is.numeric(config$coordinates_data[[2]]) |
    !is.numeric(config$coordinates_data[[3]])) {
    stop_nice(paste(
      "allGenesScatterplot:",
      "second and third columns must be numeric"
    ))
  }

  if (!config$label_column %in% colnames(config$coordinates_data)) {
    stop_nice(paste(
      "allGenesScatterplot",
      "`label_column` not found in coordinates file"
    ))
  }

  # Unlist because YAML will produce lists instead of named vectors
  if (!is.null(config$annotation_colours)) {
    config$annotation_colours <- lapply(config$annotation_colours, unlist)
  }

  config
}
