#' @importFrom tools file_ext
#' @noRd
degOverviewConfig <- function(config, data_folder = "") { 
  message("Checking degOverview configuration")
  
  if (not_null(config$models)) {
    models_table <- loadModels(config$models, data_folder)
    config$models <- models_table
  }
  config
}
