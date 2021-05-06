#' @importFrom tools file_ext
#' @noRd
degDetailsConfig <- function(config, data_folder = "") { 
  message("Checking degDetails configuration")
  
  if (not_null(config$models)) {
    models_table <- loadModels(config$models, data_folder)
    config$models <- models_table
  }
  config
}
