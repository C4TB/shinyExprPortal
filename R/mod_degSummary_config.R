#' @importFrom tools file_ext
#' @noRd
degSummaryConfig <- function(config, data_folder = "") { 
  message("Checking degSummary configuration")
  
  
  requiredPackages <- c("kableExtra")
  stopIfNotInstalled(requiredPackages, "degSummary")
  
  if (not_null(config$models)) {
    models_table <- loadModels(config$models, data_folder)
    config$models <- models_table
  }
  config
}
