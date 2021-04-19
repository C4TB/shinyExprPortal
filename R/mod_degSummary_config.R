#' @importFrom tools file_ext
#' @noRd
degSummaryConfig <- function(config, data_folder = "") { 
  message("Checking degSummary configuration")
  
  if (!"kableExtra" %in% rownames(installed.packages())) {
    stop("Package kableExtra required for degSummary module not found")
  }
  
  if (not_null(config$models)) {
    models_table <- loadModels(config$models, data_folder)
    config$models <- models_table
  }
  config
}
