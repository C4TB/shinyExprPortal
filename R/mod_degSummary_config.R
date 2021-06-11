#' @noRd
degSummaryConfig <- function(config, data_folder = "") { 
  message("Checking degSummary configuration")
  
  requiredPackages <- c("knitr", "kableExtra")
  stopIfNotInstalled(requiredPackages, "degSummary")
  
  if (is.null(config$partition_variable))
    stop("degSummary:
         'partition_variable' to split results table is missing")
  
  if (not_null(config$models)) {
    models_table <- loadModels(config$models, data_folder)
    config$models <- models_table
  }
  config
}
