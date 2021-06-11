#' @noRd
degDetailsConfig <- function(config, data_folder = "") { 
  message("Checking degDetails configuration")
  
  requiredPackages <- c("bsplus", "plotly", "DT", "shinycssloaders")
  stopIfNotInstalled(requiredPackages, "degDetails")
  
  if (is.null(config$category_variable)) {
    stop("degDetails: 
         'category_variable' to identify model results is missing")
  }
  
  if (not_null(config$models)) {
    models_table <- loadModels(config$models, data_folder)
    config$models <- models_table
  }
  config
}
