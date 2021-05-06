singleVariableCorrConfig <- function(config, data_folder = "") { 
  message("Checking singleVariableCorr configuration")
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleVariableCorr")
  }
  config$link_to <- config$link_to %||% NULL
  config
}