singleVariableCorrConfig <- function(config, data_folder = "") { 
  message("Checking singleVariableCorr configuration")
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleVariableCorr")
  }
  config
}