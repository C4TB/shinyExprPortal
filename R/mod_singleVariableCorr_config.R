singleVariableCorr_config <- function(config, data_folder = "") { 
  message("Checking singleVariableCorr configuration")
  
  requiredPackages <- c("DT")
  stopIfNotInstalled(requiredPackages, "singleVariableCorr")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleVariableCorr")
  }
  
  config
}