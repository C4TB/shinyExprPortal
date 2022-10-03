singleVariableCorr_config <- function(config, data_folder = "") {
  message("Checking singleVariableCorr configuration")

  required_packages <- c("DT")
  stopIfNotInstalled(required_packages, "singleVariableCorr")

  if (class(config) == "logical") 
    if (config)
      config <- list()
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleVariableCorr")
  }

  config
}
