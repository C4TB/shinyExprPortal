singleMeasureCorr_config <- function(config, data_folder = "") {
  message("Checking singleMeasureCorr configuration")

  required_packages <- c("DT")
  stopIfNotInstalled(required_packages, "singleMeasureCorr")

  if (is.logical(config)) 
    if (config)
      config <- list()
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleMeasureCorr")
  }

  config
}
