wholeDataCorrConfig <- function(config, data_folder = "") { 
  message("Checking wholeDataCorr configuration")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "wholeDatacorr")
  }
  
  list(link_to = config$link_to,
       heatmap_variables = config$heatmap_variables,
       advanced = config$advanced)
}