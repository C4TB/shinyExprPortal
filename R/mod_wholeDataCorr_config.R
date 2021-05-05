wholeDataCorrConfig <- function(config, data_folder = "") { 
  message("Checking wholeDataCorr configuration")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "wholeDatacorr")
  }
  
  #list(subset_clinical_variable = config$subset_clinical_variable,
  list(heatmap_variables = config$heatmap_variables,
       advanced = config$advanced)
}