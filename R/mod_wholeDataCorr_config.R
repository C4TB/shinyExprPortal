wholeDataCorrConfig <- function(config, data_folder = "") { 
  message("Checking wholeDataCorr configuration")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "wholeDatacorr")
  }
 
  if (is.null(config$heatmap_variables))
    stop("wholeDataCorr:
         named 'scatterplot_variables' list is missing")
   
  config
}