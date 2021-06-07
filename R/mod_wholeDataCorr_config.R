wholeDataCorrConfig <- function(config, data_folder = "") { 
  message("Checking wholeDataCorr configuration")
  
  requiredPackages <- c("bsplus", "RColorBrewer", "plotly", "DT",
                        "shinycssloaders")
  stopIfNotInstalled(requiredPackages, "degDetails")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "wholeDatacorr")
  }
 
  if (is.null(config$heatmap_variables))
    stop("wholeDataCorr:
         named 'scatterplot_variables' list is missing")
   
  config
}