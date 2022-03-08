multiVariableCorr_config <- function(config, data_folder = "") { 
  message("Checking multiVariableCorr configuration")
  
  requiredPackages <- c("bsplus", "RColorBrewer", "plotly", "DT",
                        "shinycssloaders")
  stopIfNotInstalled(requiredPackages, "degDetails")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "multiVariableCorr")
  }
 
  if (is.null(config$heatmap_variables))
    stop("multiVariableCorr:
         named 'scatterplot_variables' list is missing")
   
  config
}