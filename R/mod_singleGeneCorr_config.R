singleGeneCorrConfig <- function(config, ...) { 

  message("Checking singleGeneCorr configuration")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleGeneCorr")
  }

  if (is.null(config$tabs)) {
    stop("singleGeneCorr:
         output definitions missing in configuration file.")
  }
  
  if (is.null(config$colour_variables)) {
    stop("singleGeneCorr:
         list of colour variables missing in configuration file.") 
  }
  config
  
}