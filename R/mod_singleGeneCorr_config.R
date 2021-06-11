singleGeneCorrConfig <- function(config, ...) { 

  message("Checking singleGeneCorr configuration")
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleGeneCorr")
  }

  if (is.null(config$tabs)) {
    stop("singleGeneCorr:
         'tabs' definitions are missing")
  }
  
  if (is.null(config$colour_variables)) {
    stop("singleGeneCorr:
         list of 'colour_variables' is missing") 
  }
  config
  
}