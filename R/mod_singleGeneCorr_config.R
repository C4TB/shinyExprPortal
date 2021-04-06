singleGeneCorrConfig <- function(config, ...) { 

  message("Checking singleGeneCorr configuration")
  if (is.null(config$advanced)) { 
    config$advanced <- TRUE
  }

  if (is.null(config$tabs)) {
    stop("Single gene correlation tab:
         output definitions missing in configuration file.")
  }
  
  if (is.null(config$colour_variables)) {
    stop("Single gene correlation tab:
         list of colour variables missing in configuration file.") 
  }
  config
  
}