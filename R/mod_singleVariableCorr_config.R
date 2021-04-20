singleVariableCorrConfig <- function(config, data_folder = "") { 
  message("Checking singleVariableCorr configuration")
  if (is.null(config$advanced)) { 
    config$advanced <- TRUE
  }
  config
}