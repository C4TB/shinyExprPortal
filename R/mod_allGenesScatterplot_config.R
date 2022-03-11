# Config template

#' @noRd
allGenesScatterplot_config <- function(config, data_folder = "") { 
  message("Checking allGenesScatterplot configuration")
  
  #requiredPackages <- c("")
  #stopIfNotInstalled(requiredPackages, "MODULENAME")
  
  if (is.null(config$coordinates_file)) {
    stop("allGenesScatterplot: 
         'coordinates_file' is missing")
  }
  
  if (is.null(config$annotation_column)) {
    stop("allGenesScatterplot: 
         'annotation_column' is missing")
  }
  
  config$coordinates_data <-
    readFile(config$coordinates_file, data_folder = data_folder)
  
  config
}