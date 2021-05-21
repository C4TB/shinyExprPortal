cohortOverviewConfig <- function(config, ...) {
  
  message("Checking cohortOverview configuration")
  
  
  requiredPackages <- c("r2d3")
  stopIfNotInstalled(requiredPackages, "cohortOverview")
  
  if (is.null(config$profile_variables)) {
    stop("Cohort overview tab: 
         profile variables lists missing in configuration file.")
  }
  if (is.null(config$colour_variables)) { 
    stop("Cohort overview tab: 
         list of colour variables missing in configuration file")  
  }
  config
}