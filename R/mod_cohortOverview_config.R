cohortOverviewConfig <- function(config, ...) {
  
  message("Checking cohortOverview configuration")
  
  
  requiredPackages <- c("r2d3")
  stopIfNotInstalled(requiredPackages, "cohortOverview")
  
  if (is.null(config$profile_variables)) {
    stop("cohortOverview: 
         list of 'profile_variables' is missing")
  }
  if (is.null(config$colour_variables)) { 
    stop("cohortOverview: 
         list of 'colour_variables' is missing")
  }
  config
}