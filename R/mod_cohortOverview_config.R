cohortOverviewConfig <- function(config, ...) {
  
  message("Checking cohortOverview configuration")
  
  if (!requireNamespace("r2d3", quietly = TRUE)) {
    stop("Package \"r2d3\" needed for this function to work.
         Please install it.",
         call. = FALSE)
  }
  
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