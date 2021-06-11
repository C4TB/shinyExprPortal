compareTrajGroupsConfig <- function(config, ...) {
  message("Checking compareTrajGroups configuration")
  
  if (is.null(config$subset_classes))
    stop("compareTrajGroups:
         a list of 'subset_classes' to filter expression is missing")
  
  if (is.null(config$sidebyside_class))
    stop("compareTrajGroups:
         a 'sidebyside_class' to split subjects is missing")
  
  if (is.null(config$trajectory_class))
    stop("compareTrajGroups:
         a 'trajectory_class' to match clinical suffix is missing")
  
  if (is.null(config$compare_variables))
    stop("compareTrajGroups:
         a list of 'compare_variables' for clinical trajectories is missing")
  
  if (!is.null(config$palette)) {
    if (length(config$palette) == 1) {
      
      stopIfNotInstalled(c("RColorBrewer"), "compareTrajGroups")
      stopifnot("compareTrajGroups:
      palette provided is not a valid RColorBrewer palette" =
                  config$palette %in% rownames(RColorBrewer::brewer.pal.info))
    }
  }
  
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "compareTrajGroups")
  }
  config
}