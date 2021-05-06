compareTrajGroupsConfig <- function(config, ...) {
  message("Checking compareTrajGroups configuration")
  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "compareTrajGroups")
  }
  config
}