cohortOverview_config <- function(config, ...) {
    message("Checking cohortOverview configuration")

    required_packages <- c("r2d3")
    stopIfNotInstalled(required_packages, "cohortOverview")

    if (is.null(config$profile_variables)) {
        stop_nice("cohortOverview: list of 'profile_variables' is missing")
    }
    if (is.null(config$color_variables)) {
        stop_nice("cohortOverview: list of 'color_variables' is missing")
    }
    config
}
