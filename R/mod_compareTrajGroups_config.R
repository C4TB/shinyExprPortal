compareTrajGroups_config <- function(config, ...) {
    message("Checking compareTrajGroups configuration")

    if (is.null(config$subset_categories)) {
        stop_nice(
            "compareTrajGroups:",
            "a list of 'subset_categories' to filter expression is missing"
        )
    }

    if (is.null(config$trajectory_category)) {
        stop_nice(
            "compareTrajGroups:",
            "a 'trajectory_category' to match measures suffix is missing"
        )
    }

    if (is.null(config$compare_variables)) {
        stop_nice(
            "compareTrajGroups:",
            "a list of 'compare_variables' for measures trajectories is missing"
        )
    }

    if (!is.null(config$custom_traj_palette)) {
        if (length(config$custom_traj_palette) == 1) {
            stopIfNotInstalled(c("RColorBrewer"), "compareTrajGroups")
            if (!config$custom_traj_palette
                %in% rownames(RColorBrewer::brewer.pal.info)) {
                stop_nice(
                    "compareTrajGroups:",
                    "'custom_traj_palette' provided is not a valid RColorBrewer
                    palette"
                )
            }
        }
    }

    if (!is.null(config$advanced)) {
        validateAdvancedSettings(config$advanced, "compareTrajGroups")
    }
    config
}
