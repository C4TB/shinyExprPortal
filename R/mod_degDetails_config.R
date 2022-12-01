degDetails_config <- function(config, data_folder = "", parent_config = NULL) {
    message("Checking degDetails configuration")

    if (is.null(config$category_variable)) {
        stop_nice(
            "degDetails:",
            "'category_variable' to identify model results is missing"
        )
    }

    config$pvalue_max <-
        config$pvalue_max %||% parent_config$pvalue_max %||% 0.05
    config$padj_max <-
        config$padj_max %||% parent_config$padj_max %||% 0.05
    config$pvalue_col <-
        config$pvalue_col %||% parent_config$pvalue_col %||% "P.value"
    config$padj_col <-
        config$padj_col %||% parent_config$padj_col %||% "q.value"

    if (!is.null(config$custom_point_colors) &
        (!is.list(config$custom_point_colors))) {
        stop_nice("degDetails: 'custom_points_colors' must be a list")
    }
    config$custom_point_colors <-
        config$custom_point_colors %||% c("black", "green", "blue", "red")

    if (is.null(parent_config$models) & is.null(config$models)) {
        stop_nice("degDetails: 'models' tablle not found in configuration")
    }

    if (is.null(parent_config$models) & !is.null(config$models)) {
        models_table <-
            loadModels(
                config$models,
                data_folder,
                config$pvalue_max,
                config$padj_max,
                config$pvalue_col,
                config$padj_col
            )
        config$models <- models_table
    }
    config
}
