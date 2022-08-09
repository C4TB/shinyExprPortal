#' @noRd
degDetails_config <- function(config, data_folder = "", parent_config = NULL) {
  message("Checking degDetails configuration")

  if (is.null(config$category_variable)) {
    stop_nice(paste(
      "degDetails:",
      "'category_variable' to identify model results is missing"
    ))
  }

  config$pvalue_max <- 
    config$pvalue_max %||% parent_config$pvalue_max %||% 0.05
  config$padj_max <-
    config$padj_max %||% parent_config$padj_max %||% 0.05
  config$pvalue_col <-
    config$pvalue_col %||% parent_config$pvalue_col %||% "P.value"
  config$padj_col <-
    config$padj_col %||% parent_config$padj_col %||% "q.value"

  
  if (is.null(parent_config$models) & is.null(config$models)) {
    stop_nice("degDetails: 'models' tablle not found in configuration")
  }
  
  if (is.null(parent_config$models) & !is.null(config$models)) {
    models_table <-
      loadModels(config$models,
                 data_folder,
                 config$pvalue_max,
                 config$padj_max,
                 config$pvalue_col,
                 config$padj_col)
    config$models <- models_table
  }
  config
}
