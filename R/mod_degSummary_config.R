#' @noRd
degSummary_config <- function(config, data_folder = "", parent_config = NULL) {
  message("Checking degSummary configuration")

  required_packages <- c("knitr", "kableExtra")
  stopIfNotInstalled(required_packages, "degSummary")

  # Module configuration has priority over group module configuration
  config$max_p <-
    config$max_p %||% parent_config$max_p %||% 0.05
  config$max_p_adj <-
    config$max_p_adj %||% parent_config$max_p_adj %||% 0.05
  config$pvalue_col <-
    config$pvalue_col %||% parent_config$pvalue_col %||% "P.value"
  config$padj_col <-
    config$padj_col %||% parent_config$padj_col %||% "adj.P.Val"

  if (is.null(parent_config$models) & is.null(config$models)) {
    stop_nice("degDetails: 'models' table not found in configuration")
  }
  
  if (is.null(parent_config$models) & !is.null(config$models)) {
    models_table <-
      loadModels(config$models,
                 data_folder,
                 config$max_p,
                 config$max_p_adj,
                 config$pvalue_col,
                 config$padj_col)
    config$models <- models_table
  }
  config
}
