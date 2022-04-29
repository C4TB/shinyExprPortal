#' @noRd
degSummary_config <- function(config, data_folder = "") { 
  message("Checking degSummary configuration")
  
  requiredPackages <- c("knitr", "kableExtra")
  stopIfNotInstalled(requiredPackages, "degSummary")
  
  if (is.null(config$partition_variable))
    stop_nice(paste("degSummary:",
         "'partition_variable' to split results table is missing"))
  
  config$max_p <- config$max_p %||% 0.05
  config$padj_col <- config$padj_col %||% "q.value"
  
  if (not_null(config$models)) {
    models_table <- 
      loadModels(config$models, data_folder, config$max_p, config$padj_col)
    config$models <- models_table
  }
  config
}
