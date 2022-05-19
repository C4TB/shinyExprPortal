#' @noRd
degDetails_config <- function(config, data_folder = "") { 
  message("Checking degDetails configuration")
  
  requiredPackages <- c("bsplus", "plotly", "DT")
  stopIfNotInstalled(requiredPackages, "degDetails")
  
  if (is.null(config$category_variable)) {
    stop_nice(paste("degDetails:",
         "'category_variable' to identify model results is missing"))
  }
  
  config$max_p <- config$max_p %||% 0.05
  config$padj_col <- config$padj_col %||% "q.value"
  
  if (not_null(config$models)) {
    models_table <- 
      loadModels(config$models, data_folder, config$max_p, config$padj_col)
    config$models <- models_table
  }
  config
}
