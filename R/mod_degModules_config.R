#' @noRd
degModules_config <- function(config, data_folder = "") {
  message("Checking degModules configuration")
  modules <- config$modules
  
  if (is.null(config$models)) 
    stop_nice("degModules: 'models' table must be defined if using degModules")
  
  
  config$pvalue_max<- config$pvalue_max %||% 0.05
  config$padj_max <- config$padj_max %||% 0.05
  config$pvalue_col <- config$pvalue_col %||% "P.value"
  config$padj_col <- config$padj_col %||% "q.value"
  
  models_table <-
    loadModels(config$models,
               data_folder,
               config$pvalue_max,
               config$padj_max,
               config$pvalue_col,
               config$padj_col)
  config$models <- models_table
  
  submodules_config <-
    lapply(names(modules), function(module_name) {
      if (!is.null(modules[[module_name]])) {
        do.call(
          paste0(module_name, "_config"),
          list(
            config = modules[[module_name]],
            data_folder = data_folder,
            parent_config = config
          )
        )
      }
    })
  config$modules <- stats::setNames(submodules_config, names(modules))
  config
}
