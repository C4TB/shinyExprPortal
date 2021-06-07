geneModulesHeatmapConfig <- function(config, data_folder = "") { 
  message("Checking geneModulesHeatmap configuration")
  
  requiredPackages <- c("RColorBrewer", "DT")
  stopIfNotInstalled(requiredPackages, "geneModulesHeatmap")
  
  if (!is.null(config$modules_table)) {
    message("Loading file: modules_table")
    config$modules_table <- readFile(config$modules_table, "table", data_folder)
  } else stop("geneModulesHeatmap:
              a 'modules_table' file is required for this module")
  
  if (is.null(config$category_variable))
    stop("geneModulesHeatmap:
         'category_variable' to identify models is missing")
  
  if (is.null(config$modules_variable))
    stop("geneModulesHeatmap:
         'modules_variable' to identify modules is missing")
  
  if (is.null(config$genes_variable))
    stop("geneModulesHeatmap:
         'genes_variable' to identify genes is missing")
  
  config
}