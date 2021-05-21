geneModulesHeatmapConfig <- function(config, data_folder = "") { 
  message("Checking geneModulesHeatmap configuration")
  
  if (!is.null(config$modules_table)) {
    message("Loading file: modules_table")
    config$modules_table <- readFile(config$modules_table, "table", data_folder)
  } else stop("geneModulesHeatmap:
              a modules_table file is required for this module")
  if (is.null(config$category_column))
    stop("geneModulesHeatmap:
         a category_column to identify modules is required")
  
  config
}