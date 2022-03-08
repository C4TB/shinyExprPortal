#' @noRd
geneModulesCorr_config <- function(config, data_folder = "") {
  message("Checking geneModulesCorr configuration")
  
  requiredPackages <- c("plotly", "matrixStats", "htmlwidgets")
  stopIfNotInstalled(requiredPackages, "geneModulesCorr")
  
  if (is.null(config$subset_classes)) {
    stop("geneModulesCorr: subset_classes list missing in configuration file.")
  }
  
  if (is.null(config$sources)) {
    stop("geneModulesCorr: sources list missing in configuration file.")
  }
  if (is.null(config$scatterplot_variables)) {
    stop("geneModulesCorr:
         scatterplot_variables list missing in configuration file.")
  }
  
  modules_data <- lapply(config$sources, function(source_config) {
    
    if (data_folder != "") {
      source_file <- file.path(data_folder, "modules", source_config$file)
    }
    else {
      source_file <- file.path("modules", source_config$file)
    }
    
    if (file_ext(source_file) != "rds") {
      stop("geneModulesCorr: module data must be in rds format")
    }
    
    module_data <- readRDS(source_file)
    validate_modules <- intersect(names(module_data),
                                  c("modules", "descriptions"))
    if (length(validate_modules) != 2) {
      stop("geneModulesCorr: module data must 
           contain two data frames named modules and descriptions")
    }
    
    list(name = source_config$name, data = module_data)
    
  })

  list(subset_classes = config$subset_classes,
       across_class = config$across_class,
       modules_data = modules_data,
       scatterplot_variables = config$scatterplot_variables)
}