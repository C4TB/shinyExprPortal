#' @importFrom tools file_ext
#' @noRd
degModulesConfig <- function(config, data_folder = "") { 
  
  message("Checking degModules configuration")
  models_table <- vroom::vroom(file_path(data_folder, config$models),
                                         col_types = vroom::cols())
  models_table$Data <- lapply(models_table$File, function(file_name) {
    file_name <- file_path(data_folder, "models" ,file_name)
    if (!file.exists(file_name)) { 
      stop(paste("Model file ",
                 file_name,
                 " from degModules configuration not found."), call. = FALSE)
    }
    vroom::vroom(file_name, col_types = vroom::cols())  
  })
  
  models_table$pSignif <- sapply(models_table$Data,
                         function(x) nrow(x[which(x$p.value < 0.05), ]))
  models_table$qSignif <- sapply(models_table$Data,
                         function(x) nrow(x[which(x$q.value < 0.05), ]))
  list(modules = config$modules, models = models_table)
  
  #config
}
