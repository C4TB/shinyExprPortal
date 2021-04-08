#' @importFrom tools file_ext
#' @noRd
degSummaryConfig <- function(config, data_folder = "") { 
  
  message("Checking degSummary configuration")
  de_results <- sapply(names(config$models), function(model_name) {
    
    model_configuration <- config$models[[model_name]]
    model_results <- lapply(model_configuration, function(fname) {
      ## Check if data folder was passed
      if (data_folder != "") { 
        fname <- file.path(data_folder, "models", model_name, fname) 
      }
      else {
        fname <- file.path("models", model_name, fname)
      }
      if (!file.exists(fname)) { 
        stop(paste("File ",
                   fname,
                   " in degSummary configuration not found."), call. = FALSE)
      }
      vroom::vroom(fname, col_types = vroom::cols())
      # if (file_ext(fname) == "csv") {
      #   readr::read_csv(fname, col_names = TRUE, col_types = readr::cols())
      # } else {
      #   readr::read_tsv(fname, col_names = TRUE, col_types = readr::cols())
      # }
    })
    names(model_results) <- names(model_configuration)
    return(model_results)
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  de_results
  
  #config
}
