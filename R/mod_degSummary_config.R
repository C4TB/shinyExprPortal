#' @importFrom tools file_ext
#' @noRd
degSummaryConfig <- function(config, data_folder = "") { 
  message("Checking degSummary configuration")
  
  if (!"kableExtra" %in% rownames(installed.packages())) {
    stop("Package kableExtra required for degSummary module not found")
  }
  
  config
}
