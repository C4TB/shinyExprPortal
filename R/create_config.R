#' Title
#'
#' @param target_dir 
#'
#' @return
#' @export
#'
create_config <- function(target_dir = NULL) {
  
  config <- list()
  dir <- target_dir %||% getwd()
  
  message("This wizard will guide you through the creation of the ",
  "configuration file for an expression matrix, a measures file and an ", 
  "optional sample metadata file. Please read the documentation to ensure ",
  "that all files are in the right format.")
  
  expression_file <- readline("Enter name of expression file: ")
  if (expression_file == "") stop("Stopping creation of configuration")
  
  measures_file <- readline("Enter name of measures file: ")
  if (measures_file == "") stop("Stopping creation of configuration")
  
  sample_col <- 
    readline(paste("Enter name of column that contains sample ids in",
                    "measures file: "))
  
  metadata_file <- readLine("(optional) Enter name of sample metadata file:")

  metadata_table <- read_file(metadata_file, data_folder = dir)
  
  about_file <- readLine(paste("(optional) Enter name of text or markdown file",
                          "to appear when visitors enter the portal :"))
  
  if (about_file == "") {
    about_md <- file(file_path(dir, "about.md"))
    about_lines <- c("This is a placeholder about.md file, please edit it")
    writeLines(about_lines, about_md)
    close(about_md)
  }
  
  app_r <- file(file_path(dir, "app.R"))
  app_lines <- c(
    "library(clinvisx)",
    "run_app(config = \"config.yaml\")"
  )
  writeLines(app_lines, app_r)
  close(app_r)
  
}