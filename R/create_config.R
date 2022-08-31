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
  
  message(
    strwrap("This wizard will guide you through the creation of the
configuration file for an expression matrix, a measures file and an 
optional sample metadata file. Please read the documentation to
ensure that all files are in the right format.",
            prefix = "\n", initial = "")
  )

  cat("\n")
  
  message(
    strwrap(
    "Use this wizard if your data has one sample per patient/subject.
If your data contains multiple samples per patients, see
vignette(\"dataprep\") or the package website for a data
preparation tutorial.",
            prefix = "\n", initial = "")
  )
  
  cat("\n")
  
  message(
    strwrap("The configuration file will include correlation and differential
expression visualisation modules. See vignette(\"fullguide\") or the package
website for a complete module configuration guide.",
            prefix = "\n", initial = "")
  )
  
  continue_yn <- readline("Do you want to continue? (y/n)")
  
  if ((continue_yn == "") | (continue_yn == "n"))
    stop("Stopping creation of configuration")
  
  expression_file <- readline("Enter name of expression file: ")
  if (expression_file == "") stop("Stopping creation of configuration")
  
  expression_matrix <- read_file(expression_file, "expression_matrix", dir)
  sample_id <- colnames(expression_matrix)
  
  measures_file <- readline("Enter name of measures file: ")
  if (measures_file == "") stop("Stopping creation of configuration")
  
  measures_table <- read_file(measures_file, data_folder = dir)
  
  
  cat("(optional) Enter name of column that contains subject identifiers ",
        "in measures file.\n")
  subject_col <- readline("Leave empty to use first column.")

  if (subject_col == "") subject_col <- colnames(measures_table)[[1]]
  
  lookup_table <- data.frame(
    sample_id,
    measures_table[[subject_col]]
  )
  
  metadata_file <- readline("(optional) Enter name of sample metadata file:")

  if (metadata_file != "") {
    metadata_table <- read_file(metadata_file, data_folder = dir)
    lookup_table <- cbind(lookup_table, metadata_table)
  }
  
  readr::write_csv(lookup_table, file.path(dir, "lookup_table.csv"))
  
  data_list <- list(
    clinical = measures_file,
    lookup = "lookup.csv",
    expression_matrix = expression_file
  )
  
  sample_categories <- lapply(seq_len(ncol(metadata_table)-1), function(j) {
    name <- colnames(metadata_table)[[j]]
    values <- unique(metdata_table[, j])
    list(name = name, label = name, values = c(values))
  })
  
  about_file <- readLine(paste("(optional) Enter name of text or markdown file",
                          "to appear when visitors enter the portal :"))
  
  if (about_file == "") {
    message("Creating placeholder about.md file")
    about_md <- file(file_path(dir, "about.md"))
    about_lines <- c("This is a placeholder about.md file.",
                     "Please edit it using your favorite text editor.")
    writeLines(about_lines, about_md)
    close(about_md)
  }
  
  project_name <- readline("(optional) Enter short name of project:")
  
  if (project_name != "")
    config$name <- project_name
  config$about <- about_file
  config$data <- data_list
  config$sample_variable <- "sample_id"
  config$subject_variable <- subject_col
  config$sample_categories <- sample_categories
  
  yaml::write_yaml(config, file.path(dir, "config.yaml"))
  
  
  app_r <- file(file_path(dir, "app.R"))
  app_lines <- c(
    "library(clinvisx)",
    "run_app(config = \"config.yaml\")"
  )
  writeLines(app_lines, app_r)
  close(app_r)
  
}