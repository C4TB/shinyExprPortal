#' Parse a configuration file for the Shiny app
#'
#' @param fname The name of the yaml configuration file
#' @param data_folder Optional directory prefix for data files.
#' Enables using the same configuration file under different file structures.
#' @param test_module A module_name to parse configuration and ignore all others
#'
#' @return appdata named list, containing `data`, `modules` and `global`
#' @noRd
parseConfig <- function(fname, data_folder = "", test_module = NULL) {
  message(paste("Reading configuration file", fname))
  config <- yaml::read_yaml(fname)
  available_modules <- get_golem_config("available_modules")
  appdata <- list()
  golem::add_resource_path(prefix = "local", directoryPath = file_path(data_folder, "www"))
  
  appdata[["name"]] <- config$name %||% "clinvisx"
  if (not_null(config$logo))
  if (file.exists(file_path(data_folder, "www", config$logo))) {
    appdata$logo <-
        img(
          src = file_path("local", config$logo),
          style = "height: 60px;
                  margin-top: -14px;
                  padding-right:10px;
                  padding-bottom:10px",
          title = appdata[["name"]]
        )
  } else {
    stop("Logo image not found")
  }
  
  appdata[["about"]] <- config$about %||% NULL
  if (is.null(config$about)) {
    appdata[["about"]] <- NULL
  } else {
    appdata[["about"]] <- file_path(data_folder, config$about)
  }
  
  # Validate data section
  if (is.null(config$data)) {
    stop("Data section missing in configuration file")
  }
  if (is.null(config$data["clinical"]) ||
      is.null(config$data["sample_lookup"]) ||
              is.null(config$data["expression_matrix"])) {
    stop(
      "Data section in configuration file must include: clinical,
      sample_lookup and expression files."
    )
  }

  # Load data section
  loaded_data <- lapply(names(config$data), function(filename) {
    message("Loading file: ", filename)
    readFile(config$data[[filename]], filename, data_folder)
  })
  # Set global settings for sample and subject column
  config$global[["sample_col"]] <-
    config$global$sample_column %||% "Sample_ID"
  config$global[["subject_col"]] <-
    config$global$subject_column %||% "Subject_ID" 
  
  validateData(loaded_data,
               cols = list(
                 sample_col = config$global[["sample_col"]],
                 subject_col = config$global[["subject_col"]]
               ))
  names(loaded_data) <- names(config$data)
  appdata[["data"]] <- loaded_data
  
  if (not_null(test_module)) {
    # Check single module configuration
    message("Module test mode")
    if (test_module %in% available_modules) {
      if (!is.null(config[[test_module]])) {
        mod_conf <- do.call(paste0(test_module, "Config"),
                list(config = config[[test_module]],
                     data_folder = data_folder))
        appdata_modules <- list(mod_conf)
        names(appdata_modules) <- c(test_module)
      }
    } else {
      stop("Module not found. Check if package supports it or spelling.")
    }
  } else {
    # Parse module configuration by calling each available module function
    appdata_modules <-
      lapply(available_modules, function(module_name, config) {
        if (!is.null(config[[module_name]]))
          do.call(paste0(module_name, "Config"),
                  list(config = config[[module_name]],
                       data_folder = data_folder))
      }, config = config)
    names(appdata_modules) <- available_modules
  }
  appdata[["modules"]] <- appdata_modules
  appdata[["config"]] <- config$global
  appdata
}

validateData <-
  function(datafiles,
           cols = list(sample_col = "Sample_ID",
                       subject_col = "Subject_ID")) {
    
  expression_matrix <- datafiles[["expression_matrix"]]
  clinical <- datafiles[["clinical"]]
  sample_lookup <- datafiles[["sample_lookup"]]
  
  clinical_subjects <- clinical[, cols$subject_col]
  lookup_subjects <- unique(sample_lookup[, cols$subject_col])
  lookup_samples <- sample_lookup[, cols$sample_col]
  expression_samples <- colnames(expression_matrix)
  
  error <- FALSE
  if (!isTRUE(all.equal(sort(clinical_subjects), sort(lookup_subjects)))) {
    message("ERROR: Subjects in clinical and lookup tables do not match.")
    nclin <- setdiff(clinical_subjects, lookup_subjects)
    if (length(nclin) > 0) {
      message("Subjects in clinical table not found in lookup:")
      print(nclin) 
    }
    nlookup <- setdiff(lookup_subjects, clinical_subjects)
    if (length(nlookup) > 0) {
      message("Subjects in lookup table not found in clinical:")
      print(nlookup)
    }
    error <- TRUE
  }
  if (!isTRUE(all.equal(sort(expression_samples), sort(lookup_samples)))) {
    message(
      "ERROR: Samples in expression matrix and lookup tables do no match.")
    nexp <- setdiff(expression_samples, lookup_samples)
    if (length(nexp) > 0) {
      message("Samples from matrix not found in lookup table:")
      print(nexp)
    }
    nlookup <- setdiff(lookup_samples, expression_samples)
    if (length(nlookup) > 0) {
      message("Samples in lookup table not found in expression matrix:")
      print(nlookup)
    }
    error <- TRUE
  }
  if (error) 
    stop("Problem found in data files. See printed messages for details.",
         call. = FALSE)
}

#' User-friendly readfile
#'
#' @param filename filename
#' @param filetype "matrix" or something else
#' @param data_folder Optional directory prefix for file
#'
#' @return parsed file
#' @noRd
readFile <- function(filename, filetype, data_folder) { 
  fext <- tools::file_ext(filename)
  filename <- file_path(data_folder, filename) 
  if (!file.exists(filename)) { 
    stop(paste("File ", filename, " in yaml configuration not found."),
         call. = FALSE)
  }
  tryCatch({
    if (fext == "rds") {
      readRDS(filename)
    } else {
      if (filetype == "expression_matrix") {
        as.matrix(data.table::fread(filename))
      } else {
        as.data.frame(vroom::vroom(filename, col_types = vroom::cols()))
      }
    }
    # else if (fext == "csv") {
    #   read.csv(filename)
    # }
    # else if (fext == "tsv") { 
    #   read.delim(filename)
    # }
  },
  error = function(errormsg) {
    stop(errormsg)
  },
  warning = function(warningmsg) {
    stop(warningmsg)
  })
  
}

