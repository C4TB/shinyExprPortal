#' Parse a configuration file for the Shiny app
#'
#' @param fname The name of the yaml configuration file
#' @param data_folder Optional directory prefix for data files.
#' Enables using the same configuration file under different file structures.
#' @param test_module A module_name to parse configuration and ignore all others
#'
#' @return config named list
#' @noRd
parseConfig <- function(fname, data_folder = "", test_module = NULL) {
  message(paste("Reading configuration file", fname))
  raw_config <- yaml::read_yaml(fname)
  
  available_modules <- get_golem_config("available_modules")
  config <- list()
  golem::add_resource_path(prefix = "local",
                           directoryPath = file_path(data_folder, "www"))
  config$data_folder <- data_folder
  config$bootstrap <- 
    raw_config$bootstrap %||% list(version = 4)
  config$name <- raw_config$name %||% "clinvisx"
  if (not_null(raw_config$logo))
  if (file.exists(file_path(data_folder, "www", raw_config$logo))) {
    config$logo <-
        img(
          src = file_path("local", raw_config$logo),
          #height = "45px",
          title = config$name
        )
  } else {
    stop_nice(paste("Logo image file",raw_config$logo,"not found"))
  }
  config$iconMenu <- raw_config$iconMenu %||% NULL
  
  if (is.null(raw_config$about)) {
    config$about <- NULL
  } else {
    about_file <- file_path(data_folder, raw_config$about)
    if (file.exists(about_file)) {
      config$about <- about_file  
    } else {
      stop_nice(paste("'About' file",about_file,"not found."))
    }
  }
  
  # Global other settings
  config$max_p <- raw_config$max_p %||% 0.05
  config$padj_col <- raw_config$padj_col %||% "q.value"
  config$adjust_method <- raw_config$adjust_method %||% "q.value"
  
  # Validate data section
  if (is.null(raw_config$data)) {
    stop_nice("'data' section missing in configuration file")
  }
  if (is.null(raw_config$data$clinical) ||
      is.null(raw_config$data$sample_lookup) ||
              is.null(raw_config$data$expression_matrix)) {
    stop_nice(
      paste("Data section in configuration file must include:",
      "clinical, sample_lookup and expression_matrix files.")
    )
  }

  # Load data section
  loaded_data <- lapply(names(raw_config$data), function(file_type) {
    message("Loading file: ", file_type, "\n", appendLF = FALSE)
    if (file_type == "models") {
      loadModels(raw_config$data[[file_type]], data_folder,config$max_p, config$padj_col)
    } else {
      readFile(raw_config$data[[file_type]], file_type, data_folder)  
    }
  })
  # Set global settings for sample and subject column
  config$sample_categories <- raw_config$sample_categories
  config$sample_variable <-
    raw_config$sample_variable %||% "Sample_ID"
  config$subject_variable <-
    raw_config$subject_variable %||% "Subject_ID" 
  
  # Check if number of samples and subjects match across clinical data
  # And matrices
  # There should be no samples without a subject and no subjects without samples
  validateData(loaded_data,
               sample_variable = config$sample_variable,
               subject_variable = config$subject_variable
               )
  names(loaded_data) <- names(raw_config$data)
  config$data <- loaded_data
  
  if (not_null(test_module)) {
    # Check single module configuration
    message("Module test mode")
    if (test_module %in% available_modules) {
      if (!is.null(raw_config[[test_module]])) {
        mod_conf <- do.call(paste0(test_module, "_config"),
                list(config = raw_config[[test_module]],
                     data_folder = data_folder))
        loaded_modules <- list(mod_conf)
        names(loaded_modules) <- c(test_module)
      }
    } else {
      stop_nice("Module not found. Check if package supports it or spelling.")
    }
  } else {
    # Parse module configuration by calling each available module function
    # NB: The app does not check for wrong spelling of module names
    loaded_modules <-
      lapply(available_modules, function(module_name) {
        if (!is.null(raw_config[[module_name]]))
          do.call(paste0(module_name, "_config"),
                  list(config = raw_config[[module_name]],
                       data_folder = data_folder))
      })
    names(loaded_modules) <- available_modules
  }
  config$modules <- loaded_modules
  config
}

validateData <-
  function(datafiles,
           sample_variable = "Sample_ID",
           subject_variable = "Subject_ID") {
    
  expression_matrix <- datafiles[["expression_matrix"]]
  clinical <- datafiles[["clinical"]]
  sample_lookup <- datafiles[["sample_lookup"]]
  
  clinical_subjects <- clinical[, subject_variable]
  lookup_subjects <- unique(sample_lookup[, subject_variable])
  lookup_samples <- sample_lookup[, sample_variable]
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
    stop_nice(paste("Problem(s) found in data files.",
                    "See printed message(s) for details"))
}

#' User-friendly readfile
#'
#' @param filename filename
#' @param filetype "expression_matrix" for matrix type
#' @param data_folder Optional directory prefix for file
#'
#' @return parsed file
#' @noRd
readFile <- function(filename, filetype = "", data_folder = "") { 
  fext <- file_ext(filename)
  filename <- file_path(data_folder, filename) 
  if (!file.exists(filename)) { 
    stop_nice(paste("File ", filename, " in yaml configuration not found."))
  }
  tryCatch({
    if (fext == "rds") {
      readRDS(filename)
    } else if (fext == "fst") {
      if (!requireNamespace("fst", quietly = TRUE)) {
        stop_nice("Package fst is required for reading .fst files")
      }
      df <- fst::read_fst(filename)
      if (filetype == "expression_matrix")
      {
        rnames <- df[,1]
        df <- as.matrix(df[-1])
        rownames(df) <- rnames
        df
      }
      else df
    } else {
      if (filetype == "expression_matrix") {
        as.matrix(data.table::fread(filename))
      } 
      if (filetype == "edge_list") {
        col_names <- c("source", "target", "weight")
        delim <- ifelse(fext == "csv", ",", "\t")
        as.data.frame(vroom::vroom(filename, delim, col_names = col_names))
      } else {
        delim <- ifelse(fext == "csv", ",", "\t")
          as.data.frame(
            vroom::vroom(filename, delim = delim, col_types = vroom::cols()))
      }
    }
  },
  error = function(errormsg) {
    stop(errormsg)
  },
  warning = function(warningmsg) {
    stop(warningmsg)
  })
  
}

loadModels <- 
  function(models_file, data_folder = "", max_p = 0.05, padj_col = "q.value") {
  fext <- file_ext(models_file)
  if (fext == "rds") {
    models_table <- readRDS(file_path(data_folder, models_file))
  } else {
    delim <- ifelse(fext == "csv", ",", "\t")
    models_table <- vroom::vroom(file_path(data_folder, models_file),
                                 delim = delim,
                                 col_types = vroom::cols())
  }
  if (ncol(models_table) < 2)
  {
    stop_nice(paste("models_table loaded incorrectly.", 
    "Use comma separator in .csv files and tab with any other file extension"))
  }
  if (!"File" %in% colnames(models_table)) {
    stop_nice("'File' column missing from models_table.")
  }
  # Go through list of files and load them
  models_table$Data <- lapply(models_table$File, function(file_name) {
    file_name <- file_path(data_folder, "models" ,file_name)
    if (!file.exists(file_name)) { 
      stop_nice(paste("Model file ",
                 file_name,
                 " from degModules configuration not found."))
    }
    fext <- file_ext(file_name)
    delim <- ifelse(fext == "csv", ",", "\t")
    model <- vroom::vroom(file_name, delim = delim, col_types = vroom::cols())
  })
  
  models_table$P <- sapply(models_table$Data,
                           function(x) nrow(x[x[["P.value"]] < max_p, ]))
  models_table$P_adj <- sapply(models_table$Data,
                           function(x) nrow(x[x[[padj_col]] < max_p, ]))
  models_table
}

validateAdvancedSettings <- function(config, module_title = "") { 
  valid_settings <-
    c("clinical_outliers",
      "expression_outliers",
      "correlation_method",
      "fit_method")
  diff_advanced <- setdiff(names(config), valid_settings)
  if (length(diff_advanced) > 0) {
    stop_nice(paste(module_title, ": invalid advanced setting: \n\t",
         diff_advanced,
         "\nMust be one of\n\t", paste0(valid_settings, collapse = "\n\t")))
  }
}

# from tools::file_ext
file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
