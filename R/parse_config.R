#' Parse a configuration file for the Shiny app
#'
#' @param fname The name of the yaml configuration file
#' @param data_folder Optional directory prefix for data files.
#' Enables using the same configuration file under different file structures.
#' @param test_module A module_name to parse configuration and ignore all others
#' @param custom_modules List of custom modules name to load
#' @param nthreads Optional number of threads for data.table
#'
#' @return config named list
#' @noRd
parseConfig <-
  function(fname,
           data_folder = "",
           test_module = NULL,
           custom_modules = NULL,
           nthreads = 1L) {
    message("Reading configuration file: ", fname)

    if (!file.exists(fname)) {
      stop_nice("File ", fname, " not found")
    }
    raw_config <- yaml::read_yaml(fname)

    available_modules <- get_golem_config("available_modules")
    config <- list()
    config$nthreads <- nthreads
    add_resource_path(
      prefix = "local",
      directoryPath = file_path(data_folder, "www")
    )
    config$data_folder <- data_folder

    # Read app metadata ----
    # Settings:
    # - bootstrap
    # - name
    # - logo
    # - iconMenu
    # - about

    config$bootstrap <-
      raw_config$bootstrap %||% list(version = 4)
    config$name <- raw_config$name %||% ""
    if (!is.null(raw_config$logo)) {
      if (file.exists(file_path(data_folder, "www", raw_config$logo))) {
        config$logo <-
          img(
            src = file_path("local", raw_config$logo),
            title = config$name
          )
      } else {
        stop_nice("Logo image file ", raw_config$logo, " not found")
      }
    }
    config$iconMenu <- raw_config$iconMenu %||% NULL

    if (is.null(raw_config$about)) {
      config$about <- NULL
    } else {
      about_file <- file_path(data_folder, raw_config$about)
      if (file.exists(about_file)) {
        config$about <- about_file
      } else {
        stop_nice("About file ", about_file, " not found.")
      }
    }

    # Global default advanced settings ----
    config$default_measures_outliers <-
      match.arg(
        raw_config$measures_outliers,
        c("No", "5/95 percentiles", "IQR")
      )
    config$default_expression_outliers <-
      match.arg(
        raw_config$expression_outliers,
        c("No", "5/95 percentiles", "IQR")
      )
    config$default_correlation_method <-
      match.arg(
        raw_config$correlation_method,
        c("pearson", "spearman", "kendall")
      )
    config$default_fit_method <-
      match.arg(
        raw_config$fit_method,
        c("none", "linear", "quadratic", "cubic")
      )

    # Global other settings ----
    config$adjust_method <- raw_config$adjust_method %||% "q.value"

    # Validate config data section ----
    if (is.null(raw_config$data)) {
      stop_nice("'data' section missing in configuration file")
    }
    if (is.null(raw_config$data$measures_data) ||
      is.null(raw_config$data$expression_matrix)) {
      stop_nice(
          "Data section in configuration file must include ",
          "measures_data and expression_matrix files."
          )
    }

    # Load data section ----
    loaded_data <- lapply(names(raw_config$data), function(file_type) {
      message("Loading file: ", file_type, "\n", appendLF = FALSE)
      read_file(raw_config$data[[file_type]], file_type, data_folder, nthreads)
    })
    names(loaded_data) <- names(raw_config$data)

    # Set global settings for sample and subject column ----
    if (is.null(raw_config$sample_categories)) {
      stop_nice("sample_categories property missing in configuration file")
    }
    config$sample_categories <- raw_config$sample_categories
    config$sample_variable <-
      raw_config$sample_variable %||% "Sample_ID"
    config$subject_variable <-
      raw_config$subject_variable %||% "Subject_ID"
    config$timesep <-
      raw_config$timesep %||% "_"

    if (is.null(raw_config$data$sample_lookup)) {
      sample_categories_names <-
        vapply(config$sample_categories, function(x) x$name, character(1))
      loaded_data$sample_lookup <- create_lookup(
        loaded_data$measures_data,
        sample_categories_names,
        config$sample_variable,
        config$subject_variable
      )
      loaded_data$measures_data <-
        remove_duplicate_subjects(
          loaded_data$measures_data,
          sample_categories_names,
          config$sample_variable
        )
    }

    # Validate samples and subjects ----
    # Check if number of samples and subjects match across measures_data
    # And matrices
    # There should be no samples w/o a subject and no subjects without samples
    validateData(loaded_data,
      sample_variable = config$sample_variable,
      subject_variable = config$subject_variable
    )

    config$data <- loaded_data

    # Load modules ----
    if (!is.null(test_module)) {
      # Check single module configuration
      message("Module test mode")
      if (test_module %in% available_modules) {
        if (!is.null(raw_config[[test_module]])) {
          mod_conf <- do.call(
            paste0(test_module, "_config"),
            list(
              config = raw_config[[test_module]],
              data_folder = data_folder
            )
          )
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
          if (!is.null(raw_config[[module_name]])) {
            do.call(
              paste0(module_name, "_config"),
              list(
                config = raw_config[[module_name]],
                data_folder = data_folder
              )
            )
          }
        })
      names(loaded_modules) <- available_modules

      if (!is.null(custom_modules)) {
        extra_loaded_modules <-
          lapply(custom_modules, function(module_name) {
            if (!is.null(raw_config[[module_name]])) {
              checkModuleExists(module_name)
              do.call(
                paste0(module_name, "_config"),
                list(
                  config = raw_config[[module_name]],
                  data_folder = data_folder
                )
              )
            }
          })
        names(extra_loaded_modules) <- custom_modules
        loaded_modules <- c(loaded_modules, extra_loaded_modules)
      }
    }
    config$modules <- loaded_modules
    config
  }

checkModuleExists <- function(module_name) {
  config_name <- paste0(module_name, "_config")
  ui_name <- paste("mod", module_name, "ui", sep = "_")
  server_name <- paste("mod", module_name, "server", sep = "_")
  function_names <- c(config_name, ui_name, server_name)
  for (name in function_names) {
    if (!exists(name, mode = "function")) {
      stop_nice(
          "Problem when loading modules: ",
          name,
          " function is not defined. ",
          "Please ensure custom module functions are defined before run_app."
        )
    }
  }
}

validateData <-
  function(datafiles,
           sample_variable = "Sample_ID",
           subject_variable = "Subject_ID") {
    expression_matrix <- datafiles[["expression_matrix"]]
    measures_data <- datafiles[["measures_data"]]
    sample_lookup <- datafiles[["sample_lookup"]]

    measures_subjects <- measures_data[[subject_variable]]
    lookup_subjects <- unique(sample_lookup[[subject_variable]])
    lookup_samples <- sample_lookup[[sample_variable]]
    expression_samples <- colnames(expression_matrix)

    if (mode(measures_data[[subject_variable]]) !=
        mode(sample_lookup[[subject_variable]]))
      stop_nice("Subject column types in measures_data and lookup",
        " table are different")

    error <- FALSE
    if (!isTRUE(all.equal(sort(measures_subjects), sort(lookup_subjects)))) {
      message(
        "ERROR: Subjects in measures_data and lookup tables do not match."
        )
      nclin <- setdiff(measures_subjects, lookup_subjects)
      if (length(nclin) > 0) {
        message("Subjects in measures_data table not found in lookup:")
        message(nclin)
      }
      nlookup <- setdiff(lookup_subjects, measures_subjects)
      if (length(nlookup) > 0) {
        message("Subjects in lookup table not found in measures_data:")
        message(nlookup)
      }
      error <- TRUE
    }
    if (!isTRUE(all.equal(sort(expression_samples), sort(lookup_samples)))) {
      message(
        "ERROR: Samples in expression matrix and lookup tables do no match."
      )
      nexp <- setdiff(expression_samples, lookup_samples)
      if (length(nexp) > 0) {
        message("Samples in expression matrix not found in lookup table:")
        message(nexp)
      }
      nlookup <- setdiff(lookup_samples, expression_samples)
      if (length(nlookup) > 0) {
        message("Samples in lookup table not found in expression matrix:")
        message(nlookup)
      }
      error <- TRUE
    }
    if (error) {
      stop_nice("Problem(s) found in data files.",
        "See printed message(s) for details")
    }
  }

#' User-friendly read_file
#'
#' @param filename filename
#' @param filetype "expression_matrix" for matrix type
#' @param data_folder Optional directory prefix for file
#'
#' @return parsed file
#' @noRd
read_file <-
  function(filename, filetype = "", data_folder = "", nthreads = 1L) {
  fext <- file_ext(filename)
  filename <- file_path(data_folder, filename)
  if (!file.exists(filename)) {
    stop_nice("File ", filename, " not found.")
  }
  tryCatch({
      if (fext == "rds") {
        df <- readRDS(filename)
        if (filetype == "expression_matrix" & !is.matrix(df)) {
          stop_nice(
            "For expression_matrix file, object must be stored as matrix"
          )
        }
        df
      } else {
        # Non-serialized data
        if (filetype == "expression_matrix") {
          df <-
            data.table::fread(filename, data.table = FALSE, nThread = nthreads)
          rownames(df) <- df[, 1]
          df <- df[, -1]
          as.matrix(df)
        } else if (filetype == "edge_list") {
          col_names <- c("source", "target", "weight")
          delim <- ifelse(fext == "csv", ",", "\t")
          data <- data.table::fread(filename,
            sep = delim,
            data.table = FALSE,
            col.names = col_names,
            nThread = nthreads
          )
          data$title <- paste(data$weight)
          data
        } else {
          delim <- ifelse(fext == "csv", ",", "\t")
          data.table::fread(filename,
            sep = delim,
            data.table = FALSE,
            nThread = nthreads
          )
        }
      }
    },
    error = function(errormsg) {
      stop(errormsg)
    },
    warning = function(warningmsg) {
      stop(warningmsg)
    }
  )
}

#' Load differential expression models
#'
#' This function takes a tabular file name as input, loads it and then load
#' each model. The tabular file must have a File column. Other columns will be
#' used to organise tables and populate the radio buttons in the DEG modules.
#'
#' @param models_file file that categories and locate each model
#' @param data_folder optional data folder to find file
#' @param pvalue_max optional maximum p value for significance. Default is 0.05
#' @param padj_max optional maximum adjusted p value for significance.
#' Default is 0.05
#' @param pvalue_col optional column for p-value. Default is P.value
#' @param padj_col optional column for adjusted p-value. Default is q.value
#' @param nthreads optional number of threads to speed up reading files
#'
#' @return a data frame with columns from the model table and a Data column
#' containing data frames
#'
#' @noRd
loadModels <- function(models_file,
                       data_folder = "",
                       pvalue_max = 0.05,
                       padj_max = 0.05,
                       pvalue_col = "P.value",
                       padj_col = "q.value",
                       nthreads = 1L) {
  fext <- file_ext(models_file)
  if (fext == "rds") {
    models_table <- readRDS(file_path(data_folder, models_file))
  } else {
    delim <- ifelse(fext == "csv", ",", "\t")
    models_table <- data.table::fread(file_path(data_folder, models_file),
      sep = delim,
      data.table = FALSE,
      nThread = nthreads
    )
  }
  if (ncol(models_table) < 2) {
    stop_nice(
      "models_table loaded incorrectly.",
      "Use comma separator in .csv files or tab with any other file extension"
    )
  }
  if (!"File" %in% colnames(models_table)) {
    stop_nice("'File' column missing from models_table.")
  }

  # Go through list of files and load them
  models_table$Data <- lapply(models_table$File, function(file_name) {
    file_name <- file_path(data_folder, "models", file_name)
    if (!file.exists(file_name)) {
      stop_nice(
        "Model file ",
        file_name,
        " from degModules configuration not found."
      )
    }
    fext <- file_ext(file_name)
    delim <- ifelse(fext == "csv", ",", "\t")
    model <- data.table::fread(file_name,
      sep = delim,
      data.table = FALSE,
      nThread = nthreads
    )
    valid_symbol_cols <-
      c("Protein", "Gene", "GeneSymbol", "Symbol", "symbol", "Gene_ID")
    if (length(intersect(valid_symbol_cols, colnames(model))) == 0) {
      stop_nice(
        "Model file",
        file_name,
        "does not have a valid column for gene symbols. Valid columns include",
        valid_symbol_cols)
    }
    model
  })

  models_table$ModelFileType <- lapply(models_table$Data, function(x) {
    if ("P.Value" %in% colnames(x)) {
      "limma"
    } else if ("log2FoldChange" %in% colnames(x)) {
      "deseq"
    } else if ("PValue" %in% colnames(x)) {
      "edger"
    } else {
      "limma"
    }
  })

  models_table$P <-
    vapply(models_table$Data,
           function(x) nrow(x[x[[pvalue_col]] <= pvalue_max, ]),
           integer(1))
  models_table$P_adj <-
    vapply(models_table$Data,
           function(x) nrow(x[x[[padj_col]] <= padj_max, ]),
           integer(1))

  models_table
}

#' Validate list of advanced settings
#'
#' @param config list of advanced settings from configuration file
#' @param module_title optional module title for nice error message
#'
#' @noRd
validateAdvancedSettings <- function(config, module_title = "") {
  if (is.null(names(config)))
    stop_nice("advanced_settings must be a list of options")
  valid_settings <-
    c(
      "measures_outliers",
      "expression_outliers",
      "correlation_method",
      "fit_method"
    )
  diff_advanced <- setdiff(names(config), valid_settings)
  if (length(diff_advanced) > 0) {
    stop_nice(
      module_title, ": invalid advanced setting: \n\t",
      diff_advanced,
      "\nMust be one of\n\t", paste0(valid_settings, collapse = "\n\t")
    )
  }
}

# from tools::file_ext
file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' Create a lookup table from measures_data and columns
#'
#' @param measures_data table with duplicate rows for a subject
#' @param sample_categories metadata for samples that will be used to populate
#' lookup table
#' @param sample_variable variable that identifies a sample
#' @param subject_variable variable that identifies a subject
#'
#' @return a data frame
#'
#' @noRd
create_lookup <- function(measures_data, sample_categories, sample_variable,
                          subject_variable) {
    samples <- measures_data[[sample_variable]]
    subjects <- measures_data[[subject_variable]]
    lookup <- data.frame(samples, subjects)
    colnames(lookup) <- c(sample_variable, subject_variable)
    categories_data <- lapply(sample_categories, function(x) measures_data[[x]])
    names(categories_data) <- sample_categories
    lookup <- cbind(lookup, categories_data)
    lookup
  }

#' Removes duplicate rows for subjects
#'
#' This function requires that only variables in sample_cat_names and the
#' sample_variable have unique values for a subject. If there are additional
#' columns with unique value per sample, the rows will be kept and the portal
#' may not work as expected
#'
#' @param measures_data data frame with observations for subjects and duplicated
#'  rows
#' @param sample_cat_names names of variables used in lookup
#' @param sample_variable variable that identifies samples
#'
#' @return a data frame with single row per subject
#'
#' @noRd
remove_duplicate_subjects <-
  function(measures_data, sample_cat_names, sample_variable) {
    measures_data[, c(sample_cat_names, sample_variable)] <- list(NULL)
    unique(measures_data)
  }
