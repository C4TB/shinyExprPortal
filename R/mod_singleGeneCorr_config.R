singleGeneCorr_config <- function(config, ...) {
  message("Checking singleGeneCorr configuration")

  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleGeneCorr")
  }

  if (is.null(config$tabs)) {
    stop_nice("singleGeneCorr: 'tabs' definitions are missing")
  }

  config
}
