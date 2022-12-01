singleGeneCorr_config <- function(config, ...) {
  message("Checking singleGeneCorr configuration")

  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "singleGeneCorr")
  }

  if (is.null(config$tabs)) {
    stop_nice("singleGeneCorr: 'tabs' definitions are missing")
  }
  
  if (!is.null(config$custom_point_colors)) {
    lv <- which(
      vapply(config$custom_point_colors,
           function(x) (is.character(x) || is.list(x) || is.vector(x)) == FALSE,
      logical(1))
    )
    if (length(lv) >= 1) {
      invalid_colors <-
        paste(names(config$custom_point_colors)[lv], collapse=",")
      stop_nice("singleGeneCorr: colors for ", invalid_colors, " should be in ",
                "list format")
    }
  }

  config
}
