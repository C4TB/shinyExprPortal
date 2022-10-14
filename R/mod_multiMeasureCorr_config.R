multiMeasureCorr_config <- function(config, data_folder = "") {
  message("Checking multiMeasureCorr configuration")

  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "multiMeasureCorr")
  }

  if (is.null(config$heatmap_variables)) {
    stop_nice("multiMeasureCorr:",
      "named 'scatterplot_variables' list is missing"
    )
  }
  
  if (!is.null(config$custom_heatmap_scheme)) {
    if (!is.character(config$custom_heatmap_scheme)) {
      stop_nice("multiMeasureCorr",
                "'custom_heatmap_scheme' must be a valid Vega palette.",
                " Please check https://vega.github.io/vega/docs/schemes")
    }
  }
  config$custom_heatmap_scheme <-
    config$custom_heatmap_scheme %||% "redblue"

  config
}
