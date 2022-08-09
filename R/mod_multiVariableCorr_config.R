multiVariableCorr_config <- function(config, data_folder = "") {
  message("Checking multiVariableCorr configuration")

  if (!is.null(config$advanced)) {
    validateAdvancedSettings(config$advanced, "multiVariableCorr")
  }

  if (is.null(config$heatmap_variables)) {
    stop_nice("multiVariableCorr:",
      "named 'scatterplot_variables' list is missing"
    )
  }
  
  if (!is.null(config$custom_heatmap_scheme)) {
    if (!is.character(config$custom_heatmap_scheme)) {
      stop_nice("multiVariableCorr",
                "'custom_heatmap_scheme' must be a valid Vega palette.",
                " Please check https://vega.github.io/vega/docs/schemes")
    }
  }
  config$custom_heatmap_scheme <-
    config$custom_heatmap_scheme %||% "redblue"

  config
}
