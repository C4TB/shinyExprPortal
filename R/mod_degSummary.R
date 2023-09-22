# module UI Function
mod_degSummary_ui <-
  function(module_name, config, module_config, parent_config = NULL) {
    degSummary_tab(
      module_config$title,
      module_config$description,
      module_name
    )
}

degSummary_tab <- function(title = NULL, description = NULL, id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "Summary table",
    value = "degSummary",
    tags$h5(description %||% "Summary of differential expression models hits"),
    htmlOutput(ns("summary_table"))
  )
}

mod_degSummary_server <-
  function(module_name, config, module_config, parent_config = NULL) {

  moduleServer(module_name, function(input, output, session) {

    models <- parent_config$models %||% module_config$models
    partition <- module_config$partition_variable
    if (!is.null(partition))
    partition_values <- unique(models[[partition]])

    output$summary_table <- renderText({

      # Prepare table headers
      # Spec defines how many columns are shared across partition values
      if (!is.null(partition)) {
        header_spec <- stats::setNames(
          rep(2, length(partition_values)),
          partition_values
        )
        # Cols is required because we need to sort after pivoting
        header_cols <- sort(as.vector(outer(partition_values,
          c("P", "P_adj"), paste,
          sep = "#"
        )))
      } else {
        header_spec <- NULL
        header_cols <- c("#P", "#P_adj")
      }

      # Get only the columns that define models (e.g. Response, Tissue, Time)
      exc_cols <- c("P", "P_adj", "File", "Data", "ModelFileType", partition)
      model_cols <- colnames(models[, !colnames(models) %in% exc_cols])

      # We don't need the actual data or file names here
      models_only <- models %>%
        dplyr::select(-all_of(c("Data","File","ModelFileType")))
      # By default pivot_wider will order by the values_from
      # We use relocate to rearrange only the pivoted columns
      if (!is.null(header_spec)) {
        model_wide <-
          models_only %>%
          pivot_wider(
            names_from = all_of(partition),
            values_from = c("P", "P_adj"),
            names_glue = paste0("{", partition, "}#{.value}")
          ) %>%
          relocate(any_of(header_cols), .after = last_col())
      } else model_wide <- models_only
      knitr_table <-
        add_knitr_table(model_wide, model_cols, header_cols, header_spec)
      knitr_table
    })
  })
}
