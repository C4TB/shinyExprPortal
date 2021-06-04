# module UI Function
mod_degSummary_ui <- function(module_name, appdata, global, module_config) {
  degSummary_tab(module_name)
}

degSummary_tab <- function(id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = "Summary table",
    value = "degSummary",
    tags$h5("Summary of differential expression models"),
    htmlOutput(ns("summary_table"))
  )
}

mod_degSummary_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session) {
    
    models <- appdata$models
    partition <-  module_config$partition_variable
    partition_values <- unique(models[[partition]])
    
    output$summary_table <- renderText({

      # Prepare table headers
      # Spec defines how many columns are shared across partition values
      header_spec <- stats::setNames(rep(2, length(partition_values)),
                              partition_values)
      # Cols is required because we need to sort after pivoting
      header_cols <- sort(as.vector(outer(partition_values,
                           c("pSignif", "qSignif"), paste, sep = "_")))
      # Get only the columns that define models (e.g. Response, Tissue, Time)
      model_cols <- colnames(models[, !colnames(models) %in%
                        c("pSignif", "qSignif", "File", "Data", partition)])
      # We don't need the actual data or file names here
      models_only <- models %>% 
        dplyr::select(-.data[["Data"]], -.data[["File"]])
      
      # By default pivot_wider will order by the values_from
      # We use relocate to rearrange only the pivoted columns
      model_wide <- 
        models_only %>%
          pivot_wider(names_from = all_of(partition),
                      values_from = c("pSignif", "qSignif"),
                      names_glue = paste0("{", partition, "}_{.value}")) %>%
          relocate(any_of(header_cols), .after = last_col())
      
      model_wide %>% 
        knitr::kable(
          align = "r",
          format ="html",
          escape = "F",
          col.names = c(model_cols, gsub(".*_(.*)", "\\1", header_cols))) %>%
        kableExtra::kable_styling(
          full_width = F,
          position = "left",
          font_size = 12) %>%
        kableExtra::collapse_rows(
          valign = "top",
          columns = seq_along(model_cols[-length(model_cols)])) %>%
        kableExtra::add_header_above(
          header = c(" " = length(model_cols), header_spec))

    })
    
  })
}
