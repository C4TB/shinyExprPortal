#' module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_degSummary_ui <- function(id, appdata) {
  degSummary_tab(id)
}

degSummary_tab <- function(id = NULL) {
  ns <- NS(id)
  tabPanel(title = "DEG Summary", value = "degSummary",
           htmlOutput(ns("summary_table"))
  )
           # splitLayout(
           #   verticalLayout(
           #     wellPanel(
           #       radioButtons(
           #         ns("model_category"),
           #         label = "Select category:",
           #         choices = c("a", "b")
           #       )
           #     )
           #   ),
           #   verticalLayout(
           #     #OUTPUTS
           #     plotOutput(ns("degHeatmap"))
           #   ),
           #   cellWidths = c("20%", "80%"),
           #   cellArgs = list(style = "white-space: normal;")
           # )
}

mod_degSummary_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session) {
    
    output$summary_table <- renderText({
      
      models <- appdata$modules$degModules$models
      
      partition <-  appdata$modules$degModules$modules$degSummary$partition
      partition_values <- unique(models[[partition]])
      # Prepare table headers
      # Spec defines how many columns are shared across partition values
      header_spec <- setNames(rep(2, length(partition_values)),
                              partition_values)
      # Cols is required because we need to sort after pivoting
      header_cols <- sort(as.vector(outer(partition_values,
                           c("pSignif", "qSignif"), paste, sep = "_")))
      # Get only the columns that define models (e.g. Response, Tissue, Time)
      model_cols <- colnames(models[, !colnames(models) %in%
                        c("pSignif", "qSignif", "File", "Data", partition)])
      # We don't need the actual data or file names here
      models_only <- models %>% dplyr::select(-Data, -File)
      
      # By default pivot_wider will order by the values_from
      # We use relocate to rearrange only the pivoted columns
      model_wide <- models_only %>%
                    pivot_wider(names_from = all_of(partition),
                        values_from = c(pSignif, qSignif),
                        names_glue = paste0("{", partition, "}_{.value}")) %>%
                    relocate(any_of(header_cols), .after = last_col())
      model_wide %>% 
        knitr::kable(align = "r",
                     "html",
                     col.names = c(model_cols,
                                   gsub(".*_(.*)", "\\1", header_cols))
                       ) %>%
        kableExtra::kable_styling(full_width = F,
                                  position = "left",
                                  font_size = 12) %>%
        # kableExtra::group_rows(index = setNames(rle(model_wide$Model)[[1]], 
        #                                       rle(model_wide$Model)[[2]])) %>%
        # kableExtra::group_rows(indent = T,
        #                        index = setNames(rle(model_wide$Tissue)[[1]], 
        #                                         rle(model_wide$Tissue)[[2]])) %>%
        kableExtra::collapse_rows(valign = "top",
                                  columns = seq_along(model_cols)) %>%
        kableExtra::add_header_above(c(" " = length(model_cols),
                                       header_spec))

    })
    
  })
}
