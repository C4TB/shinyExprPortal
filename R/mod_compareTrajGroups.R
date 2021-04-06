#' module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_compareTrajGroups_ui <- function(id, appdata) {
  module_config <- appdata$modules$compareTrajGroups
  compareTrajGroups_tab(
    sampleClassInputs(
      appdata$config$sample_classes,
      id,
      module_config$subset_classes
    ),
    geneSelectInput(rownames(appdata$data$expression), id),
    id)
}

compareTrajGroups_tab <- function(sample_select, gene_select, id = NULL) {
  ns <- NS(id)
  tabPanel(title = "Compare Groups", value = "compareTrajGroups",
           splitLayout(
             verticalLayout(
               wellPanel(
                 sample_select,
                 gene_select
               )
             ),
             verticalLayout(
               #OUTPUTS
               plotOutput(ns("trajplot"))
             ),
             cellWidths = c("20%", "80%"),
             cellArgs = list(style = "white-space: normal;")
           )
  )
}

mod_compareTrajGroups_server <- function(module_name, appdata) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$data$clinical
    expression_matrix <- appdata$data$expression_matrix
    sample_lookup <- appdata$data$sample_lookup
    subject_col <- appdata$config$subject_col
    sample_col <- appdata$config$sample_col
    sample_classes <- appdata$config$sample_classes
    
    module_config <- appdata$modules$compareTrajGroups
    subset_classes <- module_config$subset_classes
    trajectory_class <- module_config$trajectory_class
    compare_col <- module_config$compare_col
    sidebyside_class <- module_config$sidebyside_class
    # Select only the sample classes required by this view
    # For each class, get the value selected by the user and filter the lookup
    selected_lookup <- reactive({
      subset_values <- getSubsetSampleClasses(subset_classes,
                                              sample_classes,
                                              input)
      selectMatchingValues(sample_lookup, subset_values)
    })
    
    subset_clinical <- reactive({
      selectFromLookup(clinical, selected_lookup(),
                       matching_col = subject_col)
    })
    
    output$trajplot <- renderPlot({
      
      req(input$selected_gene)
      
      sel_lookup <- selected_lookup()
      selected_expression <- expression_matrix[, sel_lookup[[sample_col]]]
      clinical <- subset_clinical()
      
      compare_col_id <-
        grep(paste0("(", compare_col, ")\\_.*"), colnames(clinical))
      compare_col_vars <- 
        colnames(clinical)[compare_col_id]
      subset_clinical <- 
        clinical[, c(subject_col, compare_col_vars)]
      subset_long <- pivot_longer(subset_clinical,
                                  -.data[[subject_col]],
                                  names_to = c(".value", trajectory_class),
                                  names_sep= "_")
      combined <- dplyr::left_join(sel_lookup, subset_long, by = c("Subject_ID", "Time"))
      combined$expression <- selected_expression[input$selected_gene, combined[[sample_col]]]
      df <- combined[, c(subject_col, trajectory_class, sidebyside_class, compare_col, "expression")]
      # df$Time_seq <- 
      #   as.numeric(
      #     as.character(
      #       factor(df$Time,
      #          levels = unique(df$Time),
      #          labels = seq_along(unique(df$Time))
      #       )
      #     )
      #   )
      
      # lerps <- function(v) {
      #   tibble::tibble(
      #     new_exp = approx(v$Time_seq, log(v$expression),rule = 2)$y,
      #     new_pasi = approx(v$Time_seq, log(v$PASI),rule = 2)$y,
      #   )
      # }
      #color = .data[[trajectory_class]],
      ggplot(df[order(df$Subject_ID, df$Time), ], aes(x = .data[[compare_col]],
                     y = log(.data$expression))) +
        geom_point(aes(fill = .data[[trajectory_class]]),
                   colour="black",pch=21, size = 2) +
        geom_path(aes(color = .data[[trajectory_class]],
                      group = .data[[subject_col]]),
                  alpha = 0.5,
                  #linetype = 2,
                  arrow = arrow(angle = 15, length = unit(0.1, "inches"),
                                type = "closed")) +
        scale_fill_viridis_d() +
        scale_colour_viridis_d() + 
        facet_wrap(stats::as.formula(paste("~", sidebyside_class)), scales = "fixed")
    })
    
    
    
    
  })
}
