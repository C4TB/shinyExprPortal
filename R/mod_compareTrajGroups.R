# module UI Function
mod_compareTrajGroups_ui <- function(module_name, appdata, global, module_config) {
  compareTrajGroups_tab(
    sampleClassInputs(
      global$sample_classes,
      module_name,
      module_config$subset_classes
    ),
    varsSelectInput(module_config$compare_col, module_name, initEmpty = FALSE),
    geneSelectInput(gene_list = NULL, module_name),
    module_config$title,
    module_config$advanced,
    module_name)
}

compareTrajGroups_tab <-
  function(sample_select,
           vars_select,
           gene_select,
           title = NULL,
           advanced = NULL,
           id = NULL
  ) {
  ns <- NS(id)
  tabPanel(
    title = "Compare trajectories",
    value = "compareTrajGroups",
    tags$h5(title %||% "Comparison between groups"),
           splitLayout(
             verticalLayout(
               wellPanel(
                 gene_select,
                 vars_select,
                 sample_select,
                 advanced_settings_inputs(advanced, id),
                 radioButtons(ns("showtraj"),
                              "Show trajectories?",
                              choices = c("No", "Yes"),
                              selected = c("No"))
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
mod_compareTrajGroups_server <- function(module_name, appdata, global,
                                         module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression_matrix
    sample_lookup <- appdata$sample_lookup
    subject_col <- global$subject_col
    sample_col <- global$sample_col
    sample_classes <- global$sample_classes
    
    subset_classes <- module_config$subset_classes
    trajectory_class <- module_config$trajectory_class
    sidebyside_class <- module_config$sidebyside_class
    traj_palette <- module_config$palette %||% NULL
    
    # Load genes server side
    updateSelectizeInput(session,
                         "selected_gene",
                         choices = rownames(expression_matrix),
                         selected = "",
                         server = TRUE)
    
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
      subset_expression <- expression_matrix[, sel_lookup[[sample_col]]]
      clinical <- subset_clinical()
      fit_method <- input$fit_method %||% "linear"
      
      # Return variables + time that match the selected variable
      compare_col_id <-
        grep(paste0("(", input$selected_variable, ")\\_.*"), colnames(clinical))
      compare_col_vars <- 
        colnames(clinical)[compare_col_id]
      subset_clinical <- 
        clinical[, c(subject_col, compare_col_vars)]
      subset_long <- pivot_longer(subset_clinical,
                                  -.data[[subject_col]],
                                  names_to = c(".value", trajectory_class),
                                  names_sep= "_")
      combined <- left_join(sel_lookup,
                            subset_long,
                            by = c("Subject_ID", "Time"))
      selected_expression <- subset_expression[input$selected_gene,
                                                 combined[[sample_col]]]
      validate(
        need(all(not_na(selected_expression)) & (length(selected_expression) > 0),
               "Transcript not found in subset or subset combination does not exist."
      ))
      combined$expression <- selected_expression
      df <- combined[, c(subject_col, trajectory_class,
                         sidebyside_class, input$selected_variable, "expression")]
      
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
      trajplot <- plotTrajGroups(
        df[order(df[[subject_col]], df[[trajectory_class]]), ],
        input$selected_variable,
        input$selected_gene,
        trajectory_class,
        sidebyside_class,
        traj_palette
      )
      
      if (input$showtraj == "Yes") {
        trajplot <- trajplot + 
          geom_path(aes(color = .data[[trajectory_class]],
                        group = .data[[subject_col]]),
                    alpha = 0.5,
                    arrow = arrow(angle = 15, length = unit(0.1, "inches"),
                                  type = "closed"))
      }
      trajplot + ggAddFit(fit_method)
    })
    
  })
}
