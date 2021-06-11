# module UI Function
mod_compareTrajGroups_ui <-
  function(module_name,
           appdata,
           global,
           module_config) {
    compareTrajGroups_tab(
      sampleClassInputs(
        global$sample_classes,
        module_name,
        module_config$subset_classes
      ),
      varsSelectInput(module_config$compare_variables, 
                      module_name,
                      initEmpty = FALSE),
      geneSelectInput(gene_list = NULL, module_name),
      module_config$title,
      module_config$advanced,
      module_name
    )
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
    subject_var <- global$subject_variable
    sample_var <- global$sample_variable
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
    
    expression_from_lookup <- eventReactive(selected_lookup(), {
      sel_lookup <- selected_lookup()
      expression_matrix[, sel_lookup[[sample_var]]]
    })
    
    clinical_from_lookup <- eventReactive(selected_lookup(), {
      sel_lookup <- selected_lookup()
      selectFromLookup(clinical, sel_lookup,
                       matching_col = subject_var)
    })
    
    output$trajplot <- renderPlot({
      
      req(input$selected_gene)
      
      sel_lookup <- selected_lookup()
      subset_expression <- expression_from_lookup()
      selected_clinical <- clinical_from_lookup()
      fit_method <- input$fit_method %||% "linear"
      
      # Return matching selected_variable
      compare_col_id <-
        grep(paste0("(", input$selected_variable, ")\\_.*"),
             colnames(selected_clinical))
      compare_col_vars <- 
        colnames(selected_clinical)[compare_col_id]
      subset_clinical <- 
        selected_clinical[, c(subject_var, compare_col_vars)]
      # Convert to long and separate unique variable from suffix
      subset_long <- pivot_longer(subset_clinical,
                                  -.data[[subject_var]],
                                  names_to = c(".value", trajectory_class),
                                  names_sep= "_")
      combined <- left_join(sel_lookup,
                            subset_long,
                            by = c(subject_var, trajectory_class))
      selected_expression <- subset_expression[input$selected_gene,
                                                 combined[[sample_var]]]
      validate(
        need(all(not_na(selected_expression)) &
               (length(selected_expression) > 0),
         "Transcript not found in subset or subset combination does not exist."
      ))
      combined$expression <- selected_expression
      df <-
        combined[, c(subject_var, trajectory_class, sidebyside_class,
                     input$selected_variable, "expression")]
      
      trajplot <- plotTrajGroups(
        df = df[order(df[[subject_var]], df[[trajectory_class]]), ],
        selected_variable = input$selected_variable,
        selected_gene = input$selected_gene,
        traj_var = trajectory_class,
        facet_var = sidebyside_class,
        pal = traj_palette
      )
      
      if (input$showtraj == "Yes") {
        trajplot <- trajplot + 
          geom_path(aes(color = .data[[trajectory_class]],
                        group = .data[[subject_var]]),
                    alpha = 0.5,
                    arrow = arrow(angle = 15, length = unit(0.1, "inches"),
                                  type = "closed"))
      }
      trajplot + ggAddFit(fit_method)
    }, bg = "transparent")
    
  })
}
