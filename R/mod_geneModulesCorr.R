# geneModulesCorr UI Function
mod_geneModulesCorr_ui <- function(module_name, config, module_config) {
  geneModulesCorr_tab(
    sample_select = sampleClassInputs(
      config$sample_classes,
      module_name,
      module_config$subset_classes
    ),
    sources_names = lapply(module_config$modules_data,
                           function(x) x$name),
    clinical_variables = names(module_config$scatterplot_variables),
    module_config$title,
    module_config$description,
    module_name
  )
}
#' Gene modules tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param sources_names list of module sources
#' @param clinical_variables list of set of clinical variables for scatterplots
#' @param title optional module title
#' @param description optional module description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
#'
geneModulesCorr_tab <- function(sample_select,
                                sources_names,
                                clinical_variables,
                                title = NULL,
                                description = NULL,
                                id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "Gene Modules",
    value = "geneModulesCorr",
    tags$h5(description %||% 
              "Gene modules profiles and correlation with clinical variables"),
    splitLayout(
      verticalLayout(
        wellPanel(
          sample_select,
          radioButtons(
            ns("source"),
            label = "Modules source:",
            choiceValues = seq(sources_names),
            choiceNames = sources_names,
            selected = 1
          ),
          selectizeInput(
            ns("selected_module"),
            label = with_red_star("Select a module to inspect:"),
            choices = NULL,
            options = list(dropdownParent = "body")
          )
        ),
        wellPanel(
          selectizeInput(
            ns("selected_clinical"),
            label = with_red_star("Select a clinical variable to compare across"),
            choices = clinical_variables,
            options = list(
              dropdownParent = "body",
              onInitialize = I('function(){this.setValue("");}')
            )
          )
        )
      ),
      verticalLayout(
        splitLayout(
          plotly::plotlyOutput(ns("overview"), height = 300, width = 450),
          plotOutput(ns("profile_plot"), height = 200, width = "99%")
        ),
        hr(),
        conditionalPanel(condition = "input.selected_module != ''",
                         plotOutput(ns("scatterplot")))
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = 'white-space: normal;')
    )
  )
}
#' geneModulesCorr Server Function
#' @noRd 
mod_geneModulesCorr_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    sample_classes <- config$sample_classes
    
    subset_classes <- module_config$subset_classes
    across_class <- module_config$across_class
    modules_data <- module_config$modules_data
    scatterplot_variables <- module_config$scatterplot_variables
    
    modules_computed <- reactiveValues(medians = NULL, medians_across = NULL)
    
    # Select only the sample classes required by this view
    # For each class, get the value selected by the user and filter the lookup
    selected_lookup <- reactive({
      subset_values <- getSubsetSampleClasses(subset_classes,
                                              sample_classes,
                                              input)
      selectMatchingValues(sample_lookup, subset_values)
    })
    
    observeEvent(input$source, {
      selected_source_data <- modules_data[[as.numeric(input$source)]][["data"]]
      #Update module selection
      mod_choices <-
        stats::setNames(
          as.list(selected_source_data$descriptions$Module),
          paste(
            selected_source_data$descriptions$Module,
            selected_source_data$descriptions$Title,
            sep = ": "
          )
        )
      updateSelectizeInput(
        session,
        "selected_module",
        selected = "",
        choices = mod_choices ,
        server = TRUE
      )
    })
    
    # Calculate the medians per module and overview when:
    # - user changed the source of modules
    # - or user changed subset_classes
    observeEvent(c(input$source, selected_lookup()), {

      req(input$source)
            
      selected_source_data <- modules_data[[as.numeric(input$source)]][["data"]]
      module_data <- selected_source_data$modules
      
      sel_lookup <- selected_lookup()
      selected_expression <- expression_matrix[, sel_lookup[[sample_var]]]
      
      # Compute medians per module
      medians_per_module <- mediansPerModule(module_data, selected_expression)
      modules_computed$medians <- medians_per_module
      
      # Compute summary of medians across a selected class
      modules_computed$medians_across <-
        computeModulesSummary(medians_per_module, sel_lookup, across_class)
    })
    
    # Annotate overview after a module is selected in dropdown
    observeEvent(input$selected_module,{
      req(input$selected_module)

      medians_across <- modules_computed$medians_across
      annots <- list()
      
      selected_median <-
        medians_across[medians_across$Modules == input$selected_module, ]
      annots <- apply(selected_median, 1, function(x) {
        list(
          text = input$selected_module,
          x = as.numeric(x["jittered"]),
          y = as.numeric(x["Median_Expression"]),
          xref = "x",
          yref = "y"
        )
      })
      plotly::plotlyProxy("overview", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(annotations = annots))
    })
    
    # Reactive computation of a selected_module profile
    selected_module_profile <- reactive({
      req(input$selected_module)
      
      # Isolate reads ensure that the profile only updates when selected_module
      isolate({
        module_medians <- modules_computed$medians
        selected_source_data <-
          modules_data[[as.numeric(input$source)]][["data"]]
      })
      
      computeModuleProfile(module_medians, input$selected_module,
                           selected_lookup(), sample_var, across_class )
      
    })
    
    # Render the overview when median_across changes
    output$overview <- plotly::renderPlotly({
      plotModulesOverview(modules_computed$medians_across, across_class)
    })
    
    # Render profile plot
    # Depends mostly on the selected_module_profile reactive, but
    # We also need the genes information to assemble the profile title
    output$profile_plot <- renderPlot({
   
      isolate({
        selected_source_data <-
          modules_data[[as.numeric(input$source)]][["data"]]
      })
      
      module_descriptions <- selected_source_data$descriptions
      modules_data <- selected_source_data$modules
      # Retrieve list of genes of module to get number of genes for the title
      genes <- 
        modules_data[which(modules_data$modules == input$selected_module),
                     "genes"]
      # Retrive module description to get the detailed title of the module
      desc_lv <- which(module_descriptions$Module == input$selected_module)
      description <- module_descriptions[desc_lv, ]
      profile_title <-
        paste0(
          paste(description$Module, description$Title, sep = ": "),
          " - number of genes:",
          length(genes)
        )
      
      plotModuleProfile(
        selected_module_profile(),
        expression_col = "Expression",
        sample_col = sample_var,
        across_class = across_class,
        plot_title = profile_title
      )
    })
    
    ## Render scatterplots based on group of clinical variables
    ## Triggered by user changing the clinical variable
    observeEvent(c(input$selected_clinical, selected_module_profile()), {
      req(input$selected_clinical)
      sel_lookup <- selected_lookup()
      
      subset_clinical <- selectFromLookup(clinical, sel_lookup,
                                          matching_col = subject_var)
      module_expression <- selected_module_profile()
      
      subset_values <- getSubsetSampleClasses(subset_classes,
                                              sample_classes,
                                              input)
      selected_clinical_vars <-
        paste(scatterplot_variables[[input$selected_clinical]],
              subset_values,
              sep="_")
      
      # There steps combination then changing to long format
      # First attach, through a left_join, the clinical data to the lookup
      # Second, join with the expression, so we have multiple measures matched
      # with different expressions
      # Finally, transform to a long format data frame to plot
      clinical_df <- left_join(sel_lookup,
                               subset_clinical[, c(subject_var,
                                                   selected_clinical_vars)],
                               by = subject_var)
      combined_df <- left_join(clinical_df,
                               module_expression[, c(sample_var, "Expression")],
                               by = sample_var) %>%
        pivot_longer(selected_clinical_vars,
                     names_to = "Clinical",
                     values_to = "Value")
      
      plotHeight <- 200 * length(selected_clinical_vars)
      ac_length <- length(unique(sel_lookup[[across_class]]))
      plotWidth <- { if (ac_length < 4) ac_length*200 else 800 }
      
      output$scatterplot <- renderPlot({ 
        plotClinExpScatterplot(combined_df,
                               x = "Value",
                               y = "Expression",
                               scales = "free_x",
                               facet_var = c("Clinical", across_class),
                               colour_variable = across_class,
                               ncol = 5)
      }, plotWidth, plotHeight )
      
    })
    
    
  })
}