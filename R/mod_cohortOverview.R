# cohortOverview UI function
mod_cohortOverview_ui <-
  function(module_name,
           config,
           module_config) {
    cohortOverview_tab(
      sample_select = sampleClassInputs(config$sample_classes, module_name),
      gene_select = geneSelectInput(NULL, module_name),
      profile_variables = names(module_config$profile_variables),
      colour_variables = module_config$colour_variables,
      title = module_config$title,
      description = module_config$description,
      id = module_name
    )
  }
#' Cohort overview tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param gene_select select input with gene symbols
#' @param profile_variables list of group variables for trajectory profile
#' @param colour_variables list of unique variables for trajectory color
#' @param title optional title
#' @param description optional description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#'
#' @examples
#' if (interactive()) { 
#' sample_selection <- radioButtons("sample_1", "Select sample", samples)
#' gene_selection <- selectizeInput("gene_selection", "Select gene", gene_list)
#' cohortOverview_tab(sample_selection, gene_selection,
#'  c("measureA", "measureB"),
#'  c("measureA_1", "measureA_2"))
#' }
#' @noRd
cohortOverview_tab <-
  function(sample_select,
           gene_select,
           profile_variables,
           colour_variables,
           title = NULL,
           description = NULL,
           id = NULL) {
    
  ns <- NS(id)
  tabPanel(
    title = title %||% "Cohort overview",
    value = "cohortOverview",
    tags$h5(description %||% "Cohort overview"),
    splitLayout(
      verticalLayout(
        wellPanel(
          h5("Profile settings"),
          selectizeInput(
            ns("profile_variable"),
            label = "Select clinical variable for trajectory:",
            choices = profile_variables,
            options = list(dropdownParent = "body")
          ),
          selectizeInput(ns("profile_colour"),
                         label = "Select variable for trajectory colour:",
                         choices = colour_variables,
                         options = list(dropdownParent = "body")
                         ),
          checkboxInput(ns("order_by_colour"),
                        label = "order by colour variable?")
        )
        # wellPanel(
        #   h5("Expression settings"),
        #   sample_select,
        #   gene_select,
        #   checkboxInput(ns("flip_grouping"), label = p("group by type?"))
        # )
      ),
      flowLayout(
        r2d3::d3Output(ns("cohort_overview"), height = "500px", width =
                         "500px")
        # plotOutput(
        #   ns("cohort_expression"),
        #   height = 'auto',
        #   width = "60%"
        # )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
      #mainPanel(textOutput(ns("cohort_overview")))
    )
  )
}
#' cohortOverview Server Function
#'
#' @noRd 
mod_cohortOverview_server <- function(module_name, config, module_config) {
  
  moduleServer(module_name, function(input, output, session) {
    
    ns <- session$ns
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    
    # Load genes server side
    updateSelectizeInput(session,
                         "selected_gene",
                         choices = rownames(expression_matrix),
                         selected = "",
                         server = TRUE)
    
    output$cohort_overview <- r2d3::renderD3({ 
    
      req(input$profile_variable)
      req(input$profile_colour)
      
      profile_variable <- input$profile_variable
      profile_colour <- input$profile_colour
      
      profile_variable_list <-
        module_config$profile_variables[[profile_variable]][["values"]]
      
      profile_colour_type <-
        ifelse(is.factor(clinical[[profile_colour]]),
               "character",
               mode(clinical[[profile_colour]]))
      all_vars <- union(profile_variable_list, c(profile_colour))
      selected_clinical <- clinical[, all_vars]
      
      for (i in 1:ncol(selected_clinical)) {
        selected_clinical[[i]][is.na(selected_clinical[[i]])] <- 0
      }
      
      profile_order <- ifelse(input$order_by_colour,
                              profile_colour, 
                              "estimate")
      first_profile_var <- profile_variable_list[1]
      last_profile_var <- profile_variable_list[length(profile_variable_list)]
      selected_clinical$estimate <- selected_clinical[[last_profile_var]] / 
                                    selected_clinical[[first_profile_var]]
      selected_clinical <-
        selected_clinical[order(selected_clinical[profile_order],
                                decreasing = TRUE),]
      r2d3::r2d3(data = selected_clinical,
          script = app_sys("app/build/cohort_overview.js"),
          d3_version = 5,
          dependencies = app_sys("app/build/d3-legend.js"),
          options = list (
            id = module_name,
            color = profile_colour,
            color_type = profile_colour_type,
            columns = profile_variable_list
          )
        )
    })
  })
}