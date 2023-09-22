# cohortOverview UI function
mod_cohortOverview_ui <-
    function(module_name,
    config,
    module_config) {
        cohortOverview_tab(
            sample_select =
                sampleCategoryInputs(config$sample_categories, module_name),
            gene_select = geneSelectInput(NULL, module_name),
            profile_variables = names(module_config$profile_variables),
            color_variables = module_config$color_variables,
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
#' @param color_variables list of unique variables for trajectory color
#' @param title optional title
#' @param description optional description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#'
#' @noRd
cohortOverview_tab <-
    function(sample_select,
    gene_select,
    profile_variables,
    color_variables,
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
                            label = "Select measures variable for trajectory:",
                            choices = profile_variables,
                            options = list(dropdownParent = "body")
                        ),
                        selectizeInput(ns("profile_color"),
                            label = "Select variable for trajectory color:",
                            choices = color_variables,
                            options = list(dropdownParent = "body")
                        ),
                        checkboxInput(ns("order_by_color"),
                            label = "order by color variable?"
                        )
                    )
                ),
                flowLayout(
                    r2d3::d3Output(ns("cohort_overview"),
                        height = "500px", width =
                            "500px"
                    )
                ),
                cellWidths = c("20%", "80%"),
                cellArgs = list(style = "white-space: normal;")
            )
        )
    }
#' cohortOverview Server Function
#'
#' @noRd
mod_cohortOverview_server <- function(module_name, config, module_config) {
    moduleServer(module_name, function(input, output, session) {
        ns <- session$ns

        measures_data <- config$data$measures_data
        expression_matrix <- config$data$expression_matrix
        sample_lookup <- config$data$sample_lookup

        # Load genes server side
        updateSelectizeInput(session,
            "selected_gene",
            choices = rownames(expression_matrix),
            selected = "",
            server = TRUE
        )

        output$cohort_overview <- r2d3::renderD3({
            req(input$profile_variable)
            req(input$profile_color)

            profile_variable <- input$profile_variable
            profile_color <- input$profile_color

            profile_variable_list <-
                module_config$profile_variables[[profile_variable]][["values"]]

            profile_color_type <-
                ifelse(is.factor(measures_data[[profile_color]]),
                    "character",
                    mode(measures_data[[profile_color]])
                )
            all_vars <- union(profile_variable_list, c(profile_color))
            selected_measures <- measures_data[, all_vars]

            for (i in seq_len(ncol(selected_measures))) {
                selected_measures[[i]][is.na(selected_measures[[i]])] <- 0
            }

            profile_order <- ifelse(input$order_by_color,
                profile_color,
                "estimate"
            )
            first_profile_var <- profile_variable_list[1]
            last_profile_var <-
                profile_variable_list[length(profile_variable_list)]
            selected_measures$estimate <-
                selected_measures[[last_profile_var]] /
                selected_measures[[first_profile_var]]
            selected_measures <-
                selected_measures[order(selected_measures[[profile_order]],
                    decreasing = TRUE
                ), ]
            add_cohort_overview(
                selected_measures,
                module_name,
                profile_color,
                profile_color_type,
                profile_variable_list
            )
        })
    })
}
