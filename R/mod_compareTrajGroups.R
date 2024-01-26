# module UI Function
mod_compareTrajGroups_ui <-
    function(module_name,
    config,
    module_config) {
        compareTrajGroups_tab(
            sampleCategoryInputs(
                config$sample_categories,
                module_name,
                module_config$subset_categories
            ),
            varsSelectInput(module_config$compare_variables,
                module_name,
                initEmpty = FALSE
            ),
            geneSelectInput(gene_list = NULL, module_name),
            module_config$title,
            module_config$description,
            module_config$advanced,
            module_name
        )
    }

compareTrajGroups_tab <-
    function(sample_select,
    vars_select,
    gene_select,
    title = NULL,
    description = NULL,
    advanced = NULL,
    id = NULL) {
        ns <- NS(id)
        tabPanel(
            title = title %||% "Compare trajectories",
            value = "compareTrajGroups",
            tags$h5(description %||% "Comparison between groups"),
            splitLayout(
                verticalLayout(
                    wellPanel(
                        gene_select %>%
                            shinyhelper::helper(content = "compareTrajGroups",
                                                size = "l"),
                        vars_select,
                        tags$hr(),
                        tags$b("Sample selection"),
                        sample_select,
                        advanced_settings_inputs(advanced, id)
                    )
                ),
                verticalLayout(
                    # OUTPUTS
                    vegawidget::vegawidgetOutput(ns("trajplot"))
                ),
                cellWidths = c("20%", "80%"),
                cellArgs = list(style = "white-space: normal;")
            )
        )
    }
mod_compareTrajGroups_server <- function(module_name, config, module_config) {
    moduleServer(module_name, function(input, output, session) {
        ns <- session$ns

        measures_data <- config$data$measures_data
        expression_matrix <- config$data$expression_matrix
        sample_lookup <- config$data$sample_lookup
        subject_var <- config$subject_variable
        sample_var <- config$sample_variable
        sample_categories <- config$sample_categories
        timesep <- config$timesep

        default_fit_method <- config$default_fit_method

        subset_categories <- module_config$subset_categories
        trajectory_category <- module_config$trajectory_category
        sidebyside_category <- module_config$sidebyside_category
        traj_palette <- module_config$custom_traj_palette

        # Load genes server side
        updateSelectizeInput(session,
            "selected_gene",
            choices = rownames(expression_matrix),
            selected = "",
            server = TRUE
        )

        # Select only the sample classes required by this view
        # For each class, get the value selected by the user and filter the
        # lookup
        selected_lookup <- reactive({
            subset_values <- getSelectedSampleCategories(
                sample_categories,
                input,
                subset_categories
            )
            selectMatchingValues(sample_lookup, subset_values)
        })

        expression_from_lookup <- reactive({
            sel_lookup <- selected_lookup()
            expression_matrix[, sel_lookup[[sample_var]]]
        })

        measures_from_lookup <- reactive({
            sel_lookup <- selected_lookup()
            selectFromLookup(measures_data, sel_lookup,
                matching_col = subject_var
            )
        })

        output$trajplot <- vegawidget::renderVegawidget({
            req(input$selected_gene)

            sel_lookup <- selected_lookup()
            subset_expression <- expression_from_lookup()
            selected_measures <- measures_from_lookup()
            fit_method <-
                input$fit_method %||% default_fit_method %||% "linear"

            # Return matching selected_variable
            compare_col_id <-
                grep(
                    paste0("(", input$selected_variable, ")\\", timesep, ".*"),
                    colnames(selected_measures)
                )
            compare_col_vars <-
                colnames(selected_measures)[compare_col_id]
            subset_measures <-
                selected_measures[, c(subject_var, compare_col_vars)]
            # Convert to long and separate unique variable from suffix
            subset_long <- pivot_longer(subset_measures,
                -all_of(subject_var),
                names_to = c(".value", trajectory_category),
                names_sep = timesep
            )
            combined <- left_join(sel_lookup,
                subset_long,
                by = c(subject_var, trajectory_category)
            )
            selected_expression <- subset_expression[
                input$selected_gene,
                combined[[sample_var]]
            ]
            validate(
                need(
                    all(!is.na(selected_expression)) &
                        (length(selected_expression) > 0),
                    "Transcript not found in subset or subset combination does
                    not exist."
                )
            )
            combined$expression <- selected_expression
            traj_vars <- c(
                subject_var, trajectory_category, sidebyside_category,
                input$selected_variable, "expression"
            )
            df <- combined[, traj_vars]

            trajplot <- vega_traj_scatterplot(
                data = df[order(df[[subject_var]],
                                df[[trajectory_category]]), ],
                x = input$selected_variable,
                facet_var = sidebyside_category,
                color_var = trajectory_category,
                color_palette = traj_palette
            )
            trajplot %>%
                vega_add_fitline(fit_method) %>%
                vegawidget::as_vegaspec()
        })
    })
}
