# singleGeneCorr UI Function
mod_singleGeneCorr_ui <- function(module_name, config, module_config) {
  singleGeneCorr_tab(
    sample_select =
      sampleCategoryInputs(config$sample_categories,
                           module_name,
                           module_config$subset_categories),
    gene_select = geneSelectInput(NULL, module_name),
    colors = module_config$color_variables,
    outputs = module_config$tabs,
    advanced = module_config$advanced,
    title = module_config$title,
    description = module_config$description,
    id = module_name
  )
}
#' Single gene correlation tab UI
#'
#' @param sample_select radio inputs for sample classes
#' @param gene_select select input with gene symbols
#' @param colors list of variables for color selection
#' @param outputs configuration of tabs with plots
#' @param advanced boolean flag to show or hide advanced options
#'    such as outlier removal
#' @param title optional title
#' @param description optional description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   sample_select <- radioButtons("sample_1", "Select sample", samples)
#'   gene_select <- selectizeInput("gene_selection", "Select gene", gene_list)
#'   output_list <- list("Vars" = c("Var1", "Var2"))
#'   singleGeneCorr_tab(
#'     sample_select, gene_select, c("Var1", "Var2"),
#'     output_list
#'   )
#' }
singleGeneCorr_tab <-
  function(sample_select,
           gene_select,
           colors,
           outputs,
           advanced = NULL,
           title = NULL,
           description = NULL,
           id = NULL) {
    ns <- NS(id)
    tabPanel(
      title = title %||% "Single gene",
      value = "singleGeneCorr",
      tags$h5(description %||%
        "Correlation between a selected gene and measures"),
      splitLayout(
        verticalLayout(
          wellPanel(
            gene_select %>%
              shinyhelper::helper(content = "singleGeneCorr", size = "l"),
            tags$hr(),
            tags$b("Sample selection"),
            sample_select,
            if (!is.null(colors)) {
              tagList(
                tags$hr(),
                tags$b("Plot options"),
                selectizeInput(
                  ns("color_variable"),
                  label = "Select color:",
                  choices = c("None" = "", colors),
                  options = list(allowEmptyOption = TRUE)
                )
              )
            } else {
              NULL
            },
            advanced_settings_inputs(advanced, id)
          )
        ),
        verticalLayout(
          conditionalPanel(
            "output[\'error_message\'] == true",
            ns = ns,
            tags$span("Transcript not found in subset or
                       subset combination does not exist.",
              style = "color: gray"
            )
          ),
          conditionalPanel(
            "input[\'selected_gene\'] == ''",
            ns = ns,
            tags$span("No gene selected", style = "color: gray")
          ),
          conditionalPanel(
            "(input[\'selected_gene\'] != '') &&
             (output[\'error_message\'] == false)",
            ns = ns,
            do.call(tabsetPanel, plotsTabPanels(outputs, ns))
          )
        ),
        cellWidths = c("20%", "80%"),
        cellArgs = list(style = "white-space: normal;")
      )
    )
  }
#' singleGeneCorr Server Function
#'
#' @noRd
mod_singleGeneCorr_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns


    # App level config/defaults
    measures_data <- config$data$measures_data
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup

    cores <- config$nthreads
    adjust_method <- config$adjust_method

    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    sample_categories <- config$sample_categories

    default_measures_outliers <- config$default_measures_outliers
    default_expr_outliers <- config$default_expression_outliers
    default_corr_method <- config$default_correlation_method
    default_fit_method <- config$default_fit_method

    subset_categories <- module_config$subset_categories
    # Module defaults
    custom_point_colors <- module_config$custom_point_colors

    # Load genes server side
    updateSelectizeInput(
      session,
      "selected_gene",
      choices = rownames(expression_matrix),
      selected = "",
      server = TRUE
    )
    # UI updates from URL
    observeEvent(session$userData$singleGeneCorr, {
      params <- session$userData$singleGeneCorr
      for (sample_category in sample_categories) {
        sc_name <- sample_category$name
        if (!is.null(params[[sc_name]])) {
          updateSelectizeInput(session,
            sc_name,
            selected = params[[sc_name]]
          )
        }
      }
      if (!is.null(params$gene)) {
        updateSelectizeInput(
          session,
          "selected_gene",
          choices = rownames(expression_matrix),
          selected = params$gene,
          server = TRUE
        )
      }
    })

    outlier_functions <-
      c(
        "5/95 percentiles" = valuesInsideQuantileRange,
        "IQR" = valuesInsideTukeyFences,
        "No" = function(x) TRUE
      )

    selected_lookup <- reactive({
      list_of_values <-
        getSelectedSampleCategories(sample_categories, input, subset_categories)
      selectMatchingValues(sample_lookup, list_of_values)
    })

    expression_from_lookup <- reactive({
      samples <- selected_lookup()[[sample_var]]
      samples <- samples[!is.na(samples)]
      expression_matrix[, samples]
    })

    measures_from_lookup <- reactive({
      sel_lookup <- selected_lookup()
      selectFromLookup(measures_data, sel_lookup,
        matching_col = subject_var
      )
    })

    # Compute correlation matrix and use cacheing
    correlation_df <- reactive({
      measures_outliers <-
        input$measures_outliers %||% default_measures_outliers %||% "No"
      expression_outliers <-
        input$expression_outliers %||% default_expr_outliers %||% "No"
      correlation_method <-
        input$correlation_method %||% default_corr_method %||% "pearson"

      selected_expression <- expression_from_lookup()
      subset_measures <- measures_from_lookup()

      tab_output_list <- module_config$tabs

      measures_vars <- unique(unlist(lapply(
        tab_output_list,
        function(x) x$variables
      )))

      selected_measures <-
        replaceFalseWithNA(
          subset_measures[, measures_vars],
          outlier_functions(measures_outliers)
        )
      all_na_lv <-
        vapply(colnames(selected_measures),
               function(x) all(is.na(selected_measures[[x]])),
               logical(1))
      selected_measures <- selected_measures[ , !all_na_lv]
      measures_vars <- measures_vars[!all_na_lv]

      selected_expression <-
        replaceFalseWithNA(
          t(na.omit(selected_expression)),
          outlier_functions(expression_outliers)
        )

      corr_df <- longCorrelationMatrix(
        first_col_name = "Gene",
        name_to = "Measure",
        y = selected_expression,
        x = selected_measures,
        method = correlation_method,
        adjust_method = adjust_method,
        cores = cores
      )
      # Change to factor
      corr_df[["Measure"]] <-
        factor(corr_df[["Measure"]], levels = measures_vars)
      corr_df
    }) %>% bindCache(
      input$measures_outliers,
      input$expression_outliers,
      input$correlation_method,
      selected_lookup()
    )

    # To create the plot for each tab we need to use observe
    # and iterate through each tab
    observe({
      req(input$selected_gene)
      if (isTruthy(input$color_variable)) {
        color_var <- input$color_variable
      } else {
        color_var <- NULL
      }
      selected_gene <- input$selected_gene
      measures_outliers <-
        input$measures_outliers %||% default_measures_outliers %||% "No"
      expression_outliers <-
        input$expression_outliers %||% default_expr_outliers %||% "No"
      correlation_method <-
        input$correlation_method %||% default_corr_method %||% "pearson"
      fit_method <-
        input$fit_method %||% default_fit_method %||% "linear"

      selected_expression <- expression_from_lookup()
      subset_measures <- measures_from_lookup()

      # As we are in observe, we use a special output to show error
      # If there are no genes for this subset, display message
      if (all(is.na(selected_expression[selected_gene, ])) |
        length(selected_expression) == 0) {
        output$error_message <- reactive({
          TRUE
        })
        outputOptions(output, "error_message", suspendWhenHidden = FALSE)
      } else {
        output$error_message <- reactive({
          FALSE
        })
        outputOptions(output, "error_message", suspendWhenHidden = FALSE)
      }
      req(all(!is.na(selected_expression[selected_gene, ])) &
        (length(selected_expression) > 0))
      tab_output_list <- module_config$tabs

      measures_vars <- unique(unlist(lapply(
        tab_output_list,
        function(x) x$variables
      )))

      subset_measures[, measures_vars] <-
        replaceFalseWithNA(
          subset_measures[, measures_vars],
          outlier_functions(measures_outliers)
        )

      all_na_lv <-
        vapply(colnames(subset_measures),
               function(x) all(is.na(subset_measures[[x]])),
               logical(1))
      measures_vars <- measures_vars[!all_na_lv]

      selected_expression <-
        replaceFalseWithNA(
          t(na.omit(selected_expression)),
          outlier_functions(expression_outliers)
        )

      corr_df <- correlation_df()
      corr_df <- corr_df[corr_df$Gene == selected_gene, -1]

      # We go through the list of outputs defined in the configuration file
      # as they were also used to create pairs of tabPanel-plotOutput
      # Local scope is required otherwise the last tab_output will override
      # previous ones

      for (i in seq_along(tab_output_list)) {
        local({
          tab_output <- tab_output_list[[i]]
          output_name <- tab_output$name
          output_scale <- tab_output$scale
          output_vars <- unique(tab_output$variables)

          # Add color_var to list of variables to subset
          # If NULL nothing is added
          # unique makes it so that it's not repeated
          subset_vars <- unique(c(output_vars, color_var))

          not_na_lv <-
            vapply(subset_vars,
                   function(x) all(is.na(subset_measures[[x]])),
                   logical(1))
          subset_vars <- subset_vars[!not_na_lv]

          # Check if an optional palette was provided
          if (!is.null(color_var)) {
            if (color_var %in% names(custom_point_colors)) {
              manual_colors <- custom_point_colors[[color_var]]
            } else {
              manual_colors <- NULL
            }
          }

          # Get only the variables for this tab
          tab_measures <- subset_measures[, subset_vars]

          corr_df_subset <- corr_df[corr_df[["Measure"]] %in% subset_vars, ]

          # Filter to selected gene
          if (ncol(tab_measures) > 0) {
            combined_df <-
              cbind(
                Expression = selected_expression[, selected_gene],
                tab_measures
              ) %>%
              pivot_longer(output_vars,
                names_to = "Measure",
                values_to = "Value"
              )

            # Use output_vars as levels to preserve order defined in config
            combined_df[["Measure"]] <-
              factor(combined_df[["Measure"]],
                levels = output_vars
              )
            # Retrieve element width and use that to resize plot
            # It's reactive and seems to be drawing twice when first run
            output_id <- paste("output", ns(output_name), "width", sep = "_")
            out_width <-
              session$clientData[[output_id]]
            facet_width <- (out_width / 4) %||% 200
            plotHeight <- ceiling(length(output_vars) / 4) * facet_width
            plotWidth <- {
              if (length(output_vars) < 4) {
                length(output_vars) * facet_width
              } else {
                ifelse(!is.null(out_width), out_width, 800)
              }
            }

            corr_labels <- c(
              "pearson" = "r",
              "spearman" = "\u03c1",
              "kendall" = "\u03C4"
            )

            # TODO: refactor this with padj being optional
            corr_lookup <-
              paste0("{", paste(apply(corr_df_subset, 1, function(x) {
                name <- x[["Measure"]]
                corr <- round(as.numeric(x[["estimate"]]), digits = 2)
                pvalue <- round(as.numeric(x[["pvalue"]]), digits = 2)
                padj <- round(as.numeric(x[["padj"]]), digits = 2)
  # glue::glue("'{name}': ['{name}', 'r: {corr}, p: {pvalue}, p_adj: {padj}']")
                paste0(
                  "'", name, "': ['", name,
                  "', '", corr_labels[correlation_method], ": ",
                  corr, ", P: ", pvalue, ", P_adj: ", padj, "']"
                )
              }), collapse = ","), "}")

          plotName <- paste0(output_name,"_vw")
          output[[output_name]] <- renderUI({

            vegawidget::vegawidgetOutput(ns(plotName),
                                         width = "auto",
                                         height = "auto")
          })

          output[[plotName]] <- vegawidget::renderVegawidget({
            scatterplot <- vega_layer_scatterplot(
              combined_df,
              x = "Value",
              y = "Expression",
              facet_var = "Measure",
              facet_sort = output_vars,
              label_lookup = corr_lookup,
              scales = output_scale,
              color_var = color_var,
              custom_colors = manual_colors,
              gene_name = input$selected_gene,
              opts = list(ncolumns = 4)
            )

            scatterplot %>%
              vega_add_fitline(fit_method) %>%
              vegawidget::as_vegaspec()
          })
          #if ncol
          } else {
            output[[output_name]] <- renderUI({
                htmlOutput(ns(paste0(output_name,"_text")))
            })
            output[[paste0(output_name,"_text")]] <- renderPrint({
              div("Selected subset has no data for plots",
                   class="shiny-output-error-validation")
            })
          }
        })
      }
    })
  })
}
