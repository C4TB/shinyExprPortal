mod_geneProjectionOverlay_ui <- function(module_name, config, module_config) {
  coordinates_data <- module_config$coordinates_data
  group_variable <- module_config$group_variable

  geneProjectionOverlay_tab(
    as.factor(sort(unique(coordinates_data[[group_variable]]))),
    sample_select =
      sampleCategoryInputs(config$sample_categories,
                           module_name,
                           module_config$subset_categories),
    module_config$annotation_variables,
    module_config$title,
    module_config$description,
    module_name
  )
}

geneProjectionOverlay_tab <- function(list_of_groups,
                                    sample_select,
                                    annotation_variables = NULL,
                                    title = NULL,
                                    description = NULL,
                                    id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "2D Overlay",
    value = "geneProjectionOverlay",
    tags$h5(description %||%
      "Select a subset of samples for fold change overlay.
            Click on a point or group in legend to highlight;
            click in empty space to reset."),
    splitLayout(
      verticalLayout(
        wellPanel(
          tags$b("Sample selection") %>%
            shinyhelper::helper(content = "geneProjectionOverlay", size = "l"),
          sample_select,
          tags$hr(),
          tags$b("Plot options"),
          checkboxGroupInput(
            inputId = ns("list_of_groups"),
            label = "Select groups to display:",
            choices = list_of_groups,
            selected = list_of_groups
          ),
          if (!is.null(annotation_variables)) {
            selectizeInput(ns("selected_annotations"),
              label = "Select heatmap annotations:",
              choices = annotation_variables,
              multiple = TRUE,
              options = list(dropdownParent = "body")
            )
          } else {
            NULL
          }
        )
      ),
      verticalLayout(
        ## OUTPUTS
        conditionalPanel(
          condition = "input.list_of_groups.length >= 1",
          ns = ns,
          vegawidget::vegawidgetOutput(ns("scatterplot"), width = "auto")
        ),
        conditionalPanel(
          condition = "input.list_of_groups.length == 0",
          ns = ns,
          p("Please select at least one group for overlay",
            class = "shiny-output-error-validation")
        ),
        conditionalPanel(
          condition = "output.show_heatmap == true",
          ns = ns,
          fluidRow(
            actionButton(ns("show_genes"), label = "View genes in group"),
            actionButton(ns("show_heatmap"), label = "View expression heatmap")
          )
        )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}

mod_geneProjectionOverlay_server <- function(module_name, config,
                                             module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    measures_data <- config$data$measures_data
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    all_mean <- rowMeans(expression_matrix, na.rm = TRUE)

    sample_var <- config$sample_variable
    subject_var <- config$subject_variable
    sample_classes <- config$sample_categories

    subset_categories <- module_config$subset_categories
    custom_annotation_colors <- module_config$custom_annotation_colors
    annotation_range <- module_config$annotation_range
    coordinates_data <- module_config$coordinates_data
    custom_group_colors <- module_config$custom_group_colors
    group_variable <- module_config$group_variable
    coordinates_data[[group_variable]] <-
      as.numeric(coordinates_data[[group_variable]])
    cols_df <- colnames(coordinates_data)
    name_col <- cols_df[[1]]
    x <- cols_df[[2]]
    y <- cols_df[[3]]

    selected_lookup <- reactive({
      list_of_values <-
        getSelectedSampleCategories(sample_classes, input, subset_categories)
      selectMatchingValues(sample_lookup, list_of_values)
    })

    fc_from_lookup <- reactive({
      sel_lookup <- selected_lookup()
      validate(need(nrow(sel_lookup) > 0, "No samples found for combination"))
      samples <- sel_lookup[[sample_var]]
      samples <- samples[!is.na(samples)]
      subset_mat <- expression_matrix[, samples, drop = FALSE]
      if (ncol(subset_mat) == ncol(expression_matrix)) {
        return(list("all_samples", all_mean))
      }
      subset_mean <- rowMeans(subset_mat, na.rm = TRUE)
      fc <- subset_mean - all_mean
      list("subset", fc)
    }) %>% bindCache(selected_lookup())

    scatterplot_data <- reactive({
      fc_list <- fc_from_lookup()
      fc <- fc_list[[2]]
      coordinates_data$Fold_Change <-
        fc[match(coordinates_data[[1]], names(fc))]
      # fc_list[[1]] can be "all_samples" or "subset"
      coordinates_data$mean_type <- fc_list[[1]]
      validate(need(length(input$list_of_groups) >= 1, "Please select at least one group to display"))
      lv <- coordinates_data[[group_variable]] %in% input$list_of_groups
      coordinates_data[lv, ]
    })

    # Using custom function for now until vegawidget is updated
    getSelectedGroup <- vegawidget::vw_shiny_get_signal(
      ns("scatterplot"),
      "group_sel",
      "value"
    )

    # Use data = NULL because we use a signal to set the data
    output$scatterplot <- vegawidget::renderVegawidget({
      vega_scatterplot_overlay(
        data = NULL,
        x = x,
        y = y,
        color_var = group_variable,
        overlay_var = "Fold_Change",
        tooltip_vars = c(name_col, group_variable),
        custom_group_colors = custom_group_colors,
        colors_domain = sort(unique(coordinates_data[[group_variable]]))
      )
    })

    # Set title
    plot_title <- reactive({
      df <- scatterplot_data()
      if (df$mean_type[[1]] == "all_samples") {
        "Mean expression of all samples"
      } else {
        "Expression difference between mean of subgroup and mean of all samples"
      }
    })

    # Update data
    vegawidget::vw_shiny_set_data(
      ns("scatterplot"),
      "values",
      scatterplot_data()
    )
    # Update title of second plot
    vegawidget::vw_shiny_set_signal(
      ns("scatterplot"),
      "second_title",
      plot_title()
    )

    # Outputs after selecting a group
    output$show_heatmap <-
      eventReactive(getSelectedGroup(), {
          selected_group <- getSelectedGroup()
          selected_group <- selected_group[[group_variable]][[1]]
          if (!is.null(selected_group)) TRUE else FALSE
        },
        ignoreInit = TRUE
      )
    outputOptions(output, "show_heatmap", suspendWhenHidden = FALSE)

    # Modal dialog to show genes
    subset_genes <- eventReactive(getSelectedGroup(), {
      df <- scatterplot_data()
      selected_group <- getSelectedGroup()
      selected_group <- selected_group[[group_variable]][[1]]
      df[df[[group_variable]] == as.numeric(selected_group), 1, drop = TRUE]
    })

    observeEvent(input$show_genes, {
      list_of_genes <- paste(subset_genes(), collapse = ", ")
      showModal(
        modalDialog(
          title = "Genes in selected group",
          easyClose = TRUE,
          span("Click to select all to copy"),
          pre(list_of_genes,
            class = "selectable",
            style = "white-space: pre-wrap;"
          )
        )
      )
    })

    heatmap_height <- function(n) {
      if (n > 200) 50 + (5 * n) else 50 + (10 * n)
    }

    # Modal dialog to show heatmap
    observeEvent(input$show_heatmap, {
      showModal(
        modalDialog(
          title = "Expression for genes in group",
          easyClose = TRUE,
          iheatmaprOutput(ns("group_heatmap"),
                          height = heatmap_height(length(subset_genes()))
          )
        )
      )
    })

    measures_from_lookup <- eventReactive(selected_lookup(), {
      sel_lookup <- selected_lookup()
      selectFromLookup(measures_data, sel_lookup,
        matching_col = subject_var
      )
    })

    annotations <- reactive({
      if (!isTruthy(input$selected_annotations)) {
        return(NULL)
      }
      selected_measures <- measures_from_lookup()
      selected_measures[rev(input$selected_annotations)]
    })

    observeEvent(subset_genes(), {
      sel_lookup <- selected_lookup()
      list_of_genes <- rev(subset_genes())
      if (length(list_of_genes) > 0) {
        output$group_heatmap <- renderIheatmap({
          samples <- sel_lookup[[sample_var]]
          subset_mat <-
            expression_matrix[list_of_genes, samples[!is.na(samples)]]
          hm <- iheatmap(
            subset_mat,
            colors = rev(
              RColorBrewer::brewer.pal(11, "RdBu")
            ),
            row_labels = if (nrow(subset_mat) > 200) FALSE else TRUE,
            scale = "rows",
            scale_method = "standardize",
            name = "Expression z-scores",
            layout = list(
              font = list(size = 9),
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent"
            )
          )
          # Optional annotations
          annots <- annotations()
          if (!is.null(annots)) {
            hm <- hm %>%
              custom_add_col_annotations(annots,
                size = 0.025,
                colors = custom_annotation_colors,
                range = annotation_range
              )
          }
          if (length(list_of_genes) < 200) hm <- hm %>% add_row_clustering()
          hm %>% add_col_clustering()
        })
      }
    })
  })
}
