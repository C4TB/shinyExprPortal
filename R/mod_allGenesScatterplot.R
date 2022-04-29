mod_allGenesScatterplot_ui <- function(module_name, config, module_config) {
  coordinates_data <- module_config$coordinates_data
  fill <- module_config$label_column
  
  allGenesScatterplot_tab(
    as.factor(sort(unique(coordinates_data[[fill]]))),
    sample_select = sampleCategoryInputs(config$sample_categories, module_name),
    module_config$annotation_variables,
    module_config$title,
    module_config$description,
    module_name
  )
}

allGenesScatterplot_tab <- function(list_of_clusters,
                                    sample_select,
                                    annotation_variables = NULL,
                                    title = NULL,
                                    description = NULL,
                                    id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "2D Plot",
    value = "allGenesScatterplot",
    tags$h5(description %||%
              "Select a subset of samples for fold change overlay. 
            Click on a point or cluster in legend to view heatmap; 
            click in empty space to reset."),
    splitLayout(
      verticalLayout(
        wellPanel(
          tags$b("Sample selection"),
          sample_select,
          tags$hr(),
          tags$b("Plot options"),
          checkboxGroupInput(
            inputId = ns("list_of_clusters"),
            label = "Select cluster to display:",
            choices = list_of_clusters,
            selected = list_of_clusters),
          { if (not_null(annotation_variables)) 
            selectizeInput(ns("selected_annotations"),
                           label = "Select heatmap annotations:",
                           choices = annotation_variables,
                           multiple = TRUE,
                           options = list(dropdownParent = "body")
            )
            else NULL }
        )
      ),
      verticalLayout(
        ## OUTPUTS
         vegawidget::vegawidgetOutput(ns("scatterplot"), width = "auto"),
         conditionalPanel(
           condition = "output.show_heatmap == true",
           ns = ns,
         verticalLayout(
           actionButton(ns("show_genes"), label = "View genes in cluster"),
           uiOutput(ns("heatmap_ui"))
         )
        )
        ),
        cellWidths = c("20%", "80%"),
        cellArgs = list(style = "white-space: normal;")
    )
  )
}

#' @import vegawidget 
mod_allGenesScatterplot_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup
    all_mean <- rowMeans(expression_matrix, na.rm = T)
    
    sample_var <- config$sample_variable
    subject_var <- config$subject_variable
    sample_classes <- config$sample_categories
    
    annotation_colors <- module_config$annotation_colours %||% NULL
    annotation_range <- module_config$annotation_range %||% NULL
    coordinates_data <- module_config$coordinates_data
    fill <- module_config$label_column
    coordinates_data[[fill]] <- as.numeric(coordinates_data[[fill]]) 
    cols_df <- colnames(coordinates_data)
    name_col <- cols_df[[1]]
    x <- cols_df[[2]]
    y <- cols_df[[3]]
    
    selected_lookup <- reactive({
      list_of_values <- getSelectedSampleCategories(sample_classes, input)
      selectMatchingValues(sample_lookup, list_of_values)
    })
   
    fc_from_lookup <- reactive({
      sel_lookup <- selected_lookup()
      validate(need(nrow(sel_lookup) > 0, "No samples found for combination"))
      subset_mat <- expression_matrix[, sel_lookup[[sample_var]], drop=F]
      if (ncol(subset_mat) == ncol(expression_matrix))
       return(list("all_samples", all_mean))
      subset_mean <- rowMeans(subset_mat, na.rm = T)
      fc <- subset_mean - all_mean
      list("subset", fc)
    })
   
    scatterplot_data <- reactive({
      fc_list <- fc_from_lookup()
      fc <- fc_list[[2]]
      coordinates_data$Fold_Change <- fc[match(coordinates_data[[1]], names(fc))]
      # fc_list[[1]] can be "all_samples" or "subset"
      coordinates_data$mean_type <- fc_list[[1]]
      coordinates_data %>%
       filter(.data[[fill]] %in% input$list_of_clusters)
    })
  
    # Using custom function for now until vegawidget is updated
    getSelectedCluster <- custom_vw_shiny_get_signal("scatterplot",
                                                     "cluster_sel",
                                                     "value",
                                                     module_name)
    
    # Use data = NULL because we use a signal to set the data
    output$scatterplot <- renderVegawidget({
      vega_scatterplot_overlay(
        data = NULL,
        x = x,
        y = y,
        color_var = fill,
        overlay_var = "Fold_Change",
        tooltip_vars = c(name_col, fill)
      )
    })
    
    # Set title
    plot_title <- reactive({
      df <- scatterplot_data()
      if (df$mean_type[[1]] == "all_samples") {
          "Mean expression of all samples"
      } else 
        "Expression difference between mean of subgroup and mean of all samples"
    })
    
    # Update data
    vegawidget::vw_shiny_set_data(ns("scatterplot"),
                                  "values",
                                  scatterplot_data())
    # Update title of second plot
    vegawidget::vw_shiny_set_signal(ns("scatterplot"),
                                  "second_title",
                                  plot_title())
   
    # Outputs after selecting a cluster
    output$show_heatmap <-
      eventReactive(getSelectedCluster(), {
        selected_cluster <- getSelectedCluster()
        selected_cluster <- selected_cluster[[fill]][[1]]
        if (!is.null(selected_cluster)) TRUE else FALSE
      }, ignoreInit = TRUE)
    outputOptions(output, "show_heatmap", suspendWhenHidden = FALSE)
    
    # Modal dialog to show genes
    subset_genes <- eventReactive(getSelectedCluster(), {
      df <- scatterplot_data()
      selected_cluster <- getSelectedCluster()
      selected_cluster <-selected_cluster[[fill]][[1]]
      df[df[[fill]] == as.numeric(selected_cluster),1,drop=TRUE]
    })
    
    observeEvent(input$show_genes, {
      list_of_genes <- paste(subset_genes(), collapse = ", ")
      showModal(
        modalDialog(title = "Genes in selected cluster",
                    easyClose = TRUE,
                    span("Click to select all to copy"),
                    pre(list_of_genes,
                        class = "selectable",
                        style = "white-space: pre-wrap;")
        )
      )
    })
    
    heatmap_height <- function(n) {
      if (n > 200) 50+(5*n) else 50+(10*n)
    }
    
    output$heatmap_ui <- renderUI({
      iheatmaprOutput(ns("cluster_heatmap"),
                      height = heatmap_height(length(subset_genes())))
    })
    
    clinical_from_lookup <- eventReactive(selected_lookup(), {
      sel_lookup <- selected_lookup()
      selectFromLookup(clinical, sel_lookup,
                       matching_col = subject_var)
    })
    
    annotations <- reactive({
      if (!isTruthy(input$selected_annotations))
        return(NULL)
      selected_clinical <- clinical_from_lookup()
      selected_clinical[rev(input$selected_annotations)]
    })
    
    observeEvent(subset_genes(), {
      sel_lookup <- selected_lookup()
      list_of_genes <- rev(subset_genes())
      if (length(list_of_genes) > 0) {
      output$cluster_heatmap <- renderIheatmap({
        subset_mat <- expression_matrix[list_of_genes, sel_lookup[[sample_var]]]
        hm <- iheatmap(
          subset_mat,
          colors = rev(
            RColorBrewer::brewer.pal(11, "RdBu")
          ),
          row_labels = if (nrow(subset_mat) > 200) F else T,
          scale = "rows",
          scale_method = "standardize",
          name = "Expression z-scores",
          layout = list(font = list(size = 9),
                        plot_bgcolor = "transparent",
                        paper_bgcolor = "transparent"))
        # Optional annotations
        annots <- annotations()
        if (not_null(annots)) {
          hm <- hm %>% 
            custom_add_col_annotations(annots, size = 0.025,
                                       colors = annotation_colors,
                                       range = annotation_range)
        }
        if (length(list_of_genes) < 200) hm <- hm %>% add_row_clustering()
        hm %>% add_col_clustering()
        })
    }
    })
  })
}