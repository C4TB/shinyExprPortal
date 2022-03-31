mod_allGenesScatterplot_ui <- function(module_name, config, module_config) {
  coordinates_data <- module_config$coordinates_data
  fill <- module_config$annotation_column
  
  allGenesScatterplot_tab(
    as.factor(sort(unique(coordinates_data[[fill]]))),
    sample_select = sampleCategoryInputs(config$sample_categories, module_name),
    id = module_name
  )
}

allGenesScatterplot_tab <- function(list_of_groups,
                                    sample_select,
                                    title = NULL,
                                    description = NULL,
                                    id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "2D Plot",
    value = "allGenesScatterplot",
    tags$h5(description %||%
              "Select a subset of samples for fold change overlay. 
            Click on a point or group in legend to highlight."),
    splitLayout(
      verticalLayout(
        wellPanel(
          sample_select,
          checkboxGroupInput(
            inputId = ns("list_of_groups"),
            label = "Select groups to display:",
            choices = list_of_groups,
            selected = list_of_groups)
        )
      ),
      verticalLayout(
        ## OUTPUTS
         vegawidget::vegawidgetOutput(ns("scatterplot"), width = "auto"),
         textOutput(ns("listOfGenes"))
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
    sample_classes <- config$sample_categories
    
    coordinates_data <- module_config$coordinates_data
    fill <- module_config$annotation_column
    coordinates_data[[fill]] <- as.numeric(coordinates_data[[fill]]) 
    
    selected_lookup <- reactive({
      list_of_values <- getSelectedSampleCategories(sample_classes, input)
      selectMatchingValues(sample_lookup, list_of_values)
    })
   
    fc_from_lookup <- reactive({
      sel_lookup <- selected_lookup()
      subset_mat <- expression_matrix[, sel_lookup[[sample_var]]]
      if (ncol(subset_mat) == ncol(expression_matrix))
       return(list("all_samples", all_mean))
      subset_mean <- rowMeans(subset_mat, na.rm = T)
      #fc <- (subset_mean/all_mean) - 1
      #fc <- log((subset_mean/all_mean), 2)
      fc <- subset_mean - all_mean
      list("subset", fc)
    })
   
    scatterplot_data <- reactive({
      fc_list <- fc_from_lookup()
      fc <- fc_list[[2]]
      coordinates_data$Fold_Change <- fc[match(coordinates_data[[1]], names(fc))]
      coordinates_data$mean_type <- fc_list[[1]]
      coordinates_data %>%
       filter(.data[[fill]] %in% input$list_of_groups)
    })
   
    output$scatterplot <- renderVegawidget({
      df <- scatterplot_data()
      x <- colnames(df)[[2]]
      y <- colnames(df)[[3]]
      if (df$mean_type[[1]] == "all_samples") {
        plot_title <- 
          "Mean expression of all samples"
      } else plot_title <- 
        "Expression difference between mean of subgroup and mean of all samples"
      
      vega_scatterplot_overlay(
        data = df,
        x = x,
        y = y,
        color_var = fill,
        overlay_var = "Fold_Change",
        title = plot_title
      )
    })
   
    getSelectedCluster <-
      vw_shiny_get_signal(ns("scatterplot"),
                          name = "cluster_sel",
                          body_value = "value")

   # output$listOfGenes <- renderPrint({
   #   browser()
   #   selected_cluster <- getSelectedCluster() 
   #   if (!is.null(selected_cluster)) {
   #    df <- scatterplot_data()
   #    paste(df[df[[annotation_column]] == as.numeric(selected_cluster),
   #      1],sep = ",")
   #   }
   # })
   
    
  })
}