mod_allGenesScatterplot_ui <- function(module_name, config, module_config) {
  allGenesScatterplot_tab(
    #gene_select = geneSelectInput(NULL, module_name),
    sample_select = sampleCategoryInputs(config$sample_categories, module_name),
    id = module_name
  )
}

allGenesScatterplot_tab <- function(gene_select,
                                    sample_select,
                                    title = NULL,
                                    description = NULL,
                                    id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "2D Plot",
    value = "allGenesScatterplot",
    tags$h5(description %||%
              "Select a subset of samples and gene/protein for overlay"),
    splitLayout(
      verticalLayout(
        wellPanel(
          ## INPUTS
          #gene_select,
          sample_select
        )
      ),
      verticalLayout(
        ## OUTPUTS
        fluidRow(
          column(5,plotOutput(ns("scatterplot_coords"))),
          column(5,plotOutput(ns("scatterplot_overlay")))
        )
        ),
        cellWidths = c("20%", "80%"),
        cellArgs = list(style = "white-space: normal;")
    )
  )
}

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
    coordinates_data[[fill]] <- as.factor(coordinates_data[[fill]]) 
  
    # Load gene/proteins server-side
    # updateSelectizeInput(
    #   session,
    #   "selected_gene",
    #   choices = rownames(expression_matrix),
    #   selected = "",
    #   server = TRUE
    # )
    
   output$scatterplot_coords <- renderPlot({
     x <- colnames(coordinates_data)[[2]]
     y <- colnames(coordinates_data)[[3]]
     
     
     ggplot(coordinates_data,
            aes(.data[[x]], .data[[y]], colour = .data[[fill]])) +
       geom_point() + theme_transp_border() + 
       labs(title = "Projected data with cluster membership")
   })
   
   selected_lookup <- reactive({
     list_of_values <- getSelectedSampleCategories(sample_classes, input)
     selectMatchingValues(sample_lookup, list_of_values)
   })
   
   fc_from_lookup <- reactive({
     sel_lookup <- selected_lookup()
     subset_mat <- expression_matrix[, sel_lookup[[sample_var]]]
     if (ncol(subset_mat) == ncol(expression_matrix))
       return(all_mean)
     subset_mean <- rowMeans(subset_mat, na.rm = T)
     #fc <- (subset_mean/all_mean) - 1
     #fc <- log((subset_mean/all_mean), 2)
     fc <- subset_mean - all_mean
     fc
   })
   
   scatterplot_data <- reactive({
     fc <- fc_from_lookup()
     coordinates_data$fc <- fc[match(coordinates_data[[1]], names(fc))]
     coordinates_data
   })
   
   mid_rescaler <- function(mid = 0) {
     function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
       scales::rescale_mid(x, to, from, mid)
     }
   }
   
   output$scatterplot_overlay <- renderPlot({
     df <- scatterplot_data()
     x <- colnames(df)[[2]]
     y <- colnames(df)[[3]]
     
     ggplot(df, aes(.data[[x]], .data[[y]], colour = fc)) +
       geom_point() +
       scale_color_distiller(palette = "RdBu", rescaler = mid_rescaler(), 
                             label = function(x) sprintf("%.2f", x)) +
       #scale_color_gradient2(low="#2166ac",mid = "white", high = "#b2182b", midpoint = 0,limits = c(-1, 1)) +
       theme_transp_border() +
       labs(title = "Expression difference between mean of subgroup and mean of all samples")
   })
    
  })
}