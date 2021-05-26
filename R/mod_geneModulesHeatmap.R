mod_geneModulesHeatmap_ui <- function(module_name, appdata, global, module_config) {
  category_column <- module_config$category_column
  modules_table <- module_config$modules_table
  subset_classes <- module_config$subset_classes
  geneModulesHeatmap_tab(
    unique(modules_table[[category_column]]),
    sampleClassInputs(
      global$sample_classes,
      module_name,
      subset_classes
    ),
    module_config$title,
    module_name)
}

geneModulesHeatmap_tab <- function(categories,
                                   sample_select,
                                   title = NULL,
                                   module_name = NULL) {
  
  ns <- NS(module_name)
  tabPanel(
    title = "Modules heatmap",
    value = "geneModulesHeatmap",
    tags$h5(title %||% "Select subsets and a module to view heatmap of genes"),
    splitLayout(
      verticalLayout(
        wellPanel(
          radioButtons(
            ns("selected_category"),
            label = "Select category:",
            choices = categories,
            selected = categories[[1]]
          ),
          sample_select
        )
      ),
      fluidRow(
        ## OUTPUTS ,
        column(2, DT::DTOutput(ns("modules_list"), width = "100%")),
        column(8, 
          conditionalPanel(
            condition = "input.modules_list_rows_selected.length != 0",
            ns = ns,
            verticalLayout(
              actionButton(ns("show_genes"),label = "View genes"),
              uiOutput(ns("heatmap_ui")),
              h5("Association of clinical variables with module eigengene"),
              plotOutput(ns("scatterplots"))
            )
          ))
      ),
        cellWidths = c("20%", "80%"),
        cellArgs = list(style = "white-space: normal;")
      )
  )
}

#' @import iheatmapr
mod_geneModulesHeatmap_server <- function(module_name, appdata, global, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns
    
    clinical <- appdata$clinical
    expression_matrix <- appdata$expression_matrix
    sample_lookup <- appdata$sample_lookup
    
    subject_col <- global$subject_col
    sample_col <- global$sample_col
    sample_classes <- global$sample_classes
    
    category_column <- module_config$category_column
    modules_table <- module_config$modules_table
    modules_column <- module_config$modules_column
    genes_column <- module_config$genes_column
    rank_column <- module_config$rank_column
    scatterplot_vars <- module_config$scatterplot_variables
    
    # REST OF CODE HERE
    modules_list_proxy <- DT::dataTableProxy("modules_list", session)
    
    user_selection <- reactive({
      getSelectedSampleClasses(sample_classes, input)
    })
    
    observeEvent(user_selection(),{
      DT::selectRows(modules_list_proxy, NULL)
    })
    
    modules_table_selection <- reactive({
      selected_category <- input$selected_category
      c(user_selection(), stats::setNames(selected_category, category_column))
    })

    
    selected_modules_table <- reactive({
      table <- selectMatchingValues(modules_table, modules_table_selection())
      if (not_null(rank_column)) {
        table <- table[order(table[[rank_column]]), ]
      }
      table
    })
    
    output$modules_list <- DT::renderDT({
      as.data.frame(selected_modules_table()[, modules_column])
    },
    colnames = c("Select a module:"),
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      scrollY = "600px",
      scrollCollapse = TRUE
    ),
    class = "compact hover",
    selection = "single",
    rownames = FALSE)
    
    heatmap_data <-reactive({
      req(input$modules_list_row_last_clicked)
      row_id <- input$modules_list_row_last_clicked
      module_info <- as.list(selected_modules_table()[row_id, ])
      list_of_genes <- unlist(strsplit(module_info[[genes_column]], ","))
      selected_lookup <- 
        selectMatchingValues(sample_lookup, user_selection(), sample_col)
      expression_matrix[which(rownames(expression_matrix) %in% list_of_genes),
                        selected_lookup]
    })
    
    observeEvent(input$show_genes, {
      showModal(
        modalDialog(title = "Associated genes",
          pre(paste(rownames(heatmap_data()), collapse = ", "),
              style = "white-space: pre-wrap")
        )
      )
    })
    
    hm_height <-  reactive({ min(600, 450 + nrow(heatmap_data()) * 2) })
    
    output$module_heatmap <- renderIheatmap({
      req(nrow(heatmap_data()) > 0,cancelOutput = TRUE)
      iheatmap(heatmap_data(),
                          colors = rev(RColorBrewer::brewer.pal(3,"RdBu")),
                          row_labels = if (nrow(heatmap_data()) > 80) F else T,
                          scale = "rows",
                          scale_method = "standardize",
                          name = "Expression z-scores",
                          layout = list(font = list(size = 9)))  %>%
        add_row_clustering() %>% 
        add_col_clustering()
    })
    
    output$heatmap_ui <- renderUI({
      iheatmaprOutput(ns("module_heatmap"), height = hm_height())
    })
    
    output$scatterplots <- renderPlot({
      req(heatmap_data())
      selected_lookup <- 
        selectMatchingValues(sample_lookup, user_selection())
      subset_clinical <- selectFromLookup(clinical, selected_lookup,
                                          matching_col = subject_col)
      selected_clinical <- subset_clinical[, scatterplot_vars]
      eigengene <- stats::prcomp(t(heatmap_data()), center = T, scale = T)[["x"]][, 1]
      combined_df <- cbind(Eigengene = eigengene,
                             selected_clinical)
      corr_df <- correlateMatrices(x = combined_df[, scatterplot_vars],
                                   y = combined_df$Eigengene,
                                   rowname_var = "ClinicalVariable")
      corr_df[["ClinicalVariable"]] <- factor(corr_df[["ClinicalVariable"]],
                                              levels = scatterplot_vars)
      combined_df <- combined_df %>% 
        pivot_longer(c(-.data$Eigengene),
                     names_to = "ClinicalVariable", values_to = "Value")
      combined_df[["ClinicalVariable"]] <-
        factor(combined_df[["ClinicalVariable"]], levels = scatterplot_vars)
      
        plotClinExpScatterplot(combined_df,
                             x = "Value",
                             y = "Eigengene",
                             facet_var = "ClinicalVariable",
                             scales = "free",
                             gene_name = "eigengene", ncol = 4) +
        ggAnnotateCorr(corr_df, "pearson") +
        ggAddFit("linear")
      
    })
    
    
    
  })
}
