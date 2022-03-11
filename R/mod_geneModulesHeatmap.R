mod_geneModulesHeatmap_ui <- function(module_name, appdata, global, module_config) {
  category_variable <- module_config$category_variable
  modules_table <- module_config$modules_table
  subset_classes <- module_config$subset_classes
  geneModulesHeatmap_tab(
    unique(modules_table[[category_variable]]),
    sampleClassInputs(
      global$sample_classes,
      module_name,
      subset_classes
    ),
    module_config$annotation_variables,
    module_config$title,
    module_config$description,
    module_name)
}

geneModulesHeatmap_tab <- function(categories,
                                   sample_select,
                                   annotation_variables,
                                   title = NULL,
                                   description = NULL,
                                   module_name = NULL) {
  
  ns <- NS(module_name)
  tabPanel(
    title = title %||% "Modules heatmap",
    value = "geneModulesHeatmap",
    tags$h5(
      description %||% "Select subsets and a module to view heatmap of genes"),
    splitLayout(
      verticalLayout(
        wellPanel(
          radioButtons(
            ns("selected_category"),
            label = "Select category:",
            choices = categories,
            selected = categories[[1]]
          ),
          sample_select,
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
      fluidRow(
        ## OUTPUTS ,
        column(2, DTOutput(ns("modules_list"), width = "100%")),
        column(10, 
          conditionalPanel(
            condition = "typeof input.modules_list_rows_selected != 'undefined'
              && input.modules_list_rows_selected.length != 0",
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
    
    subject_var <- global$subject_variable
    sample_var <- global$sample_variable
    sample_classes <- global$sample_classes
    
    category_variable <- module_config$category_variable
    modules_table <- module_config$modules_table
    modules_variable <- module_config$modules_variable
    genes_variable <- module_config$genes_variable
    rank_variable <- module_config$rank_variable
    scatterplot_vars <- module_config$scatterplot_variables
    annotation_vars <- module_config$annotation_variables
    annotation_colors <- module_config$annotation_colours %||% NULL
    annotation_range <- module_config$annotation_range %||% NULL
    
   
    modules_list_proxy <- DT::dataTableProxy("modules_list", session)
    
    user_selection <- reactive({
      getSelectedSampleClasses(sample_classes, input)
    })
    
    # Reset table selection if modules subset changes
    observeEvent(user_selection(),{
      DT::selectRows(modules_list_proxy, NULL)
    })
    
    # Combine modules category with selection
    modules_table_selection <- reactive({
      selected_category <- input$selected_category
      c(user_selection(), stats::setNames(selected_category, category_variable))
    })
    
    # Find list of modules
    selected_modules_table <- reactive({
      table <- selectMatchingValues(modules_table, modules_table_selection())
      if (not_null(rank_variable)) {
        table <- table[order(table[[rank_variable]]), ]
      }
      table
    })
    
    output$modules_list <- renderDT({
        as.data.frame(selected_modules_table()[, modules_variable])
      },
      colnames = c("Select a module:"),
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        scrollY = "600px",
        scrollCollapse = TRUE
      ),
      filter = "top",
      class = "compact hover",
      selection = "single",
      rownames = FALSE)
    outputOptions(output, "modules_list" ,suspendWhenHidden = TRUE)
    
    selected_lookup <- reactive({
      selectMatchingValues(sample_lookup, user_selection())
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
    
    # Find module genes and subset expression matrix
    heatmap_data <-reactive({
      req(input$modules_list_row_last_clicked)
      row_id <- input$modules_list_row_last_clicked
      module_info <- as.list(selected_modules_table()[row_id, ])
      list_of_genes <- unlist(strsplit(module_info[[genes_variable]], ","))
      expression_matrix[rownames(expression_matrix) %in% list_of_genes,
                        selected_lookup()[[sample_var]]]
    })
    
    # Modal dialog to show genes
    observeEvent(input$show_genes, {
      showModal(
        modalDialog(title = "Associated genes",
          pre(paste(rownames(heatmap_data()), collapse = ", "),
              style = "white-space: pre-wrap")
        )
      )
    })
    
    # Dynamic height
    hm_height <-  reactive({ min(600, 450 + nrow(heatmap_data()) * 2) })
    
    output$module_heatmap <- renderIheatmap({
      req(nrow(heatmap_data()) > 0, cancelOutput = TRUE)
      hm <- iheatmap(
              heatmap_data(),
              colors = rev(
                RColorBrewer::brewer.pal(11, "RdBu")
              ),
              row_labels = if (nrow(heatmap_data()) > 80) F else T,
              scale = "rows",
              scale_method = "standardize",
              name = "Expression z-scores",
              layout = list(font = list(size = 9),
                            plot_bgcolor = "transparent",
                            paper_bgcolor = "transparent"))  %>%
        add_row_clustering()
      
      # Optional annotations
      annots <- annotations()
      if (not_null(annots)) {
        hm <- hm %>% 
          custom_add_col_annotations(annots,
                             colors = annotation_colors,
                             range = annotation_range)
      }
      hm %>% add_col_clustering()
    })
    
    output$heatmap_ui <- renderUI({
      iheatmaprOutput(ns("module_heatmap"), height = hm_height())
    })
    
    output$scatterplots <- renderPlot({
      req(heatmap_data())
      selected_lookup <- 
        selectMatchingValues(sample_lookup, user_selection())
      subset_clinical <- selectFromLookup(clinical, selected_lookup,
                                          matching_col = subject_var)
      selected_clinical <- subset_clinical[, scatterplot_vars]
      
      # Compute eigengene
      eigengene <- stats::prcomp(t(heatmap_data()),
                                 center = T,
                                 scale = T)[["x"]][, 1]
      
      combined_df <- cbind(Eigengene = eigengene,
                             selected_clinical)
      
      corr_df <- correlateMatrices(x = combined_df[, scatterplot_vars],
                                   y = combined_df$Eigengene,
                                   adjust_method = NULL,
                                   rowname_var = "ClinicalVariable")
      # Reassign clinical variable names to preserver order from configuration
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
        ggAnnotateCorr(corr_df,
                       "pearson",
                       c("var_estimate", "var_pvalue")) +
        ggAddFit("linear")
      
    },height = ceiling(length(scatterplot_vars)/4) * 200, bg = "transparent")
    
    
    
  })
}
