mod_geneModulesHeatmap_ui <- function(module_name, config, module_config) {
  category_variable <- module_config$category_variable
  modules_table <- module_config$modules_table
  geneModulesHeatmap_tab(
    unique(modules_table[[category_variable]]),
    sampleCategoryInputs(
      config$sample_categories,
      module_name,
      module_config$subset_categories
    ),
    module_config$annotation_variables,
    module_config$title,
    module_config$description,
    module_name
  )
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
      description %||% "Select subsets and a module to view heatmap of genes"
    ),
    splitLayout(
      verticalLayout(
        wellPanel(
          radioButtons(
            ns("selected_category"),
            label = "Select category:",
            choices = categories,
            selected = categories[[1]]
          ),
          tags$hr(),
          tags$b("Sample selection"),
          sample_select,
          {
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
          }
        )
      ),
      fluidRow(
        ## OUTPUTS ,
        column(2, DTOutput(ns("modules_list"), width = "100%")),
        column(
          10,
          conditionalPanel(
            condition = "typeof input.modules_list_rows_selected != 'undefined'
              && input.modules_list_rows_selected.length != 0",
            ns = ns,
            verticalLayout(
              actionButton(ns("show_genes"), label = "View genes"),
              uiOutput(ns("heatmap_ui")),
              h5("Association of clinical variables with module eigengene"),
              vegawidget::vegawidgetOutput(ns("scatterplots"))
            )
          )
        )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}

#' @import iheatmapr
mod_geneModulesHeatmap_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    clinical <- config$data$clinical
    expression_matrix <- config$data$expression_matrix
    sample_lookup <- config$data$sample_lookup

    cores <- config$nthreads
    
    subject_var <- config$subject_variable
    sample_var <- config$sample_variable
    sample_categories <- config$sample_categories

    category_variable <- module_config$category_variable
    modules_table <- module_config$modules_table
    modules_variable <- module_config$modules_variable
    genes_variable <- module_config$genes_variable
    rank_variable <- module_config$rank_variable

    scatterplot_vars <- module_config$scatterplot_variables

    annotation_vars <- module_config$annotation_variables
    custom_annotation_colors <- module_config$custom_annotation_colors
    annotation_range <- module_config$annotation_range

    subset_categories <- module_config$subset_categories

    heatmap_palette <- module_config$custom_heatmap_palette

    modules_list_proxy <- DT::dataTableProxy("modules_list", session)

    user_selection <- reactive({
      getSelectedSampleCategories(sample_categories,
                                  input,
                                  subset_categories)
    })

    # Reset table selection if modules subset changes
    observeEvent(user_selection(), {
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
      if (!is.null(rank_variable)) {
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
      rownames = FALSE
    )
    outputOptions(output, "modules_list", suspendWhenHidden = TRUE)

    selected_lookup <- reactive({
      selectMatchingValues(sample_lookup, user_selection())
    })

    clinical_from_lookup <- eventReactive(selected_lookup(), {
      sel_lookup <- selected_lookup()
      selectFromLookup(clinical, sel_lookup,
        matching_col = subject_var
      )
    })

    annotations <- reactive({
      if (!isTruthy(input$selected_annotations)) {
        return(NULL)
      }
      selected_clinical <- clinical_from_lookup()
      selected_annot_df <- 
        selected_clinical[, c(rev(input$selected_annotations)), drop=FALSE] %>%
        select(where(function(x) !all(is.na(x))))
      if (ncol(selected_annot_df) == 0) return(NULL)
      
      return(selected_annot_df)
    })

    # Find module genes and subset expression matrix
    heatmap_data <- reactive({
      req(input$modules_list_row_last_clicked)
      row_id <- input$modules_list_row_last_clicked
      module_info <- as.list(selected_modules_table()[row_id, ])
      list_of_genes <- unlist(strsplit(module_info[[genes_variable]], ","))
      m <- expression_matrix[
        rownames(expression_matrix) %in% list_of_genes,
        selected_lookup()[[sample_var]]
      ]
      m <- m[rowSums(is.na(m)) != ncol(m), ]
      m
    }) %>% bindCache(
      input$modules_list_row_last_clicked,
      selected_modules_table(),
      selected_lookup()
    )

    # Modal dialog to show genes
    observeEvent(input$show_genes, {
      showModal(
        modalDialog(
          title = "Associated genes",
          pre(paste(rownames(heatmap_data()), collapse = ", "),
            style = "white-space: pre-wrap"
          )
        )
      )
    })

    observeEvent(heatmap_data(), {

      m <- heatmap_data()
      # Dynamic height
      hm_height <- min(600, 450 + nrow(m) * 2)

      output$heatmap_ui <- renderUI({
        iheatmaprOutput(ns("module_heatmap"), height = hm_height)
      })

      if (nrow(m) > 0) {
        output$module_heatmap <- renderIheatmap({
          row_labels <- ifelse(nrow(m) > 80, FALSE, TRUE)
          hm <- iheatmap(
            m,
            colors = rev(
              RColorBrewer::brewer.pal(11, heatmap_palette)
            ),
            row_labels = row_labels,
            scale = "rows",
            scale_method = "standardize",
            name = "Expression z-scores",
            layout = list(
              font = list(size = 9),
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent"
            )
          ) 
          hm <- hm %>% add_row_clustering()

          # Optional annotations
          annots <- annotations()
          if (!is.null(annots)) {
            hm <- hm %>%
              custom_add_col_annotations(annots,
                colors = custom_annotation_colors,
                range = annotation_range
              )
          }
          hm %>% add_col_clustering()
        })
      }
    })

    output$scatterplots <- vegawidget::renderVegawidget({
      req(heatmap_data())

      isolate({
        selected_lookup <-
          selectMatchingValues(sample_lookup, user_selection())
      })

      subset_clinical <- selectFromLookup(clinical, selected_lookup,
        matching_col = subject_var
      )
      selected_clinical <- subset_clinical[, scatterplot_vars]
      # Compute eigengene
      eigengene <- stats::prcomp(t(heatmap_data()),
        center = TRUE,
        scale = TRUE
      )[["x"]][, 1]

      combined_df <- cbind(
        Eigengene = eigengene,
        selected_clinical
      )
      
      corr_df <- longCorrelationMatrix(
        x = combined_df[, scatterplot_vars],
        y = combined_df[["Eigengene"]],
        adjust_method = NULL,
        name_to = "ClinicalVariable",
        cores = cores
      )
      
      # Reassign clinical variable names to preserver order from configuration
      corr_df$ClinicalVariable <- factor(corr_df$ClinicalVariable,
        levels = scatterplot_vars
      )
      combined_df <- combined_df %>%
        pivot_longer(c(-.data$Eigengene),
          names_to = "ClinicalVariable", values_to = "Value"
        )
      combined_df[["ClinicalVariable"]] <-
        factor(combined_df[["ClinicalVariable"]], levels = scatterplot_vars)
      
      corr_lookup <-
        paste0("{", paste(apply(corr_df, 1, function(x) {
          name <- x[["ClinicalVariable"]]
          corr <- round(as.numeric(x[["estimate"]]), digits = 2)
          pvalue <- round(as.numeric(x[["pvalue"]]), digits = 2)
          # glue::glue("'{name}': ['{name}', 'r: {corr}, p: {pvalue}, p_adj: {padj}']")
          paste0(
            "'", name, "': ['", name,
            "', 'r: ",
            corr, ", P: ", pvalue, "']"
          )
        }), collapse = ","), "}")
      
      vega_layer_scatterplot(combined_df,
        x = "Value",
        y = "Eigengene",
        facet_var = "ClinicalVariable",
        facet_sort = scatterplot_vars,
        label_lookup = corr_lookup,
        scales = "independent",
        gene_name = "eigengene",
        opts = list(width = 150, height = 120)
      ) %>%
        vega_add_fitline("linear") %>%
        vegawidget::as_vegaspec()
    })
  })
}
