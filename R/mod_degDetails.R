# degDetails UI Function
mod_degDetails_ui <-
  function(module_name, config, module_config, parent_config = NULL) {
    models <- parent_config$models %||% module_config$models
    category_variable <- module_config$category_variable
    categories <- unique(unlist(models[, category_variable]))
    degDetails_tab(
      categories,
      module_config$title,
      module_config$description,
      module_name
    )
}
#' Differentially expressed genes tab UI
#'
#' @param categories model categories
#' @param title optional title
#' @param description optional description
#' @param id optional module ID
#'
#' @return tab panel with inputs
#' @noRd
#'
degDetails_tab <- function(categories,
                           title = NULL,
                           description = NULL,
                           id = NULL) {
  ns <- NS(id)
  tabPanel(
    title = title %||% "Model results",
    value = "degDetails",
    tags$h5(description %||% "Individual model results"),
    splitLayout(
      verticalLayout(
        wellPanel(
          radioButtons(
            ns("model_category"),
            label = "Select category:",
            choices = categories,
            selected = categories[[1]]
          ) %>%
            shinyhelper::helper(content = "degDetails", size = "l"),
          radioButtons(
            ns("selected_model"),
            label = "Select model:",
            choices = c("1")
          ),
          tags$hr(),
          tags$b("Plot options"),
          numericInput(
            ns("fc_threshold"),
            label = "FC threshold:",
            value = 1,
            min = 0.5,
            max = 10,
            step = 0.25
          ),
          numericInput(
            ns("pvalue_threshold"),
            label = "Significance threshold:",
            value = 0.05,
            min = 0,
            max = 1,
            step = 0.01
          ),
          checkboxInput(ns("use_padj"), "Use adjusted p-value?")
        )
      ),
      verticalLayout(
        splitLayout(
          style = "font-size: 75%;",
          vegawidget::vegawidgetOutput(ns("results_plot"),
            width = "700px"
          ),
          verticalLayout(
            radioButtons(
              ns("label_method"),
              label = "Gene labels:",
              choices = list("Significant only" = "signif",
                             "From list" = "list")
            ),
            textAreaInput(
              ns("highlight_genes"),
              label = "Comma-separated list of gene symbols to highlight:",
              cols = 80,
              rows = 5
            )
          ),
          cellWidths = c("50%", "50%")
        ),
        hr(),
        verticalLayout(
          flowLayout(uiOutput(ns(
            "ui_table_checkbox"
          )), downloadButton(ns("download_deg_table"), "Download table")),
          DT::DTOutput(ns("deg_table"))
        )
      ),
      cellWidths = c("20%", "80%"),
      cellArgs = list(style = "white-space: normal;")
    )
  )
}
#' degDetails Server Function
#'
#' @noRd
mod_degDetails_server <-
  function(module_name, config, module_config, parent_config = NULL) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    valid_symbol_cols <-
      c("Gene", "GeneSymbol", "Symbol", "symbol", "Gene_ID", "Protein")

    pvalue_col <- module_config$pvalue_col
    padj_col <- module_config$padj_col
    custom_point_colors <- module_config$custom_point_colors
    models <- parent_config$models %||% module_config$models

    category_variable <- module_config$category_variable
    link_to <- module_config$link_to

    exc_columns <- c(
      category_variable,
      c("P", "P_adj", "File", "Data", "ModelFileType")
    )
    table_subset <- dplyr::select(models, -exc_columns)
    model_update <- reactiveVal(FALSE)

    # Update list of models after a category is selected
    observeEvent(input$model_category, {
      selected_category_models <- models[
        models[, category_variable] == input$model_category,
      ] %>%
        dplyr::select(-exc_columns)
      model_update(TRUE)
      updateRadioButtons(
        session,
        "selected_model",
        choiceNames = do.call(paste, c(selected_category_models)),
        choiceValues = do.call(paste, c(selected_category_models, sep = "_"))
      )
    })

    observeEvent(input$selected_model, {
      model_update(TRUE)
    })

    # Combine selected model category with model
    # Model is a string separated by "_" that is split into a vector
    condition_list <- eventReactive(c(model_update(), input$selected_model), {
      model_update(FALSE)
      isolate({
        condition <- stats::setNames(
          unlist(strsplit(input$selected_model, "_")),
          colnames(table_subset)
        )
        condition[[category_variable]] <- input$model_category
      })
      condition
    })

    # Retrieve results by matching the columns from the table
    selected_model_r <- reactive({
      condition <- condition_list()
      model_res <- list()
      # Iterate through multiple conditions to match rows
      for (var_name in names(condition)) {
        model_cond_res <- models[models[, var_name] == condition[var_name], ]
        model_res[[var_name]] <- model_cond_res
      }
      selected_model <- Reduce(
        function(x, y) inner_join(x, y, by = colnames(x)), model_res
      )
      req(nrow(selected_model) > 0)
      selected_model
    }) %>% bindCache(input$selected_model, input$model_category)

    model_file_type <- reactive({
      selected_model <- selected_model_r()
      selected_model$ModelFileType[[1]]
    })

    model_results <- reactive({
      selected_model <- selected_model_r()
      selected_model$Data[[1]]
    })

    signif_labels <- list(
      "not significant", "log FC",
      "%s", "log FC and %s"
    )

    fc_threshold_d <- debounce(reactive({ input$fc_threshold }), 500)
    pvalue_threshold_d <- debounce(reactive({ input$pvalue_threshold }), 500)

    # Volcano plot table
    # Change table when user changes:
    # - selected model
    # - FC significance threhold
    # - p value significance threshold
    # - use p-value or adjusted p-value
    vp_table <- reactive({
      table <- model_results()
      pcol <- if (input$use_padj) padj_col else pvalue_col
      prepareModelResultsTable(
        table,
        fc_threshold_d(),
        pvalue_threshold_d(),
        pcol
      )
    }) %>%
      bindCache(model_results(),
                fc_threshold_d(),
                pvalue_threshold_d(),
                input$use_padj)

    # Change label displayed in volcano plot
    # Use fully significant only or from list of genes
    gene_list <- reactive({
      if (input$label_method == "list") {
        as.list(vapply(strsplit(input$highlight_genes, ",")[[1]],
                       character(1),
                       FUN = function(x) toupper(trimws(x))))
      } else {
        NULL
      }
    }) %>% debounce(500)

    output$results_plot <- vegawidget::renderVegawidget({
      table <- vp_table()

      valid_log_cols <- c("logFC", "log2FoldChange")

      validate(need(
        intersect(valid_log_cols, colnames(table)) > 0,
        "Volcano plot can't be displayed for selected model results"
      ))

      gene_column <- intersect(colnames(table), valid_symbol_cols)[[1]]
      pcol <- if (input$use_padj) padj_col else pvalue_col
      vega_volcanoplot(
        data = table,
        fc_min = fc_threshold_d(),
        pvalue_min = pvalue_threshold_d(),
        pvalue_col = pcol,
        gene_col = gene_column,
        colors = custom_point_colors,
        gene_list = gene_list()
        ) %>%
        vegawidget::as_vegaspec()
    })

    current_URL <- reactive({
      conditions <- condition_list()
      # Last condition is the model name/category so we remove it
      conditions <- conditions[-length(conditions)]
      buildURL(conditions, paste0("?tab=", link_to))
    })

    # Create checkbox from color column
    output$ui_table_checkbox <- renderUI({
      req(vp_table())
      checkboxGroupInput(ns("deg_table_checkbox"),
        label = "Filter genes:",
        choices = levels(as.factor(vp_table()[["signif_label"]])),
        selected = levels(as.factor(vp_table()[["signif_label"]]))
      )
    })

    output$deg_table <- renderDT({
        req(input$deg_table_checkbox)
        model_table <- vp_table()
        gene_column <- intersect(colnames(model_table), valid_symbol_cols)[[1]]
        # Optional link
        if (!is.null(link_to)) {
          gene_col_id <- which(colnames(model_table) == gene_column)
          model_table[[gene_col_id]] <- urlVector(
            model_table[[gene_col_id]],
            "gene",
            isolate({
              current_URL()
            })
          )
        }
        # Print only the columns from the results file
        view_cols <- setdiff(colnames(model_table),
                c("pvalue_signif", "fc_signif", "signif", "signif_label"))
        model_table[model_table$signif_label %in% input$deg_table_checkbox,
                    view_cols]
      },
      filter = "top",
      escape = FALSE,
      rownames = FALSE,
      options = list(dom = "lrtip", scrollX = TRUE)
    )

    output$download_deg_table <- downloadHandler(
      filename = function() paste0(input$selected_model, ".csv"),
      content = function(file) {
        model_table <- vp_table()
        gene_column <- intersect(colnames(model_table), valid_symbol_cols)[[1]]
        view_cols <- setdiff(colnames(model_table),
                   c("pvalue_signif", "fc_signif", "signif", "signif_label"))
        utils::write.csv(
          model_table[model_table$signif_label %in% input$deg_table_checkbox,
                                      view_cols], file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
  })
}
