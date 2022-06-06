# degDetails UI Function
mod_degDetails_ui <- function(module_name, config, module_config) {
  if ("models" %in% names(config$data)) {
    models <- config$data$models
  } else {
    models <- module_config$models
  }
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
          ),
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
          plotly::plotlyOutput(ns("results_plot"),
            width = "700px",
            height = "500px"
          ),
          cellWidths = c(700, 200)
        ),
        hr(),
        verticalLayout(
          uiOutput(ns(
            "ui_table_checkbox"
          )),
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
mod_degDetails_server <- function(module_name, config, module_config) {
  moduleServer(module_name, function(input, output, session) {
    ns <- session$ns

    padj_col <- config$padj_col

    if ("models" %in% names(config$data)) {
      models <- config$data$models
    } else {
      models <- module_config$models
    }

    category_variable <- module_config$category_variable
    link_to <- module_config$link_to

    exc_columns <- c(
      category_variable,
      c("P", "P_adj", "File", "Data")
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
    model_results <- reactive({
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
      selected_model$Data[[1]]
    }) %>% bindCache(input$selected_model, input$model_category)

    signif_labels <- list(
      "not significant", "log FC",
      "%s", "log FC and %s"
    )
    # Volcano plot table
    vp_table <- reactive({
      table <- model_results()
      pcol <- if (input$use_padj) padj_col else "P.value"
      prepareModelResultsTable(
        table,
        input$fc_threshold,
        input$pvalue_threshold,
        pcol
      )
    })

    output$results_plot <- plotly::renderPlotly({
      table <- vp_table()

      gene_column <- {
        if ("Gene" %in% colnames(table)) "Gene" else "GeneSymbol"
      }
      pcol <- if (input$use_padj) padj_col else "P.value"
      table[[pcol]] <- -log10(table[[pcol]])
      if ("logFC" %in% colnames(table)) {
        plotly_volcano_plot(
          table,
          input$fc_threshold,
          -log10(input$pvalue_threshold),
          pcol,
          gene_column
        )
      } else {
        plotly_avgexpr_plot(
          table,
          -log10(input$pvalue_threshold),
          pcol,
          gene_column
        )
      }
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
        choices = levels(as.factor(vp_table()[["color"]])),
        selected = levels(as.factor(vp_table()[["color"]]))
      )
    })

    output$deg_table <- renderDT({
        req(input$deg_table_checkbox)
        model_table <- vp_table()
        gene_column <- {
          if ("Gene" %in% colnames(model_table)) "Gene" else "GeneSymbol"
        }
        # Optional link
        if (not_null(link_to)) {
          gene_col_id <- which(colnames(model_table) == gene_column)
          model_table[[gene_col_id]] <- urlVector(
            model_table[[gene_col_id]],
            "gene",
            isolate({
              current_URL()
            })
          )
        }
        view_cols <-
          c(
            "EnsemblID", "Gene", "GeneSymbol", "Protein",
            "logFC", "AveExpr", "P.value", padj_col
          )
        model_table[model_table$color %in% input$deg_table_checkbox, ] %>%
          dplyr::select(any_of(view_cols))
      },
      filter = "top",
      escape = FALSE,
      rownames = FALSE,
      options = list(scrollX = TRUE)
    )
  })
}
