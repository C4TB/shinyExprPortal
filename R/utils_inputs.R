advanced_settings_inputs <- function(config, id = NULL) {
  ns <- NS(id)
  to_include <- list(
    if (isTruthy(config$measures_outliers)) {
      radioButtons(ns("measures_outliers"),
        label = "Remove measures outliers?",
        choices = c("5/95 percentiles", "IQR", "No"),
        selected = "No"
      )
    } else {
      NULL
    },
    if (isTruthy(config$expression_outliers)) {
      radioButtons(ns("expression_outliers"),
        label = "Remove expression outliers?",
        choices = c("5/95 percentiles", "IQR", "No"),
        selected = "No"
      )
    } else {
      NULL
    },
    if (isTruthy(config$correlation_method)) {
      radioButtons(ns("correlation_method"),
        label = "Correlation method:",
        choices = c(
          "Pearson" = "pearson",
          "Spearman" = "spearman",
          "Kendall" = "kendall"
        ),
        selected = "pearson"
      )
    } else {
      NULL
    },
    if (isTruthy(config$fit_method)) {
      radioButtons(ns("fit_method"),
        label = "Fitting method:",
        choices = c(
          "Linear" = "linear",
          "Quadratic" = "quadratic",
          "Cubic" = "cubic",
          if (config$fit_method == "AllowHide") c("None" = "none") else NULL
        )
      )
    } else {
      NULL
    }
  )
  if (!all(vapply(to_include, is.null, logical(1)))) {
    to_include <- c(list(tags$hr(), tags$b("Other options")), to_include)
  }
  do.call(tagList, list(to_include))
}


outlier_inputs <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("measures_outliers"),
      label = "Remove measures outliers?",
      choices = c("5/95 percentiles", "IQR", "No"),
      selected = "No"
    ),
    radioButtons(ns("expression_outliers"),
      label = "Remove expression outliers?",
      choices = c("5/95 percentiles", "IQR", "No"),
      selected = "No"
    )
  )
}

#' Create selectize input for gene list
#'
#' @param gene_list list of gene symbols
#' @param id module ID (optional)
#'
#' @return a selectize input element
#'
#' @importFrom data.table as.data.table
#' @noRd
geneSelectInput <- function(gene_list, id = NULL) {
  ns <- NS(id)
  selectizeInput(
    ns("selected_gene"),
    label = with_red_star("Select a gene:"),
    choices = as.data.table(gene_list),
    options = list(
      dropdownParent = "body",
      onInitialize = I("function(){this.setValue(''); }"),
      placeholder = ""
    )
  )
}

varsSelectInput <- function(clinical_vars, id = NULL, initEmpty = TRUE) {
  ns <- NS(id)

  onInitString <- NULL
  if (initEmpty)
    onInitString <- I("function(){this.setValue(''); }")

  selectizeInput(
    ns("selected_variable"),
    label = with_red_star("Select a measure:"),
    choices = clinical_vars,
    options = list(
      dropdownParent = "body",
      onInitialize = onInitString
    )
  )
}

#' Create radio buttons for sample selections
#'
#' @param sample_categories sample categories configuration
#' @param id module ID (optional)
#' @param subset_categories list of names sample categories to filter (optional)
#'
#' @return tag list with radio buttons
#' @noRd
sampleCategoryInputs <-
  function(sample_categories, id = NULL, subset_categories = NULL) {
    ns <- NS(id)
    if (!is.null(subset_categories)) {
      sc_logic <-
        as.logical(unlist(lapply(sample_categories, function(sc) {
          sc$name %in% subset_categories
        })))
      sample_categories <- sample_categories[sc_logic]
    }
    selection_tags <- lapply(
      sample_categories,
      function(sc) {
        radioButtons(ns(sc$name), paste(sc$label, "subset"), sc$values)
      }
    )
    do.call(tagList, selection_tags)
  }
