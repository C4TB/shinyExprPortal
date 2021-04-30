advanced_settings_inputs <- function(config, id = NULL) {
  ns <- NS(id)
  to_include <- list(
    if (isTruthy(config$clinical_outliers)) { 
      
      radioButtons(ns("clinical_outliers"),
                   label = "Remove clinical outliers?",
                   choices = c("5/95 percentiles", "IQR", "No"),
                   selected = "No")
    } else NULL,
    if (isTruthy(config$expression_outliers)) {
      radioButtons(ns("expression_outliers"),
                   label = "Remove expression outliers?",
                   choices = c("5/95 percentiles", "IQR", "No"),
                   selected = "No")
    } else NULL,
    if (isTruthy(config$correlation_method)) {
      radioButtons(ns("correlation_method"),
                   label = "Correlation method:",
                   choices = c("Pearson" = "pearson",
                               "Spearman" = "spearman",
                               "Kendall" = "kendall"),
                   selected = "pearson")
    } else NULL,
    if (isTruthy(config$fit_method)) {
      radioButtons(ns("fit_method"),
                   label = "Fitting method:",
                   choices = c("Linear" = "linear",
                               "Cubic" = "cubic"))
    }
  )
  do.call(tagList, list(to_include))
}


outlier_inputs <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("clinical_outliers"),
               label = "Remove clinical outliers?",
               choices = c("5/95 percentiles", "IQR", "No"),
               selected = "No"),
   radioButtons(ns("expression_outliers"),
               label = "Remove expression outliers?",
               choices = c("5/95 percentiles", "IQR", "No"),
               selected = "No")
   )
}

#' Add radio buttons 
#
#' @param inputId the inputID
#' @param label label for the radio button
#' @param choices choices
#'
#' @return radioButtons
#' @noRd
addRadioInputUI <- function(inputId, label, choices) {
    radioButtons(inputId, label, choices)
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
      onInitialize = I('function(){this.setValue(""); }'),
      placeholder = ''
    )
  )
}

#' Create radio buttons for sample selections
#'
#' @param sample_classes sample classes configuration
#' @param id module ID (optional)
#' @param match_name list of names sample classes to filter (optional)
#'
#' @return tag list with radio buttons
#' @noRd
sampleClassInputs <- function(sample_classes, id = NULL, match_name = NULL) {
  ns <- NS(id)
  
  if (!is.null(match_name)) {
    sc_logic <-
      as.logical(unlist(lapply(sample_classes, function(sc)
        sc$name %in% match_name)))
    sample_classes <- sample_classes[which(sc_logic)]
  }
  do.call(tagList,
          lapply(sample_classes, function(sc, ns)
            radioButtons(ns(sc$name), sc$label, sc$values), ns = ns)
  )
}