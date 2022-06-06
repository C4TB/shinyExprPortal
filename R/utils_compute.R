#' Calculate the median for each column in subset of rows in matrix
#'
#' @param x a numeric \link[base]{matrix}.
#' @param rownames_list a subset of rows to operate over 
#'  (may contain missing rows).
#'
#' @return a matrix with columnwise medians or an empty matrix
#'   (if no entries in rownames_list match)
#' @noRd
colMediansSubset <- function(x, rownames_list = NULL) {
  
  if (is.null(rownames_list)) {
    res <- matrixStats::colMedians(x)
    names(res) <- colnames(x)
  }
  else {
    rownames_list <- intersect(rownames(x), rownames_list)
    if (length(rownames_list) == 1) {
      res <- x[unlist(rownames_list),]
      names(res) <- colnames(x)
    }
    else if (length(rownames_list) == 0) {
      # If none of the names are found, returns an empty matrix
      res <- x[0,]
    } else {
      x <- x[rownames_list, ]
      res <- matrixStats::colMedians(x)
      names(res) <- colnames(x)
    }
  }
  res
}

#' Compute p-values
#'
#' @param x vector x with correlation estimates
#' @param n number of comparisons
#'
#' @noRd
#' @return vector with p-values
#' @importFrom stats pt
compute_pval <- function(x, n) {
  if (!is.matrix(x)) x <- as.matrix(x)
  dof <- n - 2
  t <- (sqrt(dof) * abs(x)) / sqrt(1 - x^2)
  2* pt(t, dof, lower.tail = FALSE)
}

#' Compute correlations and p-values between matrices
#'
#' @param x a matrix or vector
#' @param y a matrix or vector
#' @param adjust_method Adjustmend method. If "q.value", calls method
#' [qvalue::qvalue()]. If not, calls [stats::p.adjust()]. Default is "q.value".
#' @param ... Additional arguments passed to [stats::cor()].
#'
#' @return a data frame with row names as values, estimate, pvalue and padjust
#'  columns.
#' @noRd
#'
#' @importFrom stats p.adjust cor
correlateMatrices <-
  function(x,
           y,
           adjust_method = "q.value",
           method = "pearson",
           rowname_var = NULL,
           colname_var = "var",
           ...) {
    if (is.vector(y))
      y <-  matrix(y, ncol = 1, dimnames = list(NULL, colname_var))
    
    # Compute correlations
    if (!requireNamespace("WGCNA", quietly = TRUE))
      cor_mat <- cor(x, y, method, use = "pairwise.complete.obs")
    else 
      cor_mat <- WGCNA::cor(x, y, method, use = "pairwise.complete.obs")
    colnames(cor_mat) <- paste0(colnames(cor_mat), "_estimate")
    
    # Compute p-values and append to correlation matrix
    pvalues_mat <- compute_pval(cor_mat, nrow(x))
    colnames(pvalues_mat) <- paste0(gsub("(.*)\\_estimate", "\\1",
                                         colnames(pvalues_mat)), "_pvalue")
    combined_mat <- cbind(cor_mat, pvalues_mat)
    
    # Adjust p-values and append to correlation matrix
    # Use apply to through each column
    # P values are adjusted based on number of genes, not number of genes x cols
    if (not_null(adjust_method)) {
      if (adjust_method == "q.value")
        padjust_matrix <-
          apply(pvalues_mat, 2, function(x) qvalue::qvalue(x)[["qvalues"]])
      else padjust_matrix <-
          apply(pvalues_mat, 2, p.adjust, method = adjust_method)
      colnames(padjust_matrix) <-
        paste0(gsub("(.*)\\_estimate", "\\1", colnames(cor_mat)), "_padj")
      combined_mat <- cbind(combined_mat, padjust_matrix)
    }
    # Append rownames as variable column
    combined_mat <-
      cbind("variable" = rownames(combined_mat),
            data.frame(combined_mat, row.names = NULL))
    # Optionally rename first column
    if (!is.null(rowname_var))
      colnames(combined_mat)[1] <- rowname_var
    combined_mat
}

longCorrelationMatrix <- function(first_col_name = "Gene",
                                  name_to = "ClinicalVariable",
                                  ...) {
  correlateMatrices(
    rowname_var = first_col_name,
    ...
  ) %>%
    pivot_longer(
      cols = -.data[[first_col_name]],
      names_to = c(name_to, ".value"),
      names_pattern = "(.*_*.*)_(estimate|pvalue|padj)"
    )
}

correlationResultsToLong <- function(data,
                                     first_col_name = "Gene",
                                     name_to = "ClinicalVariable") {
  pivot_longer(data,
               cols = c(-.data[[first_col_name]],-.data[["pvaluesrank"]]),
               names_to = c(name_to, ".value"),
               names_pattern = "(.*_*.*)_(estimate|pvalue|padj)")
}

corrResultsToTable <- 
  function(df, max_pvalue = 0.05, use_padj = F, rowname_col = "Gene") {
  # Get only correlation estimates
  selected_df <- df %>% dplyr::select(ends_with("estimate"))
  # Remove estimate suffix
  colnames(selected_df) <- gsub("(.*_)*(_estimate)", "\\1", 
                                colnames(selected_df))
  # Assign rowname and convert to matrix
  rownames(selected_df) <- df[[rowname_col]]
  cormat <- as.matrix(selected_df)
  
  # Get pvalues and match colnames with cormat
  suffix <- if (use_padj) "padj" else "pvalue"
  pval_df <- df %>% dplyr::select(ends_with(suffix))
  # Remove padj or pvalue suffix
  gsub_expr <- paste0("(.*_)*(_", suffix, "$)")
  colnames(pval_df) <- gsub(gsub_expr, "\\1", colnames(pval_df))
  # Assign rowname and convert to matrix
  rownames(pval_df) <- df[[rowname_col]]
  pmat <- as.matrix(pval_df)
  
  labels <- cormat
  labels <- signif(labels, 2)
  # Find significant to higlight in bold
  lv <- (pmat < max_pvalue) & !is.na(cormat) & !is.na(pmat)
  labels[lv] <-
    vapply(labels[lv], function(x) paste0("<b>",x,"</b>"), character(1))
  labels <- as.data.frame(labels)
  labels <- cbind(rownames(labels), labels)
  colnames(labels)[[1]] <- rowname_col
  rownames(labels) <- NULL
  labels
}