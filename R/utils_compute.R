#' Calculate the median for each column in subset of rows in matrix
#'
#' @param x a numeric \link[base]{matrix}.
#' @param rownames_list a subset of rows to operate over 
#'  (may contain missing rows).
#'
#' @return a matrix with columnwise medians or an empty matrix
#'   (if no entries in rownames_list match)
#' @importFrom matrixStats colMedians
#' @noRd
colMediansSubset <- function(x, rownames_list = NULL) {
  
  if (is.null(rownames_list)) {
    res <- colMedians(x)
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
      res <- colMedians(x)
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
#' @param adjust_method method passed to [stats::p.adjust()]. Default is "fdr".
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
           adjust_method = "fdr",
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
    
    # Compute p-values
    pvalues_mat <- compute_pval(cor_mat, nrow(x))
    colnames(pvalues_mat) <- paste0(gsub("(.*)\\_estimate", "\\1",
                                         colnames(pvalues_mat)), "_pvalue")
    
    # Adjust p-values
    if (not_null(adjust_method)) {
      padjust_matrix <-
        apply(pvalues_mat, 2, p.adjust, method = adjust_method)
      colnames(padjust_matrix) <-
        paste0(gsub("(.*)\\_estimate", "\\1",
                    colnames(cor_mat)),
               "_padj")
      cor_mat <- cbind(cor_mat, pvalues_mat, padjust_matrix)
    } else {
      cor_mat <- cbind(cor_mat, pvalues_mat)
    }
  
    cor_mat <-
      cbind("variable" = rownames(cor_mat), data.frame(cor_mat,
                                                       row.names = NULL))
    # Optionally rename first column
    if (!is.null(rowname_var))
      colnames(cor_mat)[1] <- rowname_var
    cor_mat
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

corrResultsToTable <- function(df, max_pvalue = 0) {
  selected_df <- df %>% dplyr::select(contains("estimate"))
  colnames(selected_df) <- gsub("(.*_)*(_estimate)", "\\1", 
                                colnames(selected_df))
  rownames(selected_df) <- df$Gene
  cormat <- as.matrix(selected_df)
  
  
  pval_df <- df %>% dplyr::select(contains("_pvalue")) 
  colnames(pval_df) <- gsub("(.*_)*(_pvalue)", "\\1", 
                            colnames(pval_df))
  rownames(pval_df) <- df$Gene
  pmat <- as.matrix(pval_df)
  
  labels <- cormat
  labels <- signif(labels, 2)
  # Find significant to higlight in bold
  lv <- pmat < max_pvalue &
    !is.na(cormat) & !is.na(pmat)
  labels[lv] <-
    vapply(labels[lv], function(x) paste0("<b>",x,"</b>"), character(1))
  labels <- as.data.frame(labels)
  labels <- cbind(Gene = rownames(labels), labels)
  rownames(labels) <- NULL
  labels
}