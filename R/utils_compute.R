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
    cor_mat <- cor(x, y, method, use = "pairwise.complete.obs")
    colnames(cor_mat) <- paste0(colnames(cor_mat), "_estimate")
    
    # Compute p-values
    pvalues_mat <- compute_pval(cor_mat, nrow(x))
    colnames(pvalues_mat) <- paste0(gsub("(.*)\\_estimate", "\\1",
                                         colnames(pvalues_mat)), "_pvalue")
    
    # Adjust p-values
    padjust_matrix <-
      apply(pvalues_mat, 2, p.adjust, method = adjust_method)
    colnames(padjust_matrix) <-
      paste0(gsub("(.*)\\_estimate", "\\1",
                  colnames(cor_mat)),
             "_padj")
    # Combine all and rename
    cor_mat <- cbind(cor_mat, pvalues_mat, padjust_matrix)
    cor_mat <-
      cbind("variable" = rownames(cor_mat), data.frame(cor_mat,
                                                       row.names = NULL))
    # Optionally rename first column
    if (!is.null(rowname_var))
      colnames(cor_mat)[1] <- rowname_var
    cor_mat
}

