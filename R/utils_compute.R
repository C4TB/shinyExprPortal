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
  2 * pt(t, dof, lower.tail = FALSE)
}

#' Parallel computation of correlation of data frame vs matrix
#'
#' Uses mclapply so it won't run in parallel on Windows
#'
#' @param x numeric data frame
#' @param y matrix
#' @param method correlation method to use
#' @param cores number of cores to use with the [parallel::mclapply()] function
#'
#' @noRd
#' @return numeric matrix
fast_cor <- function(x,
                     y,
                     method = c("pearson", "spearman", "kendall"),
                     cores = 1) {
  method <- match.arg(method)

  spearman_func <- function(vector, matrix) {
    lv <- !is.na(vector)
    new_v <- vector[lv]
    as.vector(stats::cor(Rfast::Rank(new_v),
                         Rfast::colRanks(matrix[lv,],
                                         parallel = TRUE),
                         use = "pairwise.complete.obs"))
  }
  pearson_func <- function(vector, matrix) {
    lv <- !is.na(vector)
    new_v <- vector[lv]
    as.vector(stats::cor(new_v,
                         matrix[lv,],
                         use = "pairwise.complete.obs"))
  }
  kendall_func <- function(vector, matrix) {
    lv <- !is.na(vector)
    new_v <- vector[lv]
    as.vector(stats::cor(new_v,
                         matrix[lv,],
                         use = "pairwise.complete.obs",
                         method = "kendall"))
  }
  selected_fun <- switch(method,
                         pearson = pearson_func,
                         spearman = spearman_func,
                         kendall = kendall_func)
  cor_mat <- do.call(cbind, parallel::mclapply(x,
                                               FUN = selected_fun,
                                               matrix = y,
                                               mc.cores = cores))
  colnames(cor_mat) <- colnames(x)
  rownames(cor_mat) <- colnames(y)
  cor_mat
}

#' Compute correlations and p-values between matrices
#'
#' @param x a matrix or vector
#' @param y a matrix or vector
#' @param adjust_method Adjustmend method. If "q.value", calls method
#' [qvalue::qvalue()]. If not, calls [stats::p.adjust()]. Default is "q.value".
#' @param method Correlation method. Can be "pearson", "spearman" or "kendall".
#' @param rowname_var Optional variable name for first column
#' @param colname_var Optional column name for result
#' @param cores Optional number of cores for faster correlation.
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
           method = c("pearson", "spearman", "kendall"),
           rowname_var = NULL,
           colname_var = "var",
           cores = 1) {
    method <- match.arg(method)
    if (is.vector(x)) {
      x <- as.data.frame(x)
    }

    if (is.vector(y)) {
      y <- matrix(y, ncol = 1, dimnames = list(NULL, colname_var))
    }

    # Compute correlations
    cor_mat <- fast_cor(x, y, method, cores)
    colnames(cor_mat) <- paste0(colnames(cor_mat), "_estimate")

    # Compute p-values and append to correlation matrix
    pvalues_mat <- compute_pval(cor_mat, nrow(x))
    colnames(pvalues_mat) <- paste0(gsub(
      "(.*)\\_estimate", "\\1",
      colnames(pvalues_mat)
    ), "_pvalue")
    combined_mat <- cbind(cor_mat, pvalues_mat)

    # Adjust p-values and append to correlation matrix
    # Use apply to through each column
    # P values are adjusted based on number of genes, not number of genes x cols
    if (!is.null(adjust_method)) {
      if (adjust_method == "q.value") {
        padjust_matrix <-
          apply(pvalues_mat, 2, function(x) qvalue::qvalue(x)[["qvalues"]])
      } else {
        padjust_matrix <-
          apply(pvalues_mat, 2, p.adjust, method = adjust_method)
      }
      colnames(padjust_matrix) <-
        paste0(gsub("(.*)\\_estimate", "\\1", colnames(cor_mat)), "_padj")
      combined_mat <- cbind(combined_mat, padjust_matrix)
    }
    # Append rownames as variable column
    combined_mat <-
      cbind(
        "variable" = rownames(combined_mat),
        data.frame(combined_mat, row.names = NULL)
      )
    # Optionally rename first column
    if (!is.null(rowname_var)) {
      colnames(combined_mat)[1] <- rowname_var
    }
    combined_mat
  }

#' Compute correlation data frame in long format
#'
#' @param first_col_name name to set the first column (colnames of matrix)
#' @param name_to name of column for long format
#' @param ... arguments to [shinyExprPortal::correlateMatrices()]
#'
#' @noRd
#' @return long format data frame
longCorrelationMatrix <- function(first_col_name,
                                  name_to = "Measure",
                                  ...) {
  correlateMatrices(
    rowname_var = first_col_name,
    ...
  ) %>%
    pivot_longer(
      cols = -all_of(first_col_name),
      names_to = c(name_to, ".value"),
      names_pattern = "(.*_*.*)_(estimate|pvalue|padj)"
    )
}

#' Transform correlation data frame to long format
#'
#' @param data correlation data frame from
#' [shinyExprPortal::correlateMatrices()]
#' @param first_col_name name to set the first column (colnames of matrix)
#' @param name_to name of column for long format
#' @param pvalue_rank_col is there a pvaluesrank column?
#'
#' @noRd
#' @return long format data framew
correlationResultsToLong <- function(data,
                                     first_col_name ,
                                     name_to,
                                     pvalue_rank_col = FALSE) {
  if (pvalue_rank_col) {
    pivot_longer(data,
                 cols = -all_of(c(first_col_name, "pvaluesrank")),
                 names_to = c(name_to, ".value"),
                 names_pattern = "(.*_*.*)_(estimate|pvalue|padj)"
    )
  } else {
    pivot_longer(data,
                 cols = -all_of(first_col_name),
                 names_to = c(name_to, ".value"),
                 names_pattern = "(.*_*.*)_(estimate|pvalue|padj)"
    )
  }

}

#' Collapse data frame into table with highlights
#'
#'
#' @param df wide correlation data frame
#' @param max_pvalue p-value filter for highlight
#' @param use_padj flag for normal or adjusted p-value
#' @param rowname_col column that contains rownames
#'
#' @noRd
#' @return data frame with HTML b tags
corrResultsToTable <-
  function(df, max_pvalue = 0.05, use_padj = FALSE, rowname_col = "variable") {
    if (!rowname_col %in% colnames(df)) {
      stop("`rowname_col` not found in data frame")
    }
    # Get only correlation estimates
    selected_df <- df %>% dplyr::select(ends_with("estimate"))
    # Remove estimate suffix
    colnames(selected_df) <- gsub(
      "(.*_)*(_estimate)", "\\1",
      colnames(selected_df)
    )
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
      vapply(labels[lv], function(x) paste0("<b>", x, "</b>"), character(1))
    labels <- as.data.frame(labels)
    labels <- cbind(rownames(labels), labels)
    colnames(labels)[[1]] <- rowname_col
    rownames(labels) <- NULL
    labels
  }
