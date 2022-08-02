#' Create a scatterplot with x = clinical and y = expression
#'
#' @param df data frame
#' @param x horizontal axis variable
#' @param y vertical axis variable
#' @param facet_var list of variables for facet_wrap
#' @param gene_name gene name for labels
#' @param scales scales parameter. 'shared' for same scale in X axes and 'free'
#' for independent X axes.
#' @param color_variable categorical variable for color
#' @param manual_colors custom palette
#' @param ncol number of columns for facet_wrap
#' @param nrow number of rows for facet_wrap
#'
#' @return a ggplot object
#'
#'
#' @noRd
plotClinExpScatterplot <-
  function(df,
           x,
           y,
           facet_var = NULL,
           gene_name = NULL,
           scales = "free",
           color_variable = NULL,
           manual_colors = NULL,
           ncol = NULL,
           nrow = NULL) {
    p <-
      ggplot(na.omit(df), aes(x = .data[[x]], y = .data[[y]])) +
      coord_cartesian(clip = "off") +
      guides(color = guide_legend(override.aes = list(size = 5))) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.placement = "outside",
        panel.spacing.y = unit(10, "mm"),
        plot.margin = margin(20, 2, 2, 2, unit = "pt")
      ) +
      theme_bg()

    if (!is.null(color_variable)) {
      p <- p +
        geom_point(aes(fill = .data[[color_variable]]),
          color = "black",
          shape = 21,
          size = 2
        )
      if (is.null(manual_colors)) {
        p <- p + scale_fill_brewer(palette = "Set1")
      } else {
        p <- p + scale_fill_manual(values = manual_colors)
      }
    } else {
      p <- p + geom_point(size = 1)
    }

    if (!is.null(facet_var)) {
      p <- p + facet_wrap(facet_var,
        scales = scales,
        strip.position = "bottom",
        ncol = ncol,
        nrow = nrow
      )
    }
    if (!is.null(gene_name)) {
      p <- p + ylab(paste("Expression level of", gene_name, sep = " "))
    }
    p
  }

ggAddFit <- function(fit_method = c("linear", "quadratic", "cubic", "none")) {
  fit_method <- match.arg(fit_method)
  if (fit_method == "none") {
    return(NULL)
  }
  fit_formula <- switch(fit_method,
    linear = y ~ x,
    quadratic = y ~ splines::ns(x, df = 2),
    cubic = y ~ splines::ns(x, df = 3)
  )
  geom_smooth(
    formula = fit_formula,
    method = "lm",
    fullrange = TRUE
  )
}

ggAnnotateCorr <- function(correlation_df,
                           correlation_method,
                           vars = c(
                             "estimate",
                             "pvalue",
                             "padj"
                           )) {
  corr_labels <- c(
    "pearson" = "r:",
    "spearman" = "\u03c1:",
    "kendall" = "\u03C4:"
  )

  if (length(vars) == 2) {
    geom_text(
      data = correlation_df,
      aes(
        label = paste(
          corr_labels[correlation_method],
          signif(.data[[vars[1]]], 2),
          ", P: ",
          signif(.data[[vars[2]]], 2),
          sep = ""
        )
      ),
      x = -Inf,
      y = Inf,
      hjust = 0,
      vjust = -0.5,
      size = 12 / .pt,
      inherit.aes = FALSE
    )
  } else {
    geom_text(
      data = correlation_df,
      aes(
        label = paste(
          corr_labels[correlation_method],
          signif(.data[[vars[1]]], 2),
          ", P: ",
          signif(.data[[vars[2]]], 2),
          ", P_adj: ",
          signif(.data[[vars[3]]], 2),
          sep = ""
        )
      ),
      x = -Inf,
      y = Inf,
      hjust = 0,
      vjust = -0.5,
      size = 12 / .pt,
      inherit.aes = FALSE
    )
  }
}
