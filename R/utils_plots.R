#' Create a scatterplot with x = clinical and y = expression
#'
#' @param df data frame
#' @param x horizontal axis variable
#' @param y vertical axis variable
#' @param facet_var list of variables for facet_wrap
#' @param gene_name gene name for labels
#' @param scales scales parameter
#' @param colour_variable categorical variable for color
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
           colour_variable = NULL,
           ncol = NULL,
           nrow = NULL) {
  
  p <-
    ggplot(na.omit(df), aes(x = .data[[x]], y = .data[[y]])) +
    coord_cartesian(clip = "off") +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
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
  
  if (!is.null(colour_variable))
    p <- p +
      geom_point(aes(color = .data[[colour_variable]]), size = 1) +
      scale_color_brewer(palette = "Set1") 
  else
    p <- p + geom_point(size = 1)
  
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(facet_var,
               scales = scales,
               strip.position = "bottom",
               ncol = ncol,
               nrow = nrow
              )
  }
  if (!is.null(gene_name))
    p <- p + ylab(paste("Expression level of", gene_name, sep = " "))
  p
}

ggAddFit <- function(fit_method = c("linear", "quadratic", "cubic", "none")) {
  fit_method <- match.arg(fit_method)
  if (fit_method == "none") return(NULL)
  fit_formula <- switch(fit_method,
                    linear = y ~ x,
                    quadratic = y ~ splines::ns(x, df = 2),
                    cubic = y ~ splines::ns(x, df = 3))
  geom_smooth(formula = fit_formula,
              method = "lm",
              fullrange = TRUE)
}

ggAnnotateCorr <- function(correlation_df,
                           correlation_method,
                           vars = c("estimate",
                                    "pvalue",
                                    "padj")) {
  corr_labels <- c("pearson" = "r:",
                   "spearman" = "\u03c1:",
                   "kendall" = "\u03C4:")
  
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
  } else
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

#' @noRd
plotModulesOverview <- function(overview_data, across_class) {
  across_formula <- stats::as.formula(paste("~", across_class))
  
  overview_data %>%
    plotly::plot_ly(source="overview", key = ~Modules) %>%
    plotly::add_markers(
      x = ~ jittered,
      y = ~ Median_Expression,
      color = across_formula,
      marker = list(size = 6, opacity = 0.5),
      colors = "Set1",
      hoverinfo = "text",
      text = ~ paste0("Module: ", Modules,
                      "<br>Median Expression: ", signif(Median_Expression, 4))
    ) %>% 
    plotly::layout(
      yaxis = list (title = "Median Expression"),
      xaxis = list(title = across_class,
                   showticklabels = FALSE)
    ) %>% htmlwidgets::onRender(
      "
        function(el, x) {
          el.on('plotly_hover', function(d) {
          Plotly.d3.select('.cursor-crosshair').style('cursor', 'pointer');
          });
          el.on('plotly_unhover', function(d) {
          Plotly.d3.select('.cursor-crosshair').style('cursor', 'crosshair');
          });
        }
    ")
}

plotModuleProfile <- function(module_profile, expression_col, sample_col,
                              across_class, plot_title = "") {
  module_exp <- module_profile[, expression_col]
  y_pos <- sum(module_exp) / length(module_exp)
  ggplot(module_profile, aes(x = .data[[sample_col]],
                             y = .data[[expression_col]])) +
    geom_tile(height = Inf,
              aes(
                x = .data[[sample_col]],
                y = y_pos,
                fill = as.factor(.data[[across_class]])
              )) +
    geom_line(aes(group = 1)) +
    scale_fill_brewer(palette = "Set1") +
    theme(
      plot.title = element_text(
        lineheight = 0.8,
        face = "bold",
        colour = "black",
        size = 15
      ),
      axis.title = element_text(
        face = "bold",
        colour = "black",
        size = 15
      ),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    ggtitle(paste(strwrap(plot_title, 50), collapse = "\n"))
}