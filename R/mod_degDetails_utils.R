plotly_volcano_plot <- function(table,
                                fc_threshold,
                                pvalue_threshold,
                                adjusted = "q.value",
                                gene_column = "Gene") {
  to_form <- function(x) as.formula(paste0("~", x))
  
  signif_labels_colors <-
    c(
      "not significant" = "gray",
      "log FC" = "darkgreen",
      "p-value" = "blue",
      "q-value" = "blue",
      "log FC and p-value" = "red",
      "log FC and q-value" = "red"
    )
  
  if (adjusted == "q.value") {
    ylab_text <- "-log10 q"
  } else {
    ylab_text <- "-log10 p"
  }
  
  lines <- list(
    list(type = "line",
         line = list(width = 1, color = "#AAAAAA", dash = "dash"),
         x0 = 0, x1 = 1, y0 = pvalue_threshold, y1 = pvalue_threshold,
         yref = "y", xref="paper"),
    list(type = "line",
         line = list(width = 1, color = "#AAAAAA",  dash = "dash"),
         y0 = 0, y1 = 1, x0 = fc_threshold, x1 = fc_threshold,
         xref="x", yref = "paper"),
    list(type = "line",
         line = list(width = 1, color = "#AAAAAA", dash = "dash"),
         y0 = 0, y1 = 1, x0 = -fc_threshold, x1 = -fc_threshold,
         xref="x", yref = "paper")
  )
  
  logfc_max <- max(abs(table[["logFC"]]))
  xaxis_max <- max(logfc_max, fc_threshold + 0.25)
  
  ylayout_axis <- list(title = ylab_text, showgrid = FALSE, color = "black",
                       ticklen = 5, showline = TRUE, zeroline = F)
  xlayout_axis <- list(title = "logFC",showgrid = FALSE, color = "black",
                       ticklen = 5, showline = TRUE, zeroline = F, 
                       range = c(-xaxis_max, xaxis_max))
  
  plotly::plot_ly(table, x = ~logFC, y = to_form(adjusted), color = ~color, 
           colors = signif_labels_colors, type = "scattergl", mode = "markers",
           marker=list(size = 8, opacity = 0.6),
           text = table[[1]], hoverinfo = "text",
           key = table[[1]], source = "volcano_plot") %>%
    plotly::layout(xaxis = xlayout_axis,
                   yaxis = ylayout_axis,
                   shapes = lines,
                   plot_bgcolor = "transparent",
                   paper_bgcolor = "transparent") %>%
    plotly::config(toImageButtonOptions = list(format = "svg"))
}

gg_volcano_plot <- function(table,
                            fc_threshold,
                            pvalue_threshold,
                            adjusted = "q.value",
                            gene_column = "Gene") {
  max_x_data <- max(abs(min(table$logFC)), max(table$logFC))
  data_fc_lim <- max(max_x_data, fc_threshold)
  signif_labels_colors <-
    c(
      "not significant" = "gray",
      "log FC" = "darkgreen",
      "p-value" = "blue",
      "q-value" = "blue",
      "log FC and p-value" = "red",
      "log FC and q-value" = "red"
    )
  
  if (adjusted == "q.value") {
    ylab_text <- "-log10 q"
  } else {
    ylab_text <- "-log10 p"
  }
  p <-
    ggplot(table,
           aes(
             .data$logFC,
             .data[[adjusted]],
             color = .data$color,
           )) +
    geom_point() +
    ylab(ylab_text) + 
    xlab("Log fold change") +
    geom_text(aes(label = .data[[1]]),
              data = table[table$signif == 3,],
              vjust = "top",
              hjust = "right") +
    scale_y_continuous(limits = c(0, NA)) +
    xlim(-data_fc_lim, data_fc_lim) +
    scale_color_manual(values = signif_labels_colors) +
    geom_vline(xintercept = -fc_threshold) + 
    geom_vline(xintercept = fc_threshold) +
    geom_hline(yintercept = pvalue_threshold) +
    theme_classic() + 
    theme(legend.title = element_blank())
  p  
}

plotly_avgexpr_plot <- function(table,
                                pvalue_threshold,
                                adjusted = "q.value",
                                gene_column = "Gene") {
  to_form <- function(x) as.formula(paste0("~", x))
  max_x_data <- max(abs(min(table$AvgExpr)), max(table$AvgExpr))
  signif_labels_colors <-
    c(
      "not significant" = "gray",
      "log FC" = "darkgreen",
      "p-value" = "blue",
      "q-value" = "blue",
      "log FC and p-value" = "red",
      "log FC and q-value" = "red"
    )
  if (adjusted == "q.value") {
    ylab_text <- "-log10 q"
  } else {
    ylab_text <- "-log10 p"
  }
  lines <- list(
    list(type = "line",
         line = list(width = 1, color = "#AAAAAA", dash = "dash"),
         x0 = 0, x1 = 1, y0 = pvalue_threshold, y1 = pvalue_threshold,
         yref = "y", xref="paper")
  )
  ylayout_axis <- list(title = ylab_text, showgrid = FALSE, color = "black",
                       ticklen = 5, showline = TRUE, zeroline = F)
  xlayout_axis <- list(showgrid = FALSE, color = "black",
                       ticklen = 5, showline = TRUE, zeroline = F, 
                       range = c(-max_x_data, max_x_data))
  plotly::plot_ly(table, x = ~AvgExpr, y = to_form(adjusted), color = ~color, 
                  colors = signif_labels_colors, type = "scattergl", mode = "markers",
                  marker=list(size = 8, opacity = 0.6),
                  text = table[[1]], hoverinfo = "text",
                  key = table[[1]], source = "avgexpr_plot") %>%
    plotly::layout(xaxis = xlayout_axis,
                   yaxis = ylayout_axis,
                   shapes = lines,
                   plot_bgcolor = "transparent",
                   paper_bgcolor = "transparent") %>%
    plotly::config(toImageButtonOptions = list(format = "svg"))
  
}

gg_avgexpr_plot <- function(table,
                            pvalue_threshold,
                            adjusted = "q.value",
                            gene_column = "Gene") {
  max_x_data <- max(abs(min(table$AvgExpr)), max(table$AvgExpr))
  signif_labels_colors <-
    c(
      "not significant" = "gray",
      "log FC" = "darkgreen",
      "p-value" = "blue",
      "q-value" = "blue",
      "log FC and p-value" = "red",
      "log FC and q-value" = "red"
    )
  if (adjusted == "q.value") {
    ylab_text <- "-log10 q"
  } else {
    ylab_text <- "-log10 p"
  }
  ggplot(table, aes(y = .data[[adjusted]],
                    x = .data$AvgExpr,
                    color = stringr::str_wrap(.data$color, width = 20))) +
    geom_point() + 
    geom_text(aes(label = .data[[1]]),
              data = table[table$signif == 3,],
              vjust = "top",
              hjust = "right") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = signif_labels_colors) +
    ylab(ylab_text) + 
    geom_hline(yintercept = pvalue_threshold) +
    xlim(-max_x_data, max_x_data) +
    theme_classic() +
    theme(legend.title = element_blank())
}

prepareModelResultsTable <-
  function(table,
           fc_threshold = 1,
           pvalue_threshold = -log10(0.05),
           pvalue_adjusted_flag = "p.value"
           ) {
    
  signif_labels <- list("not significant", "log FC",
                        "%s", "log FC and %s")
  pvalue_labels <- list("p.value" = "p-value",
                        "q.value" = "q-value")
  
  pvalue_label <- pvalue_labels[pvalue_adjusted_flag]
  table$pvalue_signif <-
    as.numeric(
      table[[pvalue_adjusted_flag]] < pvalue_threshold
    )
  # Create numeric significance level
  # 0 = not, 1 = FC only, 2 pvalue only, 3 both
  if ("logFC" %in% colnames(table)) {
    table$fc_signif <-
      as.numeric(abs(table$logFC) > abs(fc_threshold))
    table$signif <- table$fc_signif + 2 * table$pvalue_signif  
  } else {
    table$signif <- 2 * table$pvalue_signif  
  }
  # Match significance value with label
  table$color <-
    sprintf(as.character(signif_labels[table$signif+1]), pvalue_label)
  # Apply log transformation to p and q value
  table$p.value <- -log10(table$p.value)
  table$q.value <- -log10(table$q.value)
  table
}
