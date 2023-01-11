#' Vega-lite volcanoplot
#'
#' The table must be enriched with a column called signif_label which is used
#' to color the points
#'
#' @param data DE table enriched with color column
#' @param fc_min fold change significance line. Default is 1 (and -1)
#' @param pvalue_min p-value significance line. Default is 0.05
#' @param pvalue_col column to use for p-value position. Default is P.value
#' @param gene_col column to use for gene name
#' @param colors optional list of custom colors for not significant, logFC,
#'  p-value and logFC and p-value significant. Defaults are black, green, blue
#'  and red
#' @param strokeDash vector with strokeDash configuration. Default is 4,4
#' @param opacity float value for points opacity. Default is 0.5
#'
#' @return a vega spec to be passed to [vegawidget::as_vegaspec()]
#' @noRd
vega_volcanoplot <- function(data,
    fc_min = 1,
    pvalue_min = 0.05,
    pvalue_col = "P.value",
    gene_col = "Gene",
    colors = c("black", "green", "blue", "red"),
    strokeDash = c(4, 4),
    opacity = 0.5,
    gene_list = NULL) {
    data$log10 <- -log10(data[[pvalue_col]])

    if (!is.null(gene_list)) {
        data$displayGene <- ifelse(data[[gene_col]] %in% gene_list,
                                   data[[gene_col]],
                                   "")
        avoidFlag <- FALSE
    } else {
        data$displayGene <- ifelse(data$signif == 3, data[[gene_col]], "")
        avoidFlag <- TRUE
    }

    logfc_limit <- max(max(abs(data$logFC)) * 1.5, 1.2, fc_min)

    logfc_line <- data.frame(logFC = c(-fc_min, fc_min))
    signif_line <- data.frame(log10 = c(-log10(pvalue_min)))

    signif_labels <- list(
        "not significant",
        "log FC",
        "%s",
        "log FC and %s"
    )

    pvalue_label <- switch(pvalue_col,
        "P.value" = "p-value",
        "PValue" = "p-value",
        "pvalue" = "p-value",
        "FDR" = "adj. p-value",
        "padj" = "adj. p-value",
        "adj.P.Val" = "adj. p-value",
        "q.value" = "adj. p-value",
        "adj. p-value"
    )

    # suppressWarnings:
    # For "not significant" and "log FC" nothing needs to be done in sprintf
    # So it gives a warning
    # But it's easier to just pass the argument even if not needed
    color_domain <-
        suppressWarnings(lapply(signif_labels, sprintf, pvalue_label))

    chart <- list(
        `$schema` = vegawidget::vega_schema("vega"),
        data = list(
            list(name = "source_0", values = data),
            list(name = "source_1", values = logfc_line),
            list(name = "source_2", values = signif_line),
            list(
                name = "data_0",
                source = "source_0",
                transform = list(
                    list(
                        type = "filter",
                        expr = "isValid(datum[\"logFC\"]) &&
                        isFinite(+datum[\"logFC\"])  &&
                        isValid(datum[\"log10\"]) &&
                        isFinite(+datum[\"log10\"])"
                    )
                )
            ),
            list(
                name = "data_1",
                source = "source_1",
                transform = list(
                    list(
                        type = "filter",
                        expr = "isValid(datum[\"logFC\"]) &&
                                isFinite(+datum[\"logFC\"])"
                    )
                )
            ),
            list(
                name = "data_2",
                source = "source_2",
                transform = list(
                    list(
                        type = "filter",
                        expr = "isValid(datum[\"log10\"]) &&
                                isFinite(+datum[\"log10\"])"
                    )
                )
            )
        ),
        background = NULL,
        padding = 5,
        width = 400,
        height = 400,
        style = "cell",
        config = list(
            axis = list(grid = FALSE)
        ),
        marks = list(
            list(
                name = "points",
                type = "symbol",
                style = list("point"),
                from = list(data = "data_0"),
                encode = list(
                    update = list(
                        opacity = list(value = opacity),
                        fill = list(scale = "color", field = "signif_label"),
                        tooltip = list(
                            signal =
                                paste0(
                                    "{\"", gene_col, "\": isValid(datum[\"",
                                    gene_col, "\"]) ? datum[\"", gene_col,
                                    "\"] : \"\"+datum[\"", gene_col,
                                    "\"], \"logFC\": isValid(datum[\"logFC\"]) ?
                    datum[\"logFC\"] : \"\"+datum[\"logFC\"],
                    \"-log10 p\": isValid(datum[\"log10\"]) ? datum[\"log10\"] :
                    \"\"+datum[\"log10\"]}"
                                )
                        ),
                        x = list(scale = "x", field = "logFC"),
                        y = list(scale = "y", field = "log10")
                    )
                )
            ),
            list(
                type = "text",
                from = list(data = "points"),
                encode = list(enter = list(
                    text = list(field = "datum.displayGene"),
                    fontSize = list(value = 10)
                )),
                transform = list(
                    list(
                        type = "label",
                        avoidBaseMark = avoidFlag,
                        #anchor = list("top", "bottom", "right", "left"),
                        #offset = list(1),
                        size = list(signal = "[width+60,height]")
                    )
                )
            ),
            list(
                name = "vrule",
                type = "rule",
                style = list("rule"),
                from = list(data = "data_1"),
                encode = list(
                    update = list(
                        strokeDash = list(value = strokeDash),
                        stroke = list(value = "black"),
                        x = list(scale = "x", field = "logFC"),
                        y = list(value = 0),
                        y2 = list(field = list(group = "height"))
                    )
                )
            ),
            list(
                name = "hrule",
                type = "rule",
                style = list("rule"),
                from = list(data = "data_2"),
                encode = list(
                    update = list(
                        strokeDash = list(value = strokeDash),
                        stroke = list(value = "black"),
                        y = list(scale = "y", field = "log10"),
                        x2 = list(value = 0),
                        x = list(field = list(group = "width"))
                    )
                )
            )
        ),
        scales = list(
            list(
                name = "x",
                type = "linear",
                domain = list(-logfc_limit, logfc_limit),
                range = list(0, list(signal = "width")),
                nice = TRUE,
                zero = TRUE
            ),
            list(
                name = "y",
                type = "linear",
                domain = list(
                    fields = list(
                        list(data = "data_0", "field" = "log10"),
                        list(data = "data_2", "field" = "log10")
                    )
                ),
                range = list(list(signal = "height"), 0),
                nice = TRUE,
                zero = TRUE
            ),
            list(
                name = "color",
                type = "ordinal",
                domain = color_domain,
                range = colors
            )
        ),
        axes = list(
            list(
                scale = "x",
                orient = "bottom",
                grid = FALSE,
                title = "logFC",
                labelFlush = TRUE,
                labelOverlap = TRUE,
                tickCount = list(signal = "ceil(width/40)"),
                zindex = 0
            ),
            list(
                scale = "y",
                orient = "left",
                grid = FALSE,
                title = "-log10 p",
                labelOverlap = TRUE,
                tickCount = list(signal = "ceil(height/40)"),
                zindex = 0
            )
        ),
        legends = list(
            list(
                title = "Significance",
                fill = "color",
                symbolType = "circle",
                encode = list(
                    symbols = list(
                        update = list(
                            opacity = list(value = opacity)
                        )
                    )
                )
            )
        )
    )
    chart
}

prepareModelResultsTable <-
    function(table,
    fc_threshold = 1,
    pvalue_threshold = 0.05,
    pvalue_col = "P.value") {
        signif_labels <- list(
            "not significant", "log FC",
            "%s", "log FC and %s"
        )
        pvalue_labels <- list(
            "P.value" = "p-value",
            "PValue" = "p-value",
            "pvalue" = "p-value",
            "FDR" = "adj. p-value",
            "padj" = "adj. p-value",
            "adj.P.Val" = "adj. p-value",
            "q.value" = "adj. p-value"
        )
        pvalue_label <- pvalue_labels[pvalue_col]
        table$pvalue_signif <-
            as.numeric(
                table[[pvalue_col]] <= pvalue_threshold
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
        # Note: suppressWarnings is used because sometimes sprintf needs
        # no arguments, but this is known
        table$signif_label <-
            suppressWarnings(
                sprintf(as.character(signif_labels[table$signif + 1]),
                        pvalue_label)
            )
        # Apply log transformation to p and q value
        table
    }
