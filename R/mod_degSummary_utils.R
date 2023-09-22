add_knitr_table <- function(model_wide, model_cols, header_cols, header_spec) {
    knitr_table <- model_wide %>%
        knitr::kable(
            align = "r",
            format = "html",
            escape = FALSE,
            col.names = c(model_cols, gsub(".*#(.*)", "\\1", header_cols))
        ) %>%
        kableExtra::kable_styling(
            full_width = FALSE,
            position = "left",
            font_size = 12
        ) %>%
        kableExtra::collapse_rows(
            valign = "top",
            columns = seq_along(max(1,length(model_cols)-1))
        )
    if (!is.null(header_spec))
        knitr_table <- knitr_table %>% kableExtra::add_header_above(
            header = c(" " = length(model_cols), header_spec)
            )
    knitr_table
}