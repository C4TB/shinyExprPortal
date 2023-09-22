#' Side-by-side trajectory scatterplot
#'
#' Scatterplot that can be combined with vega_add_fitline.
#'
#' @param data a data frame with x, "expression", facet_var, color_var and
#' group_var
#' @param x a measure used in the x position
#' @param facet_var variable that is used to split each scatterplot
#' @param color_var variable for color/trajectory
#' @param color_palette optional list of colors (not palette name) for color_var
#'
#' @return a list of vega specification. Must be passed to
#' [vegawidget::as_vegaspec] afterwards
#'
#' @noRd
vega_traj_scatterplot <-
    function(data,
    x,
    facet_var = NULL,
    color_var,
    color_palette = NULL) {
        point_layer <- list(
            mark = list(
                type = "point",
                filled = TRUE,
                strokeWidth = 1
            ),
            encoding = list(
                x = list(
                    field = x,
                    type = "quantitative",
                    title = NULL,
                    scale = list(zero = FALSE)
                ),
                y = list(
                    field = "expression",
                    type = "quantitative",
                    title = NULL,
                    scale = list(zero = FALSE)
                ),
                color = list(field = color_var, type = "nominal")
            )
        )

        if (!is.null(color_palette)) {
            point_layer$encoding$color$scale <- list(range = color_palette)
            if (!is.null(names(color_palette))) {
                point_layer$encoding$color$scale$domain <- names(color_palette)
            }
        }

        spec <- list(
            `$schema` = vegawidget::vega_schema(),
            data = list(values = data),
            title = list(
                text = "Expression",
                orient = "left",
                align = "center",
                anchor = "middle"
            ),
            resolve = list(scale = list(x = "shared", y = "shared")),
            background = NULL,
            config = list(
                view = list(continuousHeight = 225, continuousWidth = 250)
            ),
            usermeta = list(
                x = x,
                y = "expression",
                facet_var = facet_var
            )
        )

        if (!is.null(facet_var)) {
            spec$spec <- list(
                layer = list(point_layer),
                resolve = list(axis = list(x = "shared"))
            )
            spec$facet <- list(
                field = facet_var,
                header = list(
                    title = x,
                    titleAlign = "center",
                    titleAnchor = "middle",
                    titleOrient = "bottom",
                    labelFontWeight = 600,
                    labelFontSize = 12
                )
            )
        } else {
            spec$layer <- list(point_layer)
        }

        spec
    }