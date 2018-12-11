

##' key drawing function
##'
##' 
##' @name draw_key
##' @param data A single row data frame containing the scaled aesthetics to display in this key
##' @param params A list of additional parameters supplied to the geom.
##' @param size Width and height of key in mm
##' @return A grid grob
NULL


ggname <- getFromNamespace("ggname", "ggplot2")

##' @rdname draw_key
##' @importFrom grid rectGrob
##' @importFrom grid pointsGrob
##' @importFrom grid gpar
##' @importFrom scales alpha
##' @export
draw_key_image <- function(data, params, size) {
    kt <- getOption("ggimage.keytype")
    if (is.null(kt)) {
        kt <- "point"
    }

    if (kt == "point") {
        keyGrob <- pointsGrob(
            0.5, 0.5,
            pch = 19,
            gp = gpar (
                col = alpha(data$colour, data$alpha),
                fill = alpha(data$colour, data$alpha),
                fontsize = 3 * ggplot2::.pt,
                lwd = 0.94
                )
        )
    } else if (kt == "rect") {
        keyGrob <- rectGrob(gp = gpar(
                     col = NA,
                     fill = alpha(data$colour, data$alpha)
                     ))
    } else if (kt == "image") {
        img <- image_read(system.file("extdata/Rlogo.png", package="ggimage"))
        grobs <- lapply(seq_along(data$colour), function(i) {
            img <- color_image(img, data$colour[i], data$alpha[i])

            rasterGrob(
                0.5, 0.5,
                image = img,
                width = 1,
                height = 1
            )
        })
        class(grobs) <- "gList"

        keyGrob <- ggname("image_key",
                          gTree(children = grobs))
    }
    return(keyGrob)
}


