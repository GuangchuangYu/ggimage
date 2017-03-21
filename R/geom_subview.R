##' subview geom
##'
##'
##' @title geom_subview
##' @param subview subview
##' @param x x position
##' @param y y position
##' @param width width
##' @param height height
##' @return layer
##' @importFrom ggplot2 annotation_custom
##' @importFrom gridGraphics grid.echo
##' @importFrom grid grid.grab
##' @importFrom grid convertUnit
##' @importFrom grid viewport
##' @importFrom grid pushViewport
##' @importFrom tibble data_frame
##' @author guangchuang yu
geom_subview <- function(subview, x, y, width=.1, height=.1) {
    len <- sapply(list(subview, x, y, width, height), length)
    mlen <- max(len)
    for (i in len) {
        if (mlen %% i != 0)
            stop("subview, x, y, width and height should have equal length")
    }
    if (!inherits(subview, "list")) {
        subview <- list(subview)
    }
    d <- data_frame(x=x, y=y,
                    width=width,
                    height=height,
                    subview=subview)
    lapply(1:nrow(d), function(i) {
        x <- d$x[i]
        y <- d$y[i]
        width <- d$width[i]
        height <- d$height[i]
        subview <- d$subview[[i]]

        if (inherits(subview, "expression")) {
            tmp <- eval(subview) ## base plot may return value via `invisible()`
            if (is.null(tmp) || is.null(toGrob(tmp))) {
                grid.echo()
                subview <- grid.grab()
            }
        }

        sv <- toGrob(subview)

        pushViewport(viewport())
        xmin <- convertUnit(unit(x, "native") - unit(width/2, "npc"), "native")
        xmax <- convertUnit(unit(x, "native") + unit(width/2, "npc"), "native")

        ymin <- convertUnit(unit(x, "native") - unit(height/2, "npc"), "native")
        ymax <- convertUnit(unit(x, "native") + unit(height/2, "npc"), "native")
        annotation_custom(
            sv,
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax)
    })
}

##' @importFrom ggplot2 ggplotGrob
toGrob <- function(subview) {
    if (inherits(subview, "ggplot")) {
        sv <- ggplotGrob(subview)
    } else if (inherits(subview, "trellis")) {
        sv <- grid::grid.grabExpr(print(subview))
    } else if (inherits(subview, "grob")) {
        sv <- subview
    } else {
        return(NULL)
    }
    return(sv)
}


unit <- grid::unit
