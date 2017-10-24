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
## @importFrom grid convertUnit
## @importFrom grid viewport
## @importFrom grid pushViewport
##' @importFrom tibble data_frame
##' @export
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
    d$xmin <- d$x - d$width/2
    d$xmax <- d$x + d$width/2
    d$ymin <- d$y - d$height/2
    d$ymax <- d$y + d$height/2

    lapply(1:nrow(d), function(i) {
        ## x <- d$x[i]
        ## y <- d$y[i]
        ## width <- d$width[i]
        ## height <- d$height[i]

        ## pushViewport(viewport())
        ## xmin <- convertUnit(unit(x, "native") - unit(width/2, "npc"), "native")
        ## xmax <- convertUnit(unit(x, "native") + unit(width/2, "npc"), "native")
        ## ymin <- convertUnit(unit(y, "native") - unit(height/2, "npc"), "native")
        ## ymax <- convertUnit(unit(y, "native") + unit(height/2, "npc"), "native")

        annotation_custom(
            toGrob(d$subview[[i]]),
            xmin = d$xmin[i],
            xmax = d$xmax[i],
            ymin = d$ymin[i],
            ymax = d$ymax[i])
    })
}

##' @importFrom gridGraphics grid.echo
##' @importFrom grid grid.grab
toGrob <- function(subview) {
    if (inherits(subview, "expression")) {
        tmp <- eval(subview) ## base plot may return value via `invisible()`
        if (is.null(tmp) || is.null(toGrob_(tmp))) {
            grid.echo()
            subview <- grid.grab()
        }
    }
    toGrob_(subview)
}


##' @importFrom ggplot2 ggplotGrob
##' @importFrom rvcheck get_fun_from_pkg
toGrob_ <- function(subview) {
    if (inherits(subview, "ggplot")) {
        sv <- ggplotGrob(subview)
    } else if (inherits(subview, "meme")) {
        memeGrob <- get_fun_from_pkg("meme", "memeGrob")
        sv <- memeGrob(subview)
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
