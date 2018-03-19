##' subview geom
##'
##'
##' @title geom_subview
##' @param mapping aes mapping, requires 'x', 'y' and 'subview'
##' @param data data frame
##' @param width width
##' @param height height
##' @param x x position of subview. This parameter works if mapping and data is not provided
##' @param y y position of subview. This parameter works if mapping and data is not provided
##' @param subview subview to plot, if not provided in data and specify by mapping
##' @return layer
##' @importFrom ggplot2 annotation_custom
##' @importFrom ggplot2 aes_
##' @importFrom tibble as_data_frame
## @importFrom grid convertUnit
## @importFrom grid viewport
## @importFrom grid pushViewport
##' @importFrom tibble data_frame
##' @export
##' @author guangchuang yu
geom_subview <- function(mapping = NULL, data = NULL, width=.1, height=.1, x = NULL, y = NULL, subview = NULL) {
    ## can't support `aes(x, y, subview=subview)` as ggplot2 will throw:
    ##     cannot coerce class "c("ggtree", "gg", "ggplot")" to a data.frame

    ## this is a hack to support `aes` without `inherit.aes`. mapping and data must be provided.

    if (is.null(data)) {
        data <- data_frame(x = x, y = y)
    } else if (!inherits(data, 'tbl')) {
        data <- as_data_frame(data)
    }

    if (is.null(mapping)) {
        mapping <- aes_(x = ~x, y = ~y)
    }
    mapping <- as.list(mapping)
    if (is.null(mapping$x)) {
        stop("x aesthetic mapping should be provided")
    }
    if (is.null(mapping$y)) {
        stop("y aesthetic mapping should be provided")
    }
    if (is.null(mapping$subview) && is.null(subview)) {
        stop("subview must be provided")
    }
    if (is.null(mapping$subview)) {
        if (!inherits(subview, "list")) {
            subview <- list(subview)
        }
        data$subview <- subview
    } else {
        data$subview <- data[[as.character(mapping$subview)]]
    }

    xvar <- as.character(mapping$x)
    yvar <- as.character(mapping$y)

    if (is.null(mapping$width)) {
        data$width <- width
    } else {
        data$width <- data[[as.character(mapping$width)]]
    }

    if (is.null(mapping$height)) {
        data$height <- height
    } else {
        data$height <- data[[as.character(mapping$height)]]
    }


    data$xmin <- data[[xvar]] - data$width/2
    data$xmax <- data[[xvar]] + data$width/2
    data$ymin <- data[[yvar]] - data$height/2
    data$ymax <- data[[yvar]] + data$height/2

    lapply(1:nrow(data), function(i) {
        annotation_custom(
            toGrob(data$subview[[i]]),
            xmin = data$xmin[i],
            xmax = data$xmax[i],
            ymin = data$ymin[i],
            ymax = data$ymax[i])
    })
}

##' @importFrom base2grob base2grob
toGrob <- function(subview) {
    if (inherits(subview, "expression") ||
        inherits(subview, "formula") ||
        inherits(subview, "function")) {

        subview <- base2grob(subview)
    }
    return(toGrob_(subview))
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

