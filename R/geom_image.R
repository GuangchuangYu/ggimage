##' geom layer for visualizing image files
##'
##'
##' @title geom_image
##' @param mapping aes mapping
##' @param data data
##' @param stat stat
##' @param position position
##' @param inherit.aes logical, whether inherit aes from ggplot()
##' @param na.rm logical, whether remove NA values
##' @param by one of 'width' or 'height'
##' @param ... additional parameters
##' @return geom layer
##' @importFrom ggplot2 layer
##' @export
##' @examples
##' library("ggplot2")
##' library("ggimage")
##' set.seed(2017-02-21)
##' d <- data.frame(x = rnorm(10),
##'                 y = rnorm(10),
##'                 image = sample(c("https://www.r-project.org/logo/Rlogo.png",
##'                                 "https://jeroenooms.github.io/images/frink.png"),
##'                               size=10, replace = TRUE)
##'                )
##' ggplot(d, aes(x, y)) + geom_image(aes(image=image))
##' @author guangchuang yu
geom_image <- function(mapping=NULL, data=NULL, stat="identity",
                       position="identity", inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {

    dots <- eval(substitute(alist(...)))
    image <- dots$image
    by <- match.arg(by, c("width", "height"))

    layer(
        data=data,
        mapping=mapping,
        geom=GeomImage,
        stat=stat,
        position=position,
        show.legend=NA,
        inherit.aes=inherit.aes,
        params = list(
            na.rm = na.rm,
            image = image,
            by = by,
            ...),
        check.aes = FALSE
    )
}

##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 draw_key_blank
##' @importFrom grid gTree
##' @importFrom grid gList
GeomImage <- ggproto("GeomImage", Geom,
                     draw_panel = function(data, panel_scales, coord, image, by, na.rm=FALSE) {
                         data <- coord$transform(data, panel_scales)
                         if (!is.null(data$image)) {
                             image <- data$image
                         }

                         groups <- split(data, factor(image))
                         imgs <- names(groups)
                         grobs <- lapply(seq_along(groups), function(i) {
                             data <- groups[[i]]
                             imageGrob(data$x, data$y, data$size, imgs[i], by)
                         })
                         ggplot2:::ggname("geom_image",
                                          gTree(children = do.call("gList", grobs)))
                     },
                     non_missing_aes = c("size"),
                     required_aes = c("x", "y"),
                     default_aes = aes(size=0.05),
                     draw_key = draw_key_blank
                     )

##' @importFrom EBImage readImage
##' @importFrom grid rasterGrob
##' @importFrom methods is
imageGrob <- function(x, y, size, img, by) {
    if (!is(img, "Image")) {
        img <- readImage(img)
        asp <- getAR(img)
    }
    if (by == "width") {
        width <- size
        height <- size/asp
    } else {
        width <- size * asp
        height <- size
    }

    rasterGrob(x = x,
               y = y,
               image = img,
               default.units = "native",
               height = height,
               width = width)
}


getAR <- function(img) {
    dims <- dim(img)[1:2]
    dims[1]/dims[2]
}

