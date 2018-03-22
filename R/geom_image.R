geom_ggtree_image <- function() {

}


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
##' @param color specify the color of image. NULL for original color
##' @param nudge_x horizontal adjustment to nudge image
##' @param angle angle of image
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
                       na.rm=FALSE, by="width", color=NULL, nudge_x = 0, angle = 0, ...) {

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
            by = by,
            image_color = color,
            nudge_x = nudge_x,
            angle = angle,
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
                     setup_data = function(data, params) {
                         if (is.null(data$subset))
                             return(data)
                         data[which(data$subset),]
                     },

                     draw_panel = function(data, panel_scales, coord, by, na.rm=FALSE,
                                           image_color=NULL, alpha=1, .fun = NULL, height, image_fun = NULL,
                                           hjust=0.5, angle = 0, nudge_x = 0, nudge_y = 0, asp=1) {
                         data$x <- data$x + nudge_x
                         data$y <- data$y + nudge_y
                         data <- coord$transform(data, panel_scales)


                         if (!is.null(.fun) && is.function(.fun))
                             data$image <- .fun(data$image)

                         groups <- split(data, factor(data$image))
                         imgs <- names(groups)
                         grobs <- lapply(seq_along(groups), function(i) {
                             data <- groups[[i]]
                             imageGrob(data$x, data$y, data$size, imgs[i], by, hjust, image_color, alpha, image_fun, angle, asp)
                         })
                         class(grobs) <- "gList"

                         ggplot2:::ggname("geom_image",
                                          gTree(children = grobs))
                     },
                     non_missing_aes = c("size", "image"),
                     required_aes = c("x", "y"),
                     default_aes = aes(size=0.05, image="https://www.r-project.org/logo/Rlogo.png"),
                     draw_key = draw_key_blank ## need to write the `draw_key_image` function.
                     )


##' @importFrom magick image_read
##' @importFrom magick image_colorize
##' @importFrom grid rasterGrob
##' @importFrom grid viewport
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom methods is
imageGrob <- function(x, y, size, img, by, hjust, color, alpha, image_fun, angle, asp=1) {
    if (!is(img, "magick-image")) {
        img <- image_read(img)
        asp <- getAR2(img)/asp
    }

    unit <- "native"
    if (any(size == Inf)) {
        x <- 0.5
        y <- 0.5
        width <- 1
        height <- 1
        unit <- "npc"
    } else if (by == "width") {
        width <- size
        height <- size/asp
    } else {
        width <- size * asp
        height <- size
    }

    if (hjust == 0 || hjust == "left") {
        x <- x + width/2
    } else if (hjust == 1 || hjust == "right") {
        x <- x - width/2
    }

    if (!is.null(image_fun)) {
        img <- image_fun(img)
    }

    if (!is.null(color) || alpha != 1) {
        bitmap <- img[[1]]
        if (!is.null(color)) {
            col <- col2rgb(color)
            bitmap[1,,] <- as.raw(col[1])
            bitmap[2,,] <- as.raw(col[2])
            bitmap[3,,] <- as.raw(col[3])
        }

        if (alpha != 1)
            bitmap[4,,] <- as.raw(as.integer(bitmap[4,,]) * alpha)
        img <- image_read(bitmap)
    }

    rasterGrob(x = x,
               y = y,
               image = img,
               default.units = unit,
               height = height,
               width = width,
               interpolate = FALSE,
               vp = viewport(angle=angle))
}


##' @importFrom magick image_info
getAR2 <- function(magick_image) {
    info <- image_info(magick_image)
    info$width/info$height
}


compute_just <- getFromNamespace("compute_just", "ggplot2")


## @importFrom EBImage readImage
## @importFrom EBImage channel
## imageGrob2 <- function(x, y, size, img, by, color, alpha) {
##     if (!is(img, "Image")) {
##         img <- readImage(img)
##         asp <- getAR(img)
##     }

##     unit <- "native"
##     if (any(size == Inf)) {
##         x <- 0.5
##         y <- 0.5
##         width <- 1
##         height <- 1
##         unit <- "npc"
##     } else if (by == "width") {
##         width <- size
##         height <- size/asp
##     } else {
##         width <- size * asp
##         height <- size
##     }

##     if (!is.null(color)) {
##         color <- col2rgb(color) / 255

##         img <- channel(img, 'rgb')
##         img[,,1] <- color[1]
##         img[,,2] <- color[2]
##         img[,,3] <- color[3]
##     }

##     if (dim(img)[3] >= 4) {
##         img[,,4] <- img[,,4]*alpha
##     }

##     rasterGrob(x = x,
##                y = y,
##                image = img,
##                default.units = unit,
##                height = height,
##                width = width,
##                interpolate = FALSE)
## }


## getAR <- function(img) {
##     dims <- dim(img)[1:2]
##     dims[1]/dims[2]
## }


##################################################
##                                              ##
## another solution, but the speed is too slow  ##
##                                              ##
##################################################

## draw_key_image <- function(data, params, size) {
##     imageGrob(0.5, 0.5, image=data$image, size=data$size)
## }


## ##' @importFrom ggplot2 ggproto
## ##' @importFrom ggplot2 Geom
## ##' @importFrom ggplot2 aes
## ##' @importFrom ggplot2 draw_key_blank
## GeomImage <- ggproto("GeomImage", Geom,
##                      non_missing_aes = c("size", "image"),
##                      required_aes = c("x", "y"),
##                      default_aes = aes(size=0.05, image="https://www.r-project.org/logo/Rlogo.png"),
##                      draw_panel = function(data, panel_scales, coord, by, na.rm=FALSE) {
##                          data$image <- as.character(data$image)
##                          data <- coord$transform(data, panel_scales)
##                          imageGrob(data$x, data$y, data$image, data$size, by)
##                      },
##                      draw_key = draw_key_image
##                      )


## ##' @importFrom grid grob
## imageGrob <- function(x, y, image, size=0.05, by="width") {
##     grob(x=x, y=y, image=image, size=size, by=by, cl="image")
## }

## ##' @importFrom grid drawDetails
## ##' @importFrom grid grid.raster
## ##' @importFrom EBImage readImage
## ##' @method drawDetails image
## ##' @export
## drawDetails.image <- function(x, recording=FALSE) {
##     image_object <- lapply(x$image, readImage)
##     names(image_object) <- x$image
##     for (i in seq_along(x$image)) {
##         img <- image_object[[x$image[i]]]
##         size <- x$size[i]
##         by <- x$by
##         asp <- getAR(img)
##         if (is.na(size)) {
##             width <- NULL
##             height <- NULL
##         } else if (by == "width") {
##             width <- size
##             height <- size/asp
##         } else {
##             width <- size * asp
##             height <- size
##         }

##         grid.raster(x$x[i], x$y[i],
##                     width = width,
##                     height = height,
##                     image = img,
##                     interpolate=FALSE)
##     }
## }

## ##' @importFrom ggplot2 discrete_scale
## ##' @importFrom scales identity_pal
## ##' @importFrom ggplot2 ScaleDiscreteIdentity
## ##' @export
## scale_image <- function(..., guide = "legend") {
##   sc <- discrete_scale("image", "identity", identity_pal(), ..., guide = guide,
##                        super = ScaleDiscreteIdentity)

##   sc
## }
