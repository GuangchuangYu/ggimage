##' geom layer for using flag image
##'
##'
##' @title geom_flag
##' @param mapping aes mapping
##' @param data data
##' @param inherit.aes whether inherit aes mapping from ggplot()
##' @param na.rm whether remove NA values
##' @param by one of 'width' or 'height' for specifying size
##' @param ... additional parameter
##' @return ggplot2 layer
##' @export
##' @author guangchuang yu
geom_flag <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., geom = 'flag')
}

flag <- function(flag) {
    paste0('https://behdad.github.io/region-flags/png/', flag, ".png")
}


##' list available flag
##'
##'
##' @title list.flag
##' @return flag vector
##' @export
##' @author guangchuang yu
list.flag <- function() {
     x <- readLines("https://github.com/behdad/region-flags/tree/gh-pages/png")
     y <- x[grep("title=\"\\w+\\.png", x)]
     sub(".*title=\"(\\w+)\\.png.*", '\\1', y)
}

