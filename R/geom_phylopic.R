##' geom layer for using phylopic image
##'
##'
##' @title geom_phylopic
##' @param mapping aes mapping
##' @param data data
##' @param inherit.aes whether inherit aes mapping from ggplot()
##' @param na.rm whether remove NA values
##' @param by one of 'width' or 'height' for specifying size
##' @param height size (by height) of phylopic image to be used
##' @param ... additional parameter
##' @return ggplot2 layer
##' @export
##' @author guangchuang yu
geom_phylopic <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", height = 512, ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., geom = 'phylopic', height = height)
}


phylopic <- function(id, height=512) {
    paste0("http://phylopic.org/assets/images/submissions/", id, ".", height, ".png")
}

