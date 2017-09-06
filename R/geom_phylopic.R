##' geom layer for using phylopic image
##'
##'
##' @title geom_phylopic
##' @inheritParams geom_pokemon
##' @param height size (by height) of phylopic image to be used
##' @return ggplot2 layer
##' @export
##' @author guangchuang yu
geom_phylopic <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", height = 512, ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., height = height, .fun = phylopic)
}


phylopic <- function(id, height=512) {
    paste0("http://phylopic.org/assets/images/submissions/", id, ".", height, ".png")
}

