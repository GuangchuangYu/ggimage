##' geom layer for using flag image
##'
##'
##' @title geom_flag
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author guangchuang yu
geom_flag <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = flag)
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

