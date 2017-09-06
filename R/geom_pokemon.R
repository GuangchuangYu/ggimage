##' geom layer for using pokemon image
##'
##'
##' @title geom_pokemon
##' @param mapping aes mapping
##' @param data data
##' @param inherit.aes whether inherit aes mapping from ggplot()
##' @param na.rm whether remove NA values
##' @param by one of 'width' or 'height' for specifying size
##' @param ... additional parameter
##' @return ggplot2 layer
##' @export
##' @author guangchuang yu
geom_pokemon <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = pokemon)
}

pokemon <- function(id) {
    paste0('https://raw.githubusercontent.com/Templarian/slack-emoji-pokemon/master/emojis/', id, ".png")
}

##' list available pokemon
##'
##'
##' @title list.pokemon
##' @return pokemon vector
##' @export
##' @author guangchuang yu
list.pokemon <- function() {
    list.png("https://github.com/Templarian/slack-emoji-pokemon/tree/master/emojis")
}

list.png <- function(url) {
    x <- readLines(url)
    y <- x[grep("title=\"[a-zA-Z0-9\\-]+\\.png", x)]
    sub(".*title=\"([a-zA-Z0-9\\-]+)\\.png.*", '\\1', y)
}
