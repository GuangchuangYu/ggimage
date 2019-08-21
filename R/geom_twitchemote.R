##' geom layer for using Twitch emotes
##'
##'
##' @title geom_twitchemotes
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author Brendan Rocks
geom_twitchemote <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                             na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = twitchemote)
}

twitchemote <- function(twitchemotes) {
    emote_lookup <- list.twitchemote()
    emote_ids <- emote_lookup[twitchemotes]

    ## Return a URL for the emote itself
    paste0("https://static-cdn.jtvnw.net/emoticons/v1/", emote_ids, "/1.0")
}

##' @importFrom jsonlite fromJSON
list.twitchemote <- function() {
    ## Obtain a list mapping emote text to emote ids
    emote_list <- jsonlite::fromJSON("https://twitchemotes.com/api_cache/v3/global.json")

    ## Make all keys lower-case
    names(emote_list) <- tolower(names(emote_list))
    unlist(lapply(emote_list, function(x) x$id))
}
