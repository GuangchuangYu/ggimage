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
    uid <- phylopic_valid_id(id)
    paste0("http://phylopic.org/assets/images/submissions/", uid, ".", height, ".png")
}

phylopic_valid_id <- function(id) {
    sapply(id, phylopic_valid_id_item)
}

phylopic_valid_id_item <- function(id) {
    url <- paste0("http://phylopic.org/api/a/name/", id,
                  "/images?subtaxa=true&supertaxa=true&options=pngFiles+canoicalName+json")
    res <- jsonlite::fromJSON(url)$result
    if (length(res$same) > 0) {
        taxa <- res$same
    } else if (length(res$supertaxa) > 0) {
        taxa <- res$supertaxa
    } else if (length(res$subtaxa) > 0){
        taxa <- res$subtaxa
    } else if (length(res$other) > 0) {
        taxa <- res$other
    } else {
        return(NA)
    }

    uid <- taxa$uid[1]
    return(uid)
}

##' query phylopic to get uid from scientific name
##'
##' 
##' @title phylopic_uid
##' @param name scientific name
##' @return phylopic uid
##' @export
##' @author Guangchuang Yu
phylopic_uid <- function(name) {
    if (length(name) == 1) {
        res <- phylopic_uid_item(name)
    } else {
        res <- lapply(name, phylopic_uid_item)
        
        names(res) <- name
        return(res)
    }

    return(res)
}

phylopic_uid_item <- function(name) {
    x <- gsub("[^a-zA-Z]+", "+", name)
    url <- paste0("http://phylopic.org/api/a/name/search?text=",
                  x, "&options=scientific+json")
    jsonlite::fromJSON(url)$result[[1]]
}



