##' geom layer for using phylopic image
##'
##'
##' @title geom_phylopic
##' @inheritParams geom_pokemon
##' @return ggplot2 layer
##' @export
##' @author Guangchuang Yu
geom_phylopic <- function(mapping=NULL, data=NULL, inherit.aes=TRUE,
                       na.rm=FALSE, by="width", ...) {
    geom_image(mapping, data, inherit.aes=inherit.aes, na.rm=na.rm, ..., .fun = phylopic)
}


phylopic <- function(id) {
    width <- getOption("phylopic_width")
    if (is.null(width))
        width <- 256
    url <- paste0("http://phylopic.org/assets/images/submissions/", id, ".", width, ".png")

    check_url(url)
}

phylopic_valid_id <- function(id) {
    res <- vapply(id, phylopic_valid_id_item, character(1))
    i <- which(res == "")
    res[i] <- NA
    return(res)
}

phylopic_valid_id_item <- function(id) {
    url <- paste0("http://phylopic.org/api/a/name/", id,
                  "/images?subtaxa=true&supertaxa=true&options=pngFiles+canoicalName+json")
    res <- tryCatch(jsonlite::fromJSON(url)$result,
                    error = function(e) return(NULL))
    if (is.null(res)) return("")

    if (length(res$same) > 0) {
        taxa <- res$same
    } else if (length(res$supertaxa) > 0) {
        taxa <- res$supertaxa
    } else if (length(res$subtaxa) > 0){
        taxa <- res$subtaxa
    } else if (length(res$other) > 0) {
        taxa <- res$other
    } else {
        return("")
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
    ## if (length(name) == 1) {
    ##     res <- phylopic_uid_item(name)
    ## } else {
    ##     res <- lapply(name, phylopic_uid_item)
        
    ##     names(res) <- name
    ##     return(res)
    ## }

    res <- data.frame(name = name,
                      uid = vapply(name, phylopic_uid_item, character(1)))
    return(res)
}

phylopic_uid_item <- function(name) {
    x <- gsub("[^a-zA-Z]+", "+", name)
    url <- paste0("http://phylopic.org/api/a/name/search?text=",
                  x, "&options=scientific+json")
    #res <- jsonlite::fromJSON(url)$result[[1]]
    res <- tryCatch(jsonlite::fromJSON(url)$result[[1]],
                    error = function(e) return(NULL))
    if (is.null(res)) return("") 
    for (id in res$uid) {
        uid <- phylopic_valid_id(id)
        if (!is.na(uid))
            break
    }
    return(uid)
    ## uid <- phylopic_valid_id(res$uid)
    ## i <- which(!is.na(uid))
    ## res <- res[i, , drop = FALSE]
    ## res$uid <- uid[i]
    ## return(res)
}



