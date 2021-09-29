url.exists <- function(url) {
    x <- httr::status_code(httr::HEAD(url))
    if (identical(x, 200L)) return(TRUE)
    cnt <- 1
    while (identical(x, 502L)) {
        ## server not response
        ## try again
        x <- httr::status_code(httr::HEAD(url))
        if (identical(x, 200L)) return(TRUE)
        cnt <- cnt + 1
        if (cnt > 10) break
    }
    return(FALSE)
}

check_url <- function(url) {
    idx <- vapply(url, url.exists, logical(1))
    url[!idx] <- NA
    return(url)
}

zeroGrob <- function() .zeroGrob

.zeroGrob <- grid::grob(cl = "zeroGrob", name = "NULL")

widthDetails.zeroGrob <- function(x) unit(0, "cm")
heightDetails.zeroGrob <- function(x) unit(0, "cm")
grobWidth.zeroGrob <- function(x) unit(0, "cm")
grobHeight.zeroGrob <- function(x) unit(0, "cm")
drawDetails.zeroGrob <- function(x, recording) {}
