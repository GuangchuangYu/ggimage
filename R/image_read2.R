##' read image (by magick::image_read) with the ability to remove marginal empty space
##'
##'
##' @title image_read2
##' @param path file path
##' @param ... additional parameters that pass to magick::image_read
##' @param cut_empty_space whether remove marginal empty space
##' @return magick-image object
##' @importFrom magick image_read
##' @importFrom magick image_info
##' @export
##' @author Guangchuang Yu
image_read2 <- function(path, ..., cut_empty_space = TRUE) {
    img <- image_read(path, ...)
    if (!cut_empty_space) {
        return(img)
    }

    bitmap <- img[[1]]
    info <- image_info(img)

    row_not_blank <- c(range(which(rowSums(bitmap[1,,] == "ff") != info$height)),
                       range(which(rowSums(bitmap[2,,] == "ff") != info$height)),
                       range(which(rowSums(bitmap[3,,] == "ff") != info$height))
                       )

    col_not_blank <- c(range(which(colSums(bitmap[1,,] == "ff") != info$width)),
                       range(which(colSums(bitmap[2,,] == "ff") != info$width)),
                       range(which(colSums(bitmap[3,,] == "ff") != info$width))
                       )

    row_min <- min(row_not_blank)
    row_max <- max(row_not_blank)
    col_min <- min(col_not_blank)
    col_max <- max(col_not_blank)

    x <- bitmap[1, row_min:row_max, col_min:col_max]
    y <- bitmap[2, row_min:row_max, col_min:col_max]
    z <- bitmap[3, row_min:row_max, col_min:col_max]

    bitmap <- array(as.raw(0),
        dim = c(3, row_max - row_min + 1, col_max - col_min + 1))

    bitmap[1,,] <- x
    bitmap[2,,] <- y
    bitmap[3,,] <- z

    image_read(bitmap)
}
