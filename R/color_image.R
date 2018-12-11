color_image <- function(img, color, alpha = NULL) {
    if (is.null(color))
        return(img)

    if (length(color) > 1) {
        stop("color should be a vector of length 1")
    }

    bitmap <- img[[1]]
    col <- col2rgb(color)
    bitmap[1,,] <- as.raw(col[1])
    bitmap[2,,] <- as.raw(col[2])
    bitmap[3,,] <- as.raw(col[3])

    if (!is.null(alpha) && alpha != 1)
        bitmap[4,,] <- as.raw(as.integer(bitmap[4,,]) * alpha)

    image_read(bitmap)
}
