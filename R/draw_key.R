

##' key drawing function
##'
##' 
##' @name draw_key
##' @param data A single row data frame containing the scaled aesthetics to display in this key
##' @param params A list of additional parameters supplied to the geom.
##' @param size Width and height of key in mm
##' @return A grid grob
NULL


##' @rdname draw_key
##' @importFrom grid rectGrob
##' @importFrom grid gpar
##' @export
draw_key_image <- function(data, params, size) {
    rectGrob(gp = gpar(
                 col = NA,
                 fill = alpha(data$colour, data$alpha)
             ))
}
