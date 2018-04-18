##' set background for ggplot
##'
##'
##' @title ggbackground
##' @param gg gg object
##' @param background background image
##' @param ... additional parameter to manipulate background image, see also geom_image
##' @return gg object
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 theme_void
##' @export
##' @author guangchuang yu
ggbackground <- function(gg, background, ...) {
    ggplot(data.frame(x = 0:1, y = 0:1), aes_(x = ~x, y = ~y)) +
        geom_image(image = background,size=Inf, ...) +
        geom_subview(subview = gg + theme_transparent(),
                     width=Inf, height=Inf, x=.5, y=.5) +
        theme_void() + theme(plot.margin=unit(c(0,0, -.2, -.2), "lines"))
}
