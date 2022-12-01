##' preview a plot befor saving it to a file. 
##'
##'
##' @title ggpreview
##' @param plot any plot that supported by the 'ggplotify' package
##' @param width width of the figure
##' @param height height of the figure
##' @param units units of the 'width' and 'height'
##' @return a preview of the figure
##' @importFrom ggplot2 last_plot
##' @importFrom ggplot2 ggsave
##' @importFrom ggplotify as.ggplot
##' @importFrom magick image_read
##' @export
##' @author Guangchuang Yu
ggpreview <- function(plot = last_plot(), width, height, units = 'in') {
    f <- tempfile(fileext = '.png') 
    if (!inherits(plot, 'gg')) {
        plot <- as.ggplot(plot)    
    }

    ggsave(filename = f, plot, width=width, height=height, units=units)
    as.ggplot(image_read(f))
}

