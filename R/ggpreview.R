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
##' @importFrom ggplotify as.grob
##' @importFrom grid grid.draw
##' @importFrom grid grid.newpage
##' @importFrom magick image_read
##' @export
##' @author Guangchuang Yu
ggpreview <- function(plot = last_plot(), width, height, units = 'in') {
    f <- tempfile(fileext = '.png') 
    if (!inherits(plot, 'gg')) {
        ## plot <- as.ggplot(plot)    
        plot <- as.grob(plot)    
    }

    ggsave(filename = f, plot, width=width, height=height, units=units)
    ## as.ggplot(image_read(f))
    grid.newpage()
    grid.draw(as.grob(image_read(f)))
}

