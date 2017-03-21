##' transparent background theme
##'
##'
##' @title theme_transparent
##' @param ... additional parameter to tweak the theme
##' @return ggplot object
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_rect
##' @export
##' @author Guangchuang Yu
theme_transparent <- function(...) {
    theme(panel.background = element_rect(
              fill = "transparent",
              colour = NA),
          plot.background = element_rect(
              fill = "transparent",
              colour = NA),
          legend.key = element_rect(
              fill = "transparent",
              colour = NA),
          legend.background = element_rect(
              fill = "transparent",
              colour = NA), ...)
}
