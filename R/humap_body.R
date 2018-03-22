#' Create a humap, ready for plotting a geom
#'
#' \code{humap_body} sets up a \code{ggplot} object, ready for drawing a map,
#' and compatible with with standard \code{ggplot2} function, e.g.,
#' \code{theme()} and \code{facet_wrap()}. Doesn't really do  much on its own.
#'
#' @return A ggplot object with suitable layout settings for the purpose of
#'   \code{hump_body}.
#'
#' @export
#' @import ggplot2
#' @importFrom stats na.exclude na.omit setNames
#' @importFrom magrittr %>%

humap <- function() {
    # Just return a ggplot object with appropriate theme settings
    ggplot2::ggplot() +
        ggplot2::theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.line = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid = element_blank(),
                       panel.background = element_blank())
}
