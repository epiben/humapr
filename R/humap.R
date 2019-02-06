#' Create a humap, ready for plotting a geom
#'
#' \code{humap} sets up a \code{ggplot} object with \code{theme} settings appropriate for adding \code{geoms} from the \pkg{humapr} package.
#' A \code{ggplot} object, it's compatible with standard \pkg{ggplot2} function, e.g.,
#' \code{theme()} and \code{facet_wrap()}. Doesn't really do anything on its own.
#'
#' @return A ggplot object with suitable layout settings for the purpose of
#'   \code{humap_body}.
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
