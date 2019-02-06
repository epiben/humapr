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

humap <- function(data = NULL, mapping = NULL) {
    # h_env is an internal object in R/sysdata.rda, and is reset here
    rm(list = ls(envir = h_env), envir = h_env)
    h_env$data <- data
    h_env$mapping <- mapping

    # Just return a ggplot object with appropriate theme settings
    blank <- ggplot2::element_blank() # => simpler theme() call
    ggplot2::ggplot() +
        ggplot2::theme(axis.title = blank,
                       axis.text = blank,
                       axis.line = blank,
                       axis.ticks = blank,
                       panel.grid = blank,
                       panel.background = blank)
}
