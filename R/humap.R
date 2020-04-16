#' Create a humap, ready for plotting a geom
#'
#' \code{humap} sets up a \code{ggplot} object with \code{theme} settings appropriate for
#' adding \code{geoms} from the \pkg{humapr} package. A \code{ggplot} object, it's
#' compatible with standard \pkg{ggplot2} function, e.g., \code{theme()} and
#' \code{facet_wrap()}. Doesn't really do anything on its own.
#'
#' @param mapping \code{aes()} object, just like other \code{ggplot2} functions, with two
#'   aesthetics: \code{loc} is the name of the data frame column containing localisation
#'   codes for observations; and \code{side} the column holding the laterality data (must
#'   be either "left" or "right, but see \code{bridge_side}). It is possible to supply a
#'   \code{fill} argument to use pre-computed summary statistics (see below).
#' @param data data frame, like the type you'd feed into \code{ggplot()} when producing a
#'   histogram. Is expected to be tidy, but can contain a summary statistic; in that
#'   case, you must specify a \code{fill} aesthetic and set \code{stat = "identity"} in
#'   the call to the \code{geom}.
#'
#' @return A ggplot object with suitable layout settings for the purpose of
#'   \code{humap_body}.
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
