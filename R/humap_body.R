#' Create a humap body plot
#'
#' \code{humap_body} creates a choropleth body map, on which your data are
#' projected. \code{humapr} is an extension of ggplot2, so \code{humap_*}
#' objects are compatible with with standard \code{ggplot2} function, e.g.,
#' \code{theme()} and \code{facet_wrap()}.
#'
#' If you don't supply a \code{body_halves} argument humapr will default to
#' \code{pool} mode.
#'
#' The \code{annotate} argument may be a named \code{list} with any combination
#' of the following four elements. The \code{details} element should be a
#' character string, and may be any of the valid values of the simple argument
#' (\code{"freq"}, \code{"all"}, \code{NA}). The remaining three elements
#' \code{gp}, \code{gp_text} and \code{gp_lines} must be \code{gpar} objects
#' (see \code{?grid::gpar}). If only \code{gp} is specified, it is applied to
#' both annotation text and lines. \code{gp_text} and \code{gp_lines} are
#' applied to annotation text and lines individually.
#'
#' The \code{bridge} argument allows you to use your own localisation values,
#' and bridge them to those of \code{humap_body}. The idea is simple: you supply
#' a named \code{list}, each element of which corresponds one region in the
#' \code{humap_body} map; the name of the element should be the name of the
#' region, and the element should be a character vector of values in your data
#' that correspond to that region. See vignette [add reference to vignette here]
#' for examples.
#'
#' If you want to combine several regions and map them as one, supply a named
#' \emph{list} in \code{combine}, following this logic: the name of each element
#' will be printed as the annotation text (if you so desire), and the element
#' must be a character vector specifying the names of \code{humap_body} regions
#' to map as one. Underscores in the list element names will be converted to
#' wide spaces. Make sure to \emph{not} use a name for a merged region that is
#' already used for another region; e.g., this is not allowed, as \code{hand}
#' already the name of another region: \code{list(hand = c("shoulder", "arm",
#' "elbow", "wrist"))}. You may, however, use a name of a region inside the
#' group, so this would be allowed: \code{list(hand = c("wrist", "hand"))}.
#'
#' @param data tidy data frame, like the type you'd feed into ggplot() when
#'   producing a histogram
#' @param loc character string indicating the name of the data frame column
#'   containing localisation codes for observations.
#' @param side either character string or list. If \emph{string}, the argument
#'   should be the data frame column indicating left and right side of
#'   observations, and column values must be "left" or "right". If \emph{list},
#'   the \code{var} element must denote the name of the data frame column
#'   holding the data, and two \code{left} and \code{right} elements should be
#'   character vectors denoting what values in the data frame column correspond
#'   to those sides. Thus, for a data frame with the relevant column called
#'   'lr_col', valid specifications would include \code{"lr_col"} and
#'   \code{list(var = "lr_col", left = c("left", "l"), right = c("right",
#'   "r"))}.
#' @param type currently, only \code{"simple"} is available, but we're working
#'   on more natural-looking and gender-specific maps as well.
#' @param proj \code{"front"} or \code{"back"}. Ignored if \code{type =
#'   "simple"}, so currently not in use.
#' @param body_halves character string defining how to deal with body halves.
#'   \code{"separate"} (default) discriminates the left half from the right;
#'   \code{"join"} merges observations in, e.g., right and left side of the
#'   chest.
#' @param annotate \code{"freq"} (defaults) shows only absolute and relative
#'   frequencies, \code{"all"} includes region names, and \code{NA} omits labels
#'   altogether. See Details for ways to fine-tune the apperance of annotations.
#' @param combine a \emph{list} of vectors naming the regions to be combined and
#'   mapped as one, e.g., \code{list(arm = c("shoulder", "arm", "elbow",
#'   "wrist"))}. See Details.
#' @param bridge named \emph{list} specifying the bridge from your data format
#'   to the native format of \code{humap_body}, e.g., \code{list(head =
#'   c("head", "face", "scalp"))}. See Details.
#' @param na_rm logical indicating whether to remove missing data. Default is
#'   \code{FALSE}.
#' @param controls named \emph{list} of more specific parameters for fine-tuning
#'   the appereance of the humap. Currently, these controls are available (more
#'   will follow in the fuuture): \code{na_fill} should be a string or function
#'   specifying the fill of zero-count regions (default: "black"),
#'   \code{label_pad} a numeric defining the padding between labels, in percent
#'   of map height (default: \code{3.5}), \code{mid_include} a logical defining
#'   whether to include mid-line observations and split them equally between
#'   left and right (default: \code{FALSE}), and \code{round_counts} a logical
#'   indicating whether to round (up) half-counts in annotation labels when splitting
#'   mid-line observations between left and right (default: \code{FALSE}).
#'
#' @return A ggplot object with suitable layout settings for the purpose of
#'   \code{hump_body}.
#'
#' @export
#' @import ggplot2
#' @importFrom stats na.exclude na.omit setNames
#' @importFrom magrittr %>%

humap_body <- function(data, loc, side = NULL, type = "simple", proj = "neutral",
                       body_halves = "separate", annotate = "freq", bridge = NULL,
                       combine = NULL, na_rm = FALSE, controls = NULL) {

    # Safety moves and housekeeping
    if (missing(data)) stop("Please, include data.")
    if (missing(loc)) stop("Please, specify a 'loc'.")
    housekeeping(match.call()[-c(1, 2)], formals()[-1])
        # Forces controls$mid_include = FALSE (for now, let's see later)

    # Import relevant map (maps object in R/sysdata.rda)
    mapname <- sprintf("%s_%s", h_env$type, h_env$proj)
    h_env$map <- maps[[mapname]]$map # SpatialPolygons object
    h_env$mapdf <- maps[[mapname]]$mapdf # df with grouped polygon coordinates
    h_env$pids <- as.data.frame(h_env$map)$Layer %>% # polygon ids
        setNames(seq(.))
    h_env$regions <- grep("_outline", h_env$pids, value = TRUE, invert = TRUE)
        # exclude potential outline polygons/lines from 'regions'

    # Ensure valid user-supplied regions in "combine", if relevant
    if (!is.null(h_env$combine))
        test_combined(h_env$body_halves, h_env$combine, h_env$pids)

    # Convert user formats with bridge argument, if relevant
    if (!is.null(h_env$bridge))
        data <- build_bridge(data, h_env$bridge, h_env$type)

    # Add mapped_loc variable to user's data frame
    data <- generate_mapped_loc(data, h_env$loc, h_env$side,
                                h_env$regions, h_env$body_halves, h_env$combine)

    # Generate (preliminary) data for annotations, if relevant
    if (h_env$annotate %in% c("all", "freq"))
        prep_annotations(data$mapped_loc, h_env$combine, h_env$type,
                         h_env$gender, h_env$proj, h_env$body_halves)

    # Removing missing data, if so desired by user
    if (h_env$na_rm)
        data <- data[!is.na(data$mapped_loc), ]

    # Build ggplot object
    ggplot2::ggplot() +
        ggplot2::guides(fill = if (is.na(h_env$annotate)) NULL else FALSE) +
        geom_humap(aes(x = mapped_loc, fill = ..count.., group = 1), data,
                   stat = "count", na.rm = h_env$na_rm) +
        ggplot2::theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.line = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid = element_blank(),
                       panel.background = element_blank(),
                       legend.title = element_blank())
}
