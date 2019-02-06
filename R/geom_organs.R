#' geom_organs builds a map of the human internal organs
#'
#'
#' Use this function to draw a choropleth on a human body map. Use the arguments
#' to choose exactly what kind of map you want. If you don't supply a
#' \code{body_halves} argument humapr will default to \code{pool} mode.
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
#' and bridge them to those of \code{geom_body}. The idea is simple: you supply
#' a named \code{list}, each element of which corresponds one region in the
#' \code{geom_body} map; the name of the element should be the name of the
#' region, and the element should be a character vector of values in your data
#' that correspond to that region. See vignette [add reference to vignette here]
#' for examples.
#'
#' If you want to combine several regions and map them as one, supply a named
#' \emph{list} in \code{combine}, following this logic: the name of each element
#' will be printed as the annotation text (if you so desire), and the element
#' must be a character vector specifying the names of \code{geom_body} regions
#' to map as one. Underscores in the list element names will be converted to
#' wide spaces. Make sure to \emph{not} use a name for a merged region that is
#' already used for another region; e.g., this is not allowed, as \code{hand}
#' already the name of another region: \code{list(hand = c("shoulder", "arm",
#' "elbow", "wrist"))}. You may, however, use a name of a region inside the
#' group, so this would be allowed: \code{list(hand = c("wrist", "hand"))}.
#'
#' @param mapping supply an \code{aes()} object, just like other \code{ggplot2}
#'   functions. \code{humapr::geom_*} can take to aesthetics: \code{loc} is the
#'   name of the data frame column containing localisation codes for
#'   observations; and \code{side} the column holding the laterality data (must
#'   be either "left" or "right, although see \code{bridge_side}).
#' @param data tidy data frame, like the type you'd feed into ggplot() when
#'   producing a histogram
#' @param type currently, only \code{"simple"} is available.
#' @param annotate CURRENTLY NOT AVAILABLE \code{"freq"} (defaults) shows only absolute and relative
#'   frequencies, \code{"all"} includes region names, and \code{NA} omits labels
#'   altogether. See Details for ways to fine-tune the apperance of annotations.
#' @param combine named \emph{list} of vectors naming the regions to be combined
#'   and mapped as one, e.g., \code{list(arm = c("shoulder", "arm", "elbow",
#'   "wrist"))}. See Details.
#' @param bridge_loc named \emph{list} specifying the bridge from your data
#'   format to the native format of \code{geom_body}, e.g., \code{list(head =
#'   c("head", "face", "scalp"))}. See Details.
#' @param na.rm logical indicating whether to remove missing data. Default is
#'   \code{FALSE}.
#' @param controls named \emph{list} of more specific parameters for fine-tuning
#'   the appereance of the humap. Currently, these controls are available (more
#'   will follow in the fuuture): \code{na_fill} should be a string or function
#'   specifying the fill of zero-count regions (default: "black"),
#'   \code{label_pad} a numeric defining the padding between labels, in percent
#'   of map height (default: \code{3.5}), \code{mid_include} a logical defining
#'   whether to include mid-line observations and split them equally between
#'   left and right (default: \code{FALSE}), and \code{round_counts} a logical
#'   indicating whether to round (up) half-counts in annotation labels when
#'   splitting mid-line observations between left and right (default:
#'   \code{FALSE}).
#' @param show.legend,inherit.aes,... like all other \code{geoms}.
#'
#' @return A layer object to a \code{humap} object.
#'
#' @export

geom_organs <- function(mapping = NULL, data = NULL, type = "simple", annotate = "freq",
                        bridge_loc = NULL, combine = NULL, controls = NULL,
                        # Standard arguments to layer()
                        na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE, ...) {

    h_env$map_name <- "internal_organs"
        # map_name is useful later for pre-specified annotation coordinates and other map-specific behaviour

    # Safety moves and housekeeping
    if (missing(data)) stop("Please, include data.")
    vargs <- list() # list(type = c("simple", "detailed"),
                  # with_back = c(TRUE, FALSE))
    formals_mod <- c(formals(), body_halves = "separate") # so it doesn't appear as argument, but works downstream
    housekeeping(match.call()[-1], formals_mod, vargs)

    # Import relevant map (maps object in R/sysdata.rda)
    fetch_map(h_env$map_name)

    # Ensure valid user-supplied regions in "combine", if relevant
    test_combined()

    # Apply bridge, if any
    data <- build_bridge(data, geom = "organs")

    # Add mapped_loc variable to user's data frame
    data <- generate_mapped_loc(data)

    # Generate (preliminary) data for annotations, if relevant
    prep_annotations(data$mapped_loc, symmetric_map = FALSE)

    # Update aes() object to reflect changes
    mapping <- update_mapping(mapping)

    ggplot2::layer(
        geom = GeomBody, mapping = mapping, data = data, stat = "count",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}