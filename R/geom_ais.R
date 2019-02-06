#' geom_ais builds a body map using AIS coding directly
#'
#'
#' Use this function to draw a choropleth on a human body map. Use the arguments
#' to choose exactly what kind of map you want. If you don't supply a
#' \code{body_halves} argument humapr will default to \code{pool} mode.
#'
#' \code{geom_ais} is a variant of \code{\link{geom_body}}, so for details.
#'
#' @param mapping supply an \code{aes()} object, just like other \code{ggplot2}
#'   functions. \code{humapr::geom_*} can take to aesthetics: \code{loc} is the
#'   name of the data frame column containing localisation codes for
#'   observations; and \code{side} the column holding the laterality data (must
#'   be either "left" or "right, although see \code{bridge_side}).
#' @param with_back logical, should the plot include the back-side of the head, neck and trunk?
#' @inheritParams geom_body
#'
#' @return A layer object to a \code{humap} object.
#'
#' @export

geom_ais <- function(mapping = NULL, data = NULL, type = "simple", with_back = TRUE,
                      body_halves = NULL, annotate = "freq", bridge_loc = NULL,
                      bridge_side = NULL, combine = NULL, controls = NULL,
                      # Standard arguments to layer()
                      na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE, ...) {

    h_env$map_name <- "body_ais"

    # Safety moves and housekeeping
    if (missing(data)) stop("Please, include data.")
    vargs <- list(type = c("simple", "detailed"),
                  with_back = c(TRUE, FALSE))
    housekeeping(match.call()[-1], formals(), vargs)

    # Import relevant map (maps object in R/sysdata.rda)
    map_name <- sprintf("ais_%s", h_env$type)
    if (h_env$with_back) map_name <- paste0(map_name, "_complete")
    fetch_map(map_name)

    # Ensure valid user-supplied regions in "combine", if relevant
    test_combined()

    # Use built-in AIS bridge (by design)
    h_env$bridge_loc <- sprintf("ais_%s", h_env$type)
    data <- build_bridge(data, geom = "ais")

    # Add mapped_loc variable to user's data frame
    data <- generate_mapped_loc(data)

    # Generate (preliminary) data for annotations, if relevant
    map_symmetric <- if (h_env$with_back) FALSE else TRUE
    prep_annotations(data$mapped_loc, map_symmetric)
        # map_name might be useful later for pre-specified annotation coordinates

    # Update aes() object to reflect changes
    mapping <- update_mapping(mapping)

    ggplot2::layer(
        geom = GeomBody, mapping = mapping, data = data, stat = "count",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}