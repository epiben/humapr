#' Add organs map to your \code{humap}
#'
#'
#' Use this function to draw a choropleth on the internal organs of a human. The \code{bridge_loc}, \code{combine} and
#' \code{controls} arguments behave as for \code{\link{geom_body}}, so go to its Details section for more on how to use
#' these.
#'
#' The polygons in this geom overlap, so you can give them outlines with \code{colour} and \code{size} (just as you do
#' for, i.a., histogram and ribbon geoms) to make them easier to distinguish. By default, there is no outline.
#'
#' @inheritParams geom_body
#'
#' @return A layer object to a \code{humap} object.
#'
#' @export

geom_organs <- function(mapping = NULL, data = NULL,  annotate = "freq", bridge_loc = NULL, combine = NULL,
                        controls = NULL,
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
    fetch_map("all_organs")

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