GeomBody <- ggplot2::ggproto("GeomBody", Geom,
     required_aes = c("x", "y", "fill"),
     default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1, alpha = 1),
     draw_key = function (data, ...) draw_key_polygon(data, ...),
     draw_group = function(data, panel_scales, coord, ...) {

         # Transform data and append a label column to the data frame
         if (h_env$controls$mid_include)
             data <- dplyr::mutate(data,
                                   y = if (h_env$controls$round_counts) ceiling(y / 2) else y / 2,
                                   count = if (h_env$controls$round_counts) ceiling(count / 2) else count / 2)
         coords <- coord$transform(data, panel_scales)
         data$label <- as.character(panel_scales$x$breaks) # as.character to remove attributes
         data <- dplyr::filter(data, !is.na(label))

         # Pick only mapped regions with counts > 0 (only used if user combined regions)
         if (!is.null(h_env$combine)) {
             for (combine_name in unique(h_env$combine_key)) {
                 data$label[data$label == combine_name] <-
                     names(h_env$combine_key[h_env$combine_key == combine_name])[1]
             }
         }

         # Labels need original data (i.e., before combining and expanding data df)
         label_data <- data

         # Fill all regions included in the combine statement
         if (!is.null(h_env$combine)) {
             new_data <- list(data)
             for (i in seq(nrow(data))) {
                 if (data[i, "label"] %in% names(h_env$combine_key)) {
                     old_locs <- names(h_env$combine_key[h_env$combine_key == h_env$combine_key[data[i, "label"]]])
                     for (old_loc in old_locs) {
                         # if (h_env$body_halves == "join") old_loc <- paste0(c("right_", "left_"), old_loc)
                         if (!data[i, "label"] == old_loc) {
                             new_data[[length(new_data) + 1]] <-
                                 dplyr::mutate(data[i, ], label = old_loc, y = 0, count = 0, prop = 0)
                         }
                     }
                 }
             }
             data <- do.call(rbind, new_data)
         }

         # Prepare data for plotting and create labels, if requested by user
         if (h_env$body_halves == "join" & !h_env$map_name %in% c("internal_organs"))
             # Essentially, row-bind two modified versions of the 'data' df
             data <- dplyr::mutate(data, label = paste0("left_", label), y = 0, count = 0, prop = 0) %>%
                 rbind(dplyr::mutate(data, label = paste0("right_", label)))

         # Make "local" copies of map and mapdf objects, to simplify subsequent code
         mapdf <- h_env$mapdf

         # Give missing regions default NA fill
         data <- dplyr::distinct(dplyr::select(mapdf, id)) %>%
             dplyr::left_join(data, by = c("id" = "label")) %>%
             dplyr::mutate(label = id, fill = ifelse(is.na(fill), h_env$controls$na_fill, fill))

         # Yield fill colours for each polygon
         fill_df <- dplyr::filter(mapdf, !duplicated(id)) %>%
             dplyr::left_join(data, by = c("id" = "label")) %>%
             dplyr::arrange(layer_order)

         # Start building the output, first off is the polygon map
         m <- grid::polygonGrob(x = grid::unit(mapdf$long, "native"), y = grid::unit(mapdf$lat, "native"),
                                gp = grid::gpar(col = fill_df$colour,
                                                fill = scales::alpha(fill_df$fill, fill_df$alpha),
                                                lwd = fill_df$size),
                                id = mapdf$layer_order)

         # Define x and y scales, as they're used repeatedly in the code
         xscale <- range(mapdf$long)
         yscale <- range(mapdf$lat)

         # If the user wants annotations, the relevant grobs are made here
         if (h_env$annotate %in% c("all", "freq")) {

             if (h_env$map_name %in% c("internal_organs")) {
                 local_coords <- dplyr::mutate(h_env$anno_coords, label = region)
             } else {
                 local_coords <- dplyr::mutate(h_env$anno_coords, label = rm_lr(region))
             }

             # Find the lines margin (i.e., where the lines end)
             lines_margin <- max(diff(xscale) / 10, min(local_coords$x1) - xscale[1], max(local_coords$x1) - xscale[2])

             # Align x2 values with left/right margins
             local_coords$x2 <- ifelse(local_coords$label_side == "left", xscale[1] - lines_margin, xscale[2] + lines_margin)

             # Define temp_labels to extract only relevant annotation coordinates
             temp_labels <- switch(h_env$body_halves,
                                   "join" = paste0("right_", label_data$label),
                                   label_data$label)
             local_coords <- dplyr::filter(local_coords, region %in% temp_labels)

             # Prepare data to feed into humap_vp()
             li_margin <- list(main = 2 * lines_margin, map = lines_margin * c(-1, 1))

             # Adjust yscale to make sure all annotations fit
             yscale <- c(min(yscale[1], min(local_coords$y1)),
                         max(yscale[2], max(local_coords$y1)))

             # Create lines grob
             lines <- grid::polylineGrob(x = line_coords(local_coords, c("x0", "x1", "x2")),
                                         y = line_coords(local_coords, c("y0", "y1", "y1")),
                                         default.units = "native", id.lengths = rep(3, nrow(local_coords)),
                                         gp = h_env$gp_lines)

            labels <- lapply(label_data$label, make_label, data = label_data, local_coords = local_coords)
            labels <- do.call(grid::grobTree, labels)

            # Find longest label, and use it to define the lateral margins
            label_text <- sapply(labels$children, function(.) .$label)
            long_label <- label_text[order(nchar(label_text), decreasing = TRUE)][1] # indexing in case of ties

            # Return final grob tree with appropriate viewport
            map_vp <- humap_vp(xscale, yscale, li_margin, long_label)
            grid::grobTree(m, lines, labels, vp = map_vp)
         } else {
             li_margin <- list(main = 0, map = c(0, 0))
             map_vp <- humap_vp(x_range = xscale, y_range = yscale, li_margin = li_margin, long_label = "")
             grid::grobTree(m, vp = map_vp)
         }
     }
)

#' Add normal body map to your \code{humap}
#'
#'
#' Use this function to draw a choropleth on a human body map.
#'
#' The \code{annotate} argument may be a named \code{list} with any combination of the following four elements:
#' \itemize{
#'   \item \code{details}: a character string, and may be any of the valid values of the simple argument
#' (\code{"freq"}, \code{"all"}, \code{NA})
#'   \item \code{gp}: must be a \code{gpar} object (see \code{?grid::gpar}). If only \code{gp} is specified, it is applied to both
#' annotation text and lines.
#'   \item \code{gp_text}: must be a \code{gpar} object, and is applied to annotation text.
#'   \item \code{gp_lines}: must be a \code{gpar} object, and is applied to annotation lines
#' }
#'
#' The \code{bridge} argument allows you to use your own localisation values, and bridge them to those of
#' \code{geom_body}. The idea is simple: you supply a named \code{list}, each element of which corresponds one region in
#' the \code{geom_body} map; the name of the element should be the name of the region, and the element value a
#' character vector of values in your data that correspond to that region. See vignette [add reference to vignette here]
#' for examples.
#'
#' If you want to combine several regions and map them as one, supply a named \emph{list} in \code{combine}, following
#' this logic: the name of each element will be printed as the annotation text (if you so desire), and the element must
#' be a character vector specifying the names of \code{geom_body} regions to map as one. Underscores in the list element
#' names will be converted to wide spaces. Make sure to \emph{not} use a name for a merged region that is already used
#' for another region; e.g., this is not allowed, because \code{hand} is already the name of another region: \code{list(hand =
#' c("shoulder", "arm", "elbow", "wrist"))}. You may, however, use a name of a region inside the group, e.g., \code{list(hand = c("wrist", "hand"))}.
#'
#' @param mapping \code{aes()} object, just like other \code{ggplot2} functions, with two aesthetics: \code{loc} is the
#'   name of the data frame column containing localisation codes for observations; and \code{side} the column holding
#'   the laterality data (must be either "left" or "right, but see \code{bridge_side}). In the future, we'll probably
#'   add a third aesthetic to allow the fill reflect other things than merely tabulation counts.
#' @param data tidy data frame, like the type you'd feed into \code{ggplot()} when producing a histogram.
#' @param type currently, only \code{"simple"} is available, but we're working on more natural-looking and sex-specific
#'   maps as well.
#' @param proj \code{"front"} or \code{"back"}. Ignored if \code{type = "simple"}, so currently not in use.
#' @param body_halves character string defining how to deal with body halves. \code{"separate"} (default with side
#'   aesthetic) discriminates the left half from the right; \code{"join"} (default without side aesthetic) merges
#'   observations in, e.g., right and left side of the chest.
#' @param annotate \code{"freq"} (defaults) shows only absolute and relative frequencies, \code{"all"} includes region
#'   names, and \code{NA} omits labels altogether. See Details for ways to fine-tune the apperance of annotations.
#' @param combine named \emph{list} of vectors naming the regions to be combined and mapped as one, e.g., \code{list(arm
#'   = c("shoulder", "arm", "elbow", "wrist"))}. See Details for behaviour.
#' @param bridge_loc named \emph{list} specifying the bridge from your data format to the native format of
#'   \code{geom_body}, e.g., \code{list(head = c("head", "face", "scalp"))}. See Details.
#' @param bridge_side named \emph{list}, specyfing the bridge from your laterality values for each of the native values
#'   of \code{geom_body}, e.g. \code{list(left = c("left", "l"), right = c("right", "r"), mid = c("mid", "m"))}. The
#'   \code{mid} element is only required if mid-line observation are included, see the \code{controls} argument.
#' @param na.rm logical indicating whether to remove missing data. Default is \code{FALSE}.
#' @param controls named \emph{list} of more specific parameters for fine-tuning the appereance of the humap. Currently,
#'   these controls are available (more will follow in the future):
#'   \itemize{
#'     \item \code{na_fill}:  should be a string or function specifying the fill of zero-count regions (default: "black")
#'     \item \code{label_pad}: a numeric defining the padding between labels, in percent of map height (default: \code{3.5})
#'     \item \code{mid_include}: a logical defining whether to include mid-line observations and split them equally between left and right (default: \code{FALSE})
#'     \item \code{round_counts}: a logical indicating whether to round (up) half-counts in annotation labels when splitting mid-line observations between left and right (default: \code{FALSE})
#'   }
#' @param show.legend,inherit.aes,... passed on to the underlying machinery of the \code{geom}.
#'
#' @return A layer object to be added to a \code{humap} object.
#'
#' @export

geom_body <- function(mapping = NULL, data = NULL, body_halves = NULL, annotate = "freq", bridge_side = NULL,
                      bridge_loc = NULL, combine = NULL, controls = NULL, type = "simple", proj = "neutral",
                      # Standard arguments to layer()
                      na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE, ...) {

    h_env$map_name <- "body_simple"

    # Safety moves and housekeeping
    if (missing(data)) {
        if (is.null(h_env$data)) {
            stop("Please, include data.")
        } else {
            data <- h_env$data
        }
    }
    vargs <- list(type = c("simple", "female", "male"),
                  proj = c("front", "back", "neutral"))
    housekeeping(match.call()[-1], formals(), vargs)

    # Import relevant map (maps object in R/sysdata.rda)
    h_env$map_name <- sprintf("%s_%s", h_env$type, h_env$proj)
    fetch_map(h_env$map_name)

    # Ensure valid user-supplied regions in "combine", if relevant
    test_combined()

    # Convert user formats with bridge argument, if relevant
    data <- build_bridge(data)

    # Add mapped_loc variable to user's data frame
    data <- generate_mapped_loc(data)

    # Generate (preliminary) data for annotations, if relevant
    prep_annotations(data$mapped_loc, h_env$map_name)
        # map_name might be useful later for pre-specified annotation coordinates

    # Update aes() object to reflect changes
    mapping <- update_mapping(h_env$mapping)

    ggplot2::layer(
        geom = GeomBody, mapping = mapping, data = data, stat = "count",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
