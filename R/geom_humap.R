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
         data$label <- panel_scales$x.labels[data$x]
         data <- dplyr::filter(data, !is.na(label))

         # Pick only mapped regions with counts > 0 (only used if user combined regions)
         if (!is.null(h_env$combine)) {
             for (combine_name in unique(h_env$combine_key)) {
                 data$label[data$label == combine_name] <-
                     names(h_env$combine_key[h_env$combine_key == combine_name])[1]
             }
         }

         # Labels neded original data (i.e., before combining and expanding data df)
         label_data <- data

         # Here, we make sure to fill all regions included in the combine statement
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
         if (h_env$body_halves == "join")
             # Essentially, row-bind two modified versions of the 'data' df
             data <- dplyr::mutate(data, label = paste0("left_", label), y = 0,
                                   count = 0, prop = 0) %>%
                 rbind(dplyr::mutate(data, label = paste0("right_", label)))

         # Make "local" copies of map and mapdf objects, to simplify subsequent code
         map <- h_env$map
         mapdf <- h_env$mapdf

         # Give missing regions default NA fill
         data <- dplyr::left_join(as.data.frame(map), data, by = c("Layer" = "label")) %>%
             dplyr::mutate(label = Layer, fill = ifelse(is.na(fill), h_env$controls$na_fill, fill))

         # Yield fill colours for each polygon
         fill_df <- dplyr::filter(mapdf, !duplicated(id)) %>%
             dplyr::left_join(data, by = c("id" = "label"))

         # Start building the output, first off is the polygon map
         m <- grid::polygonGrob(x = grid::unit(mapdf$long, "native"),
                                y = grid::unit(mapdf$lat, "native"),
                                id = as.factor(mapdf$id),
                                gp = grid::gpar(col = fill_df$colour,
                                                fill = scales::alpha(fill_df$fill,
                                                                     fill_df$alpha)))

         # Define x and y scales, as they're used repeatedly in the code
         xscale <- sp::bbox(map)["x", ]
         yscale <- sp::bbox(map)["y", ]

         # If the user wants annotations, the relevant grobs are made here
         if (h_env$annotate %in% c("all", "freq")) {
             local_coords <- h_env$anno_coords %>%
                 dplyr::mutate(label = row.names(.),
                               side = ifelse(grepl("left_", label), "left", "right"))
             lines_margin <- max(diff(xscale) / 10,
                                 min(local_coords$x1) - xscale[1],
                                 max(local_coords$x1) - xscale[2])
             local_coords$x2 <- ifelse(local_coords$side == "left",
                                       xscale[2] + lines_margin,
                                       xscale[1] - lines_margin)
             local_coords <- switch(h_env$body_halves,
                                    right = , # uses the following (= join)
                                    join = subset(local_coords, side == "right"),
                                    left = subset(local_coords, side == "left"),
                                    local_coords)
             temp_labels <- if (h_env$body_halves == "join") {
                 paste0("right_", label_data$label)
             } else {
                 label_data$label
             }
             local_coords <- dplyr::filter(local_coords, label %in% temp_labels)

             # Prepare data to feed into humap_vp()
             li_margin <- list(main = 2 * lines_margin, map = lines_margin * c(-1, 1))

             # Adjust yscale to make sure all annotations fit
             yscale <- c(min(yscale[1], min(local_coords$y1)),
                         max(yscale[2], max(local_coords$y1)))

             # Create lines grob
             lines <- grid::polylineGrob(x = line_coords(local_coords, c("x0", "x1", "x2")),
                                         y = line_coords(local_coords, c("y0", "y1", "y1")),
                                         default.units = "native",
                                         id.lengths = rep(3, nrow(local_coords)),
                                         gp = h_env$gp_lines)

             # Create labels grob, using list with named elements
             labels <- sapply(label_data$label, function(.)
                 make_label(., label_data, local_coords), simplify = FALSE)
             labels <- do.call(grid::grobTree, labels)

             # Find longest label, and use it to define the lateral margins
             label_text <- sapply(labels$children, function(.) .$label)
             long_label <- label_text[order(nchar(label_text), decreasing = TRUE)][1]

             # Return final grob tree with appropriate viewport
             map_vp <- humap_vp(xscale, yscale, li_margin, long_label, h_env$body_halves)
             grid::grobTree(m, lines, labels, vp = map_vp)
         } else {
             map_vp <- humap_vp(x_range = xscale, y_range = yscale,
                                li_margin = list(main = 0, map = c(0, 0)),
                                long_label = "", h_env$body_halves)
             grid::grobTree(m, vp = map_vp)
         }
     }
)

#' geom_body builds a normal body map
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
#'   be either "left" or "right, although see \code{side_bridge}).
#' @param data tidy data frame, like the type you'd feed into ggplot() when
#'   producing a histogram
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
#' @param combine named \emph{list} of vectors naming the regions to be combined and
#'   mapped as one, e.g., \code{list(arm = c("shoulder", "arm", "elbow",
#'   "wrist"))}. See Details.
#' @param loc_bridge named \emph{list} specifying the bridge from your data
#'   format to the native format of \code{geom_body}, e.g., \code{list(head =
#'   c("head", "face", "scalp"))}. See Details.
#' @param side_bridge names \emph{list}, specyfing the bridge from your
#'   laterality values for each of the native values of \code{geom_body}, e.g.
#'   \code{list(left = c("left", "l"), right = c("right", "r"), mid = c("mid",
#'   "m"))}. The \code{mid} element is only required if mid-line observation are
#'   included, see the \code{controls} argument.
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

geom_body <- function(mapping = NULL, data = NULL, type = "simple", proj = "neutral",
                      body_halves = "separate", annotate = "freq", loc_bridge = NULL,
                      side_bridge = NULL, combine = NULL, controls = NULL,
                      # Standard arguments to layer()
                      na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE, ...) {

    # Safety moves and housekeeping
    if (missing(data)) stop("Please, include data.")
    if (is.null(mapping)) stop("Please, specify a mapping.")
    if (is.null(mapping$loc)) stop("Please, specify a 'loc'.")
    housekeeping(match.call()[-1], formals())

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
    if (!is.null(h_env$loc_bridge))
        data <- build_bridge(data, h_env$loc_bridge, h_env$type)

    # Add mapped_loc variable to user's data frame
    data <- generate_mapped_loc(data, h_env$loc, h_env$side, h_env$side_bridge,
                                h_env$regions, h_env$body_halves, h_env$combine)

    # Generate (preliminary) data for annotations, if relevant
    if (h_env$annotate %in% c("all", "freq"))
        prep_annotations(data$mapped_loc, h_env$combine, h_env$type,
                         h_env$gender, h_env$proj, h_env$body_halves)

    # Removing missing data, if so desired by user
    if (h_env$na.rm)
        data <- data[!is.na(data$mapped_loc), , drop = FALSE]

    # Update aes() object to reflect changes
    mapping$x <- as.symbol("mapped_loc")
    mapping$fill <- as.symbol("..count..")
    mapping$group <- 1
    mapping$loc <- mapping$side <- NULL

    ggplot2::layer(
        geom = GeomBody, mapping = mapping, data = data, stat = "count",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
