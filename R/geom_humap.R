GeomHumap <- ggplot2::ggproto("GeomHumap", Geom,
     required_aes = c("x", "y", "fill"),
     default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1, alpha = 1),
     draw_key = function (data, ...) draw_key_polygon(data, ...),
     draw_group = function(data, panel_scales, coord, ...) {
         # Transform data and append a label column to the data frame
         if (h_env$controls$mid_include)
             data <- dplyr::mutate(data, y = y / 2, count = count / 2)
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

geom_humap <- function(mapping = NULL, data = NULL, stat = "count",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomHumap, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
