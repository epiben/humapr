GeomHumap <- ggproto("GeomHumap", Geom,
    required_aes = c("x", "y", "fill"),
    default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1, alpha = 1),
    draw_key = function (data, ...) draw_key_polygon(data/2, ...),
    draw_group = function(data, panel_scales, coord) {
        # Transform data and append a label column to the data frame
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

        # Labels should use original data (i.e., before combining and expanding data df)
        label_data <- data

        # Here, we make sure to fill all regions included in the combine statement
        if (!is.null(h_env$combine)) {
            new_data <- list(data)
            for (i in seq(nrow(data))) {
                if (data[i, "label"] %in% names(h_env$combine_key)) {
                    old_locs <- names(h_env$combine_key[h_env$combine_key == h_env$combine_key[data[i, "label"]]])
                    for (old_loc in old_locs) {
                        # if (h_env$half == "mirror") old_loc <- paste0(c("right_", "left_"), old_loc)
                        if (!data[i, "label"] == old_loc) {
                            new_data[[length(new_data) + 1]] <- data[i, ] %>%
                                dplyr::mutate(label = old_loc, y = 0, count = 0, prop = 0)
                        }
                    }
                }
            }
            data <- do.call(rbind, new_data)
        }

        # Prepping data for plotting and creating labels, if requested by user
        data <- dplyr::mutate(data, label = if (h_env$half == "mirror") paste0("right_", label) else label)
        if (h_env$half == "mirror") {
            data <- data %>%
                dplyr::mutate(label = paste0("left_", rm_lr(label)),
                       y = 0,
                       count = 0,
                       prop = 0) %>%
                rbind(data)
        }

        # Make "local" copies of map and mapdf objects
        map <- h_env$map
        mapdf <- h_env$mapdf

        # This gives missing regions the NA default fill
        data <- dplyr::left_join(as.data.frame(map), data, by = c("Layer" = "label")) %>%
            dplyr::mutate(
                fill = ifelse(is.na(fill), h_env$controls$na_fill, fill),
                label = Layer
            )

        # Used to fill the polygons
        first_rows <- mapdf[!duplicated(mapdf$id), ] %>%
            dplyr::left_join(data, by = c("id" = "label"))

        # Start building the output, first off is the map made of polygons
        m <- grid::polygonGrob(x = grid::unit(mapdf$long, "native"),
                               y = grid::unit(mapdf$lat, "native"),
                               id = as.factor(mapdf$id),
                               gp = grid::gpar(col = first_rows$colour,
                                               fill = scales::alpha(first_rows$fill,
                                                                    first_rows$alpha)))

        # Define x and y scales, as they're used repeatedly in the code
        xscale <- sp::bbox(map)["x", ]
        yscale <- sp::bbox(map)["y", ]

        # If the user wants annotations, the relevant grobs are made here
        if (h_env$annotate %in% c("all", "freq")) {
            # Generate coordinates for annotations
            if (!h_env$controls$vert_adj == "smart") {
                local_coords <- subset(h_env$anno_coords, !is.na(x0) & !is.na(offset))
                local_coords$side <-  ifelse(grepl("left_", local_coords$region), "left", "right")
                local_coords$x1 <- with(local_coords, ifelse(side == "left", x0 + abs(offset), x0 - abs(offset)))
            }
            if (h_env$controls$vert_adj == "smart") {
                local_coords <- h_env$anno_coords
                local_coords$side <-  ifelse(grepl("left_",
                                                   row.names(local_coords)),
                                             "left", "right")
                # local_coords$x0 <- with(local_coords, ifelse(side == "left", xscale[2] - x0, x0))
                # local_coords$x1 <- with(local_coords, ifelse(side == "left", xscale[2] - x1, x1))
            }
            label_pad <- diff(xscale) / 10
            lines_margin <- max(label_pad, min(local_coords$x1) - xscale[1],
                                max(local_coords$x1) - xscale[2]) # I think this is the right calculation now
            local_coords$x2 <- ifelse(local_coords$side == "left",
                                      xscale[2] + lines_margin,
                                      xscale[1] - lines_margin)
            local_coords <- switch(h_env$half,
                                   mirror = , # uses the following (= right)
                                   right = subset(local_coords, side == "right"),
                                   left = subset(local_coords, side == "left"),
                                   local_coords)
            # local_coords <- local_coords[row.names(local_coords) %in% data$label, ]
            temp_labels <- if (h_env$half == "mirror") paste0("right_", label_data$label) else label_data$label
            local_coords <- local_coords[row.names(local_coords) %in% temp_labels, ]

            # Prepare data to feed into vps()
            li_margin <- list(main = 2 * lines_margin,
                              map = lines_margin * c(-1, 1))
            if (h_env$half %in% c("right", "left")) {
                li_margin$main <- lines_margin
                li_margin$map <- lines_margin *
                    if (h_env$half == "right") c(-1, 0) else c(0, 1)
            }

            # Adjust the viewports in s to make room for annotations
            yscale <- c(min(yscale[1], min(local_coords$y1)),
                        max(yscale[2], max(local_coords$y1)))

            # Create lines grob
            lines <- grid::polylineGrob(x = line_coords(local_coords, c("x0", "x1", "x2")),
                                        y = line_coords(local_coords, c("y0", "y1", "y1")),
                                        default.units = "native",
                                        id.lengths = rep(3, nrow(local_coords)))

            # Create label grob
            labels <- list()
            for (id in label_data$label) labels[[id]] <- make_label(id, label_data,
                                                                    local_coords,
                                                                    label_pad)
            labels <- do.call(grid::grobTree, labels)

            # Finds the longest label, and use it to define the area for margins
            label_text <- list()
            for (lab in labels$children) label_text[[lab$name]] <- lab$label
            label_text <- do.call(c, label_text)
            long_label <- label_text[nchar(label_text) == max(nchar(label_text))][[1]]
            # h_env$longest_label <- if (!is.null(h_env$longest_label)) {
            #     if (nchar(long_label) > nchar(h_env$longest_label)) {
            #         long_label
            #     } else {
            #         h_env$longest_label
            #     }
            # } else {
            #     long_label
            # } Was trying to find a way to harmonise across panels...

            # Return final grob tree
            grid::grobTree(m, lines, labels, vp = h_env$vps(xscale, yscale,
                                                            li_margin,
                                                            long_label,
                                                            h_env$half))
        } else {
            the_vp <- h_env$vps(x_range = xscale, y_range = yscale,
                                li_margin = list(main = 0, map = c(0, 0)),
                                longest_label = "", h_env$half)

            grid::grobTree(m, vp = the_vp)
        }
    }
)

geom_humap <- function(mapping = NULL, data = NULL, stat = "sum",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {

  layer(
    geom = GeomHumap, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
