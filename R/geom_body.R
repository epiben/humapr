GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    data$label <- panel_scales$x.labels[data$x]
    surf <- humapr_env$surf

    # Pick only mapped regions with counts > 0 (only used if user combined regions)
    label_ids <- if (humapr_env$half == "mirror") paste0("right_", data$label) else data$label
    if (!is.null(humapr_env$combine)) {
        for (comb_name in unique(humapr_env$comb_key)) {
            label_ids[label_ids == comb_name] <- data$label[data$label == comb_name] <- names(humapr_env$comb_key[humapr_env$comb_key == comb_name])[1]
        }
    }

    shade_surf <- function(path, colour) surf@paths[[path]]@rgb <<- colour
    if (humapr_env$half == "mirror") body(shade_surf) <- quote(surf@paths[[paste0("left_", path)]]@rgb <<- surf@paths[[paste0("right_", path)]]@rgb <<- colour)

    # First, make all regions white
    for (loc in humapr_env$regions) {
        shade_surf(loc, "#FFFFFF")
    }

    # Then, colour regions according to frequency
    for (i in seq(nrow(data))) {
        # I should really consider making a utility function for removing the left/right indication
        if (data[i, "label"] %in% names(humapr_env$comb_key)) {
            for (old_loc in names(humapr_env$comb_key[humapr_env$comb_key == humapr_env$comb_key[data[i, "label"]]])) {
                shade_surf(old_loc, coords[i, "fill"])
            }
        } else {
            shade_surf(data[i, "label"], coords[i, "fill"])
        }
    }

    # And finally, colour the outline paths
    for (id in grep("_outline", humapr_env$path_ids, value = TRUE)) {
        surf@paths[[id]]@rgb <- humapr_env$outline_colour
    }

    surf_grob <- grImport::pictureGrob(surf)
    humapr_env$vp_scales <- list(x = surf_grob$childrenvp[[2]]$xscale,
                                 y = surf_grob$childrenvp[[2]]$yscale)

    if (humapr_env$annotate) {
        # Generate coordinates for annotations
        local_coords <- humapr_env$anno_coords[label_ids, ]
        local_coords$side <-  ifelse(grepl("left_", label_ids), "left", "right")
        local_coords$x1 <- with(local_coords, ifelse(side == "left", x0 + abs(offset), x0 - abs(offset)))
        margin <- max(500, abs(min(local_coords$x1)))
        local_coords$x2 <- ifelse(local_coords$side == "left", surf@summary@xscale[2] + margin, -margin)

        find_coords <- function(df, xy) sapply(apply(local_coords[, paste0(xy, 0:2)], 1, c), c)
        lines <- grid::polylineGrob(x = find_coords(local_coords, "x"),
                                    y = find_coords(local_coords, "y"),
                                    default.units = "native",
                                    vp = surf_grob$childrenvp,
                                    id.lengths = rep(3, length(label_ids)))

        # Generate label grob
        make_label <- function (count, side, line_x, line_y) {
            rel <- signif(count / sum(data$count) * 100, 1)
            label <- paste0(count, " (", rel, "%)")
            x_coord <- if (side == "left") line_x + 150 else line_x - 150
            textGrob(label, x = x_coord,
                     y = unit(line_y, "native") + unit(0.1, "lines"),
                     default.units = "native", just = side,
                     vp = surf_grob$childrenvp)
        }

        labels <- list()
        for (id in label_ids) labels[[id]] <- make_label(data[data$label == id, "count"],
            local_coords[id, "side"], local_coords[id, "x2"],
            local_coords[id, "label_y"])
        labels <- do.call(grid::grobTree, labels)

        grid::grobTree(surf_grob, lines, labels, vp = viewport(gp = humapr_env$anno_gp))
    } else {
        surf_grob
    }
  }
)

geom_body <- function(mapping = NULL, data = NULL, stat = "count",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomBody, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
