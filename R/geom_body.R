GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    data$label <- panel_scales$x.labels[data$x]
    surf <- humapr_env$surf

    shade_surf <- function(path, colour) surf@paths[[path]]@rgb <<- colour
    if (humapr_env$half == "mirror") body(shade_surf) <- quote(surf@paths[[paste0("left_", path)]]@rgb <<- surf@paths[[paste0("right_", path)]]@rgb <<- colour)

    for (i in seq(nrow(data))) {
        if (data[i, "label"] %in% humapr_env$comb_key) {
            for (old_loc in names(humapr_env$comb_key[humapr_env$comb_key == data[i, "label"]])) {
                shade_surf(old_loc, coords[i, "fill"])
            }
        } else {
            shade_surf(data[i, "label"], coords[i, "fill"])
        }
    }

    for (loc in humapr_env$regions[!humapr_env$regions %in% data$label]) {
        shade_surf(loc, "#FFFFFF")
    }
    for (id in grep("_outline", humapr_env$path_ids, value = TRUE)) {
        surf@paths[[id]]@rgb <- humapr_env$outline_colour
    }

    surf_grob <- grImport::pictureGrob(surf)
    humapr_env$vp_scales <- list(x = surf_grob$childrenvp[[2]]$xscale,
                                 y = surf_grob$childrenvp[[2]]$yscale)

    if (humapr_env$annotate) {
        # Pick only mapped regions with counts > 0
        mid_id <- if (humapr_env$half == "mirror") paste0("right_", data$label) else data$label

        # Generate coordinates for annotations
        anno_coords <- data.frame(row.names = mid_id,
            side = ifelse(grepl("left_", mid_id), "left", "right"),
            x.start = ifelse(grepl("left_", mid_id), surf@summary@xscale[2] + 500, -500),
            x.end = humapr_env$mids[mid_id, "xm"],
            y.start = humapr_env$mids[mid_id, "ym"],
            y.end = humapr_env$mids[mid_id, "ym"],
            stringsAsFactors = FALSE)

        # Generate lines grob
        lines <- segmentsGrob(x0 = anno_coords$x.start,
                              y0 = anno_coords$y.start,
                              x1 = anno_coords$x.end,
                              y1 = anno_coords$y.end,
                              default.units = "native",
                              vp = surf_grob$childrenvp)

        # Generate label grob
        make_label <- function (count, side, line_x, line_y) {
            rel <- signif(count / humapr_env$N * 100, 1)
            label <- paste0(count, " (", rel, "%)")
            x_coord <- if (side == "left") line_x + 150 else line_x - 150
            textGrob(label, x = x_coord,
                     y = unit(line_y, "native") + unit(0.1, "lines"),
                     default.units = "native", just = side,
                     vp = surf_grob$childrenvp)
        }

        labels <- list()
        for (id in mid_id) labels[[id]] <- make_label(data[data$label == id, "count"],
            anno_coords[id, "side"], anno_coords[id, "x.start"],
            anno_coords[id, "y.start"])
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
