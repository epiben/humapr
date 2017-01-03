GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    data$label <- panel_scales$x.labels
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
        surf@paths[[id]]@rgb <- humapr_env$outline
    }

    if (humapr_env$annotate %in% c("all", "absolute", "relative")) {
    } else {
        grImport::pictureGrob(surf)
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
