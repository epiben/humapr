GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
      browser()
    coords <- coord$transform(data, panel_scales)

    shade_surf <- function(path, colour) {}
        body(shade_surf) <- if (humapr_env$half == "mirror") {
            quote(surf@paths[[paste0("left_", path)]]@rgb <<- surf@paths[[paste0("right_", path)]]@rgb <<- colour)
        } else {
            quote(surf@paths[[path]]@rgb <<- colour)
        }
    surf <- humapr_env$surf
    for (i in seq(humapr_env$mapped_loc)) {
        # This can be simplified a lot by using panel_scales$x.labels to find the mapped locs
        # - will also simplify the humap() function
        colour <- if (!is.na(coords[i, "fill"])) coords[i, "fill"] else "#FFFFFF"
        region <- humapr_env$mapped_loc[i]
        if (region %in% humapr_env$comb_key) {
            for (old_loc in names(humapr_env$comb_key[humapr_env$comb_key == region])) {
                shade_surf(old_loc, colour)
            }
        } else {
            shade_surf(region, colour)
        }
    }
    for (loc in humapr_env$unused_loc) {
        shade_surf(loc, "#FFFFFF")
    }

    if (humapr_env$half == "mirror") {
        shade_surf("outline", humapr_env$outline)
    } else {
        for (id in grep("outline", humapr_env$path_ids, value = TRUE)) {
            shade_surf(id, humapr_env$outline)
        }
    }

    if (humapr_env$annotate %in% c("all", "absolute", "relative")) {
    } else {
        grImport::pictureGrob(surf) # See if it's easier to use a gpar function to shade the paths...
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
