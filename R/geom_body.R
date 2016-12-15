GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord, coding_scheme) {
    coords <- coord$transform(data, panel_scales)
    s <- surface_env$surf # The surface_env environment and its surf object are created by humap() before it invoks ggplot
    for (i in seq(s@summary@numPaths - 1)) {
      s@paths[[i]]@rgb <- if (!is.na(coords[crosswalk[["simple"]][[i]], "fill"])) coords[crosswalk[["simple"]][[i]], "fill"] else "#FFFFFF"
    } # This loops takes less than 0.005 seconds
    grImport::pictureGrob(body) # Takes about 0.05 seconds
  }
)

geom_body <- function(mapping = NULL, data = NULL, stat = "count",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBody, mapping = mapping,data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
