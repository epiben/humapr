# Things to add:
# - User-specified level of detail (e.g., draft, normal, publication, super), based on the level of detail when creating xml file
# - Choose gender (neutral, male, female)
# - Choose side of body (both, left right)
#    - Should left and right be the same ("mirrored"), or are data left-right speciic?
# - Perhaps half-male/half-female layout, as those categories are pretty fixed and might be of interest to users

GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    # There needs to be a cross-over vector to translate properly-formatted data to path id's in the xml file
    # Make sure that lighter colours, as default, have lower counts than darker 
    coords <- coord$transform(data, panel_scales)
    s <- surface_env$surf # The surface_env environment and its surf object are created by humap() before it invoks ggplot
    for (i in seq(s@summary@numPaths - 1)) {
      s@paths[[i]]@rgb <- if (!is.na(coords[i, "fill"])) coords[i, "fill"] else "#FFFFFF"
    } # This loops takes about 0.001 seconds
    grImport::pictureGrob(body) # Takes about 0.05 seconds
  }
)

geom_body <- function(mapping = NULL, data = NULL, stat = "count",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBody, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
