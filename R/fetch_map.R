fetch_map <- function(map_name) {
    h_env$map <- maps[[map_name]]$map # SpatialPolygons object
    h_env$mapdf <- maps[[map_name]]$mapdf # df with grouped polygon coordinates
    h_env$pids <- as.data.frame(h_env$map)$Layer %>% # polygon ids
        setNames(seq(.))
    h_env$regions <- grep("_outline", h_env$pids, value = TRUE, invert = TRUE)
        # exclude potential outline polygons/lines from 'regions'
}