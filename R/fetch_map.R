fetch_map <- function(mapname) {
    mapname <- sprintf("%s_%s", h_env$type, h_env$proj)
    h_env$map <- maps[[mapname]]$map # SpatialPolygons object
    h_env$mapdf <- maps[[mapname]]$mapdf # df with grouped polygon coordinates
    h_env$pids <- as.data.frame(h_env$map)$Layer %>% # polygon ids
        setNames(seq(.))
    h_env$regions <- grep("_outline", h_env$pids, value = TRUE, invert = TRUE)
        # exclude potential outline polygons/lines from 'regions'
}