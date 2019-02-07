fetch_map <- function(map_name) {
    h_env$mapdf <- maps[[map_name]]$mapdf # df with grouped polygon coordinates
    h_env$pids <- maps[[map_name]]$pid
    h_env$regions <- maps[[map_name]]$regions
    h_env$coords <- maps[[map_name]]$coords
}