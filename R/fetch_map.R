fetch_map <- function(type, gender, proj, half) {
    mapname <- sprintf("%s_%s_%s", type, gender, proj)
    h_env$map <- humapr:::maps[[mapname]]$map # maps object is in R/sysdata.rda
    h_env$mapdf <- humapr:::maps[[mapname]]$mapdf # data frame with grouped polygon coordinates

    h_env$pids <- as.data.frame(h_env$map)$Layer %>% # polygon ids
        setNames(seq(.))

    # Make sure user-supplied regions in "combine" are valid
    if (!is.null(h_env$combine)) test_combined(h_env$half, h_env$combine, h_env$pids)

    h_env$regions <- grep("_outline", h_env$pids, value = TRUE, invert = TRUE)
}