fetch_map <- function(type, gender, proj, half) {
    mapname <- sprintf("%s_%s_%s", type, gender, proj)
    map <- humapr:::maps[[mapname]]$map # maps object is in R/sysdata.rda
    mapdf <- humapr:::maps[[mapname]]$mapdf # data frame with grouped polygon coordinates

    pids_all <- as.data.frame(map)$Layer # polygon ids
<<<<<<< HEAD
    names(pids_all) <- seq(1, length(pids_all)) # I think this suffices:
=======
    # names(pids_all) <- seq(1, length(pids_all)) # I think this suffices:
    names(pids_all) <- seq(pids_all)
>>>>>>> 49e738e489531271e35c6a05e9ea74120bb7c81d
    pids <- pids_all

    # Think should do the same in a simpler way
    pids <- as.data.frame(map)$Layer %>% # polygon ids
        setNames(seq(.))

    # Make sure user-supplied regions in "combine" are valid
    if (!is.null(h_env$combine)) test_combined(h_env$half, h_env$combine, pids)

    # Remove regions and update coordinates
    # if (half %in% c("right", "left")) {
    #     pids <- grep(paste0(half, "_"), pids_all, value = TRUE)
    #     regions_to_go <- pids_all[!pids_all %in% pids]
    #     map <- map[-as.numeric(names(regions_to_go)), ]
<<<<<<< HEAD
    # } # disabled, actually makes no sense

    h_env$regions <- grep("_outline", pids, value = TRUE, invert = TRUE)
    # h_env$pids <- pids # disabled, not relevant when half can't be chosen
    # h_env$map <- map # disabled, not relevant when half can't be chosen
    # h_env$mapdf <- mapdf # disabled, not relevant when half can't be chosen
=======
    # }
    h_env$regions <- grep("_outline", pids, value = TRUE, invert = TRUE)
    # h_env$pids <- pids
    # h_env$map <- map
    # h_env$mapdf <- mapdf
>>>>>>> 49e738e489531271e35c6a05e9ea74120bb7c81d
}