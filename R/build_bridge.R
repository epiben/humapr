build_bridge <- function (d, bridge, type, lr.var, loc.var) {
    if (is.character(bridge)) {
        if (bridge == "ais") {
            if (!is.list(h_env$lr.var)) { # If list, user should have given correct info
                h_env$lr.var <- list(var = h_env$lr.var, left = c(2, 20:29),
                                     right = c(1, 10:19), mid = c(0, 3))
                # Used by generate_mapped_loc()
            }
        }
    }

    built_in <- list(
        ais = list(head = 51:58, neck = 59, shoulder = 60, arm = 61, elbow = 62,
                   forearm = 63, wrist = 64, hand = 65, fingers = 66, back = 68,
                   flank = 69, chest = 70, abdomen = 71, buttocks = 72,
                   genitalia = 73:74, hip = 75, thigh = 76, knee = 77, leg = 78,
                   ankle = 79, foot = 80, toes = 81))

    if (names(built_in) %in% bridge) bridge <- built_in[[bridge]]

    if (!is.list(bridge)) stop(paste0("Please, make sure to give a *list* as
                                      the 'bridge' argument, or choose one of
                                      the built-in options: ", names(built_in),
                                      "."),
                               call. = FALSE)

    for (hreg in names(bridge)) {
        d[d[, h_env$loc.var] %in% bridge[[hreg]], h_env$loc.var] <- hreg
    }

    valid_regions <-
        switch(type,
               body = c("head", "neck", "chest", "abdomen", "genitalia",
                        "shoulder", "arm", "elbow", "forearm", "wrist", "hand",
                        "fingers", "hip", "thigh", "knee", "leg", "ankle",
                        "foot", "toes"),
               stop("Invalid map chosen"))

    d[!d[, h_env$loc.var] %in% valid_regions, ] <- NA

    d # Return updated data frame
}