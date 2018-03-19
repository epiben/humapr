build_bridge <- function (d, bridge, type) {
    if (is.character(bridge)) {
        if (bridge %in% c("ais", "ais_simple")) {
            if (!is.list(h_env$side)) { # If list, user should have given correct info
                h_env$side <- list(var = h_env$side, left = c(2, 20:29),
                                     right = c(1, 10:19), mid = c(0, 3))
                # Used by generate_mapped_loc()
            }
        }
    }

    built_in <- list(
        ais = list(
            head = 51:58, neck = 59, shoulder = 60, arm = 61, elbow = 62,
            forearm = 63, wrist = 64, hand = 65, fingers = 66, back = 68,
            flank = 69, chest = 70, abdomen = 71, buttocks = 72,
            genitalia = 73:74, hip = 75, thigh = 76, knee = 77, leg = 78,
            ankle = 79, foot = 80, toes = 81),
        ais_simple = list(
            head = 51:58, neck = 59, arm = 60:62, forearm = 62:64, hand = 65:66, chest = c(68, 70),
            abdomen = c(69, 71:74), thigh = 75:77, leg = 78:79, foot = 80:81),
        simple = list(
            head = 11:12, neck = 21:22, chest = 31:32, abdomen = 41:42,
            pelvis = 51:52, arm = 61:62, forearm = 64:65, hand = 71:72,
            leg = 81:82)
    )

    if (bridge %in% names(built_in)) bridge <- built_in[[bridge]]

    if (!is.list(bridge))
        stop(paste0("Please, make sure to give a *list* as the 'bridge' argument, or choose one of the built-in options: ",
                    names(built_in), "."),
             call. = FALSE)

    for (hreg in names(bridge)) {
        d[d[, h_env$loc] %in% bridge[[hreg]], h_env$loc] <- hreg
    }

    converted <- d[, h_env$loc] %in% unique(rm_lr(h_env$regions))
    if (sum(!converted) > 0) {
        message(sprintf("%s data points (of %s) could not be mapped a region in the chosen map.",
                        sum(!converted), nrow(d)))
        message(springt("Problematic 'loc' values: %s.",
                        paste0(unique(d[!converted, h_env$loc]), collapse = ", ")))
    }

    d[converted, ] # Return updated data frame
}