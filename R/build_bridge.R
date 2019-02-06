build_bridge <- function(d, geom = "body") { # function (d, bridge, type) {
    if (is.null(h_env$bridge_loc))
        return(d) # return unaltered data frame if no bridge_loc

    bridge <- h_env$bridge_loc # to simplify subsequent code
    regions <- if (h_env$map_name %in% c("internal_organs")) {
        h_env$regions
    } else {
        unique(rm_lr(h_env$regions))
    }

    # Apply bridge_side for built-it bridges?
    if (is.character(bridge)) {
        if (bridge %in% c("ais", "ais_simple")) {
            if (!is.list(h_env$bridge_side)) { # If list, user should have given correct info
                h_env$bridge_side <- list(left = c(2, 20:29), right = c(1, 10:19), mid = c(0, 3, 4, 6, 7, 30:33))
                # Used by generate_mapped_loc()
            }
        }
    }

    built_in <- list(
        # geom level
        body = list(
            # actual bridge level
            ais = list(
                head = 51:58, neck = 59, shoulder = 60, arm = 61, elbow = 62,
                forearm = 63, wrist = 64, hand = 65, fingers = 66, back = 68,
                flank = 69, chest = 70, abdomen = 71, buttocks = 72,
                genitalia = 73:74, hip = 75, thigh = 76, knee = 77, leg = 78,
                ankle = 79, foot = 80, toes = 81),
            ais_simple = list(
                head = 51:58, neck = 59, arm = 60:61, forearm = 62:63, hand = 64:66,
                chest = c(68, 70), abdomen = c(69, 71:74), thigh = 75:76, leg = 77:78,
                foot = 79:81),
            simple = list(
                head = 11:12, neck = 21:22, chest = 31:32, abdomen = 41:42,
                pelvis = 51:52, arm = 61:62, forearm = 64:65, hand = 71:72,
                leg = 81:82)),
        ais = list(
            ais_simple = list(
                '52' = 51:58, '61' = 60:61, '63' = 62:63, '65' = 64:66,
                '70' = c(68, 70), '71' = c(69, 71:74), '76' = 75:76, '78' = 77:78,
                '80' = 79:81))
    )

    if (bridge %in% names(built_in[[geom]])) bridge <- built_in[[geom]][[bridge]]

    if (!is.list(bridge))
        stop(paste0("Please, make sure to give a *list* as the 'bridge' argument, or choose one of the built-in options: ",
                    names(built_in), "."),
             call. = FALSE)

    d[, h_env$loc] <- as.character(d[, h_env$loc]) # make sure it's not a factor
    for (hreg in names(bridge))
        d[d[, h_env$loc] %in% bridge[[hreg]], h_env$loc] <- hreg

    valid_locs <- d[, h_env$loc] %in% c(regions, NA) # include NA, as it's not an error, just a missing value

    if (sum(!valid_locs) > 0) {
        message(sprintf("%s data points (of %s) could not be associated with a region in the chosen map; they will be ignored.",
                        sum(!valid_locs), nrow(d)))
        message(sprintf("Problematic values in '%s' variable: %s.",
                        h_env$loc, paste0(unique(d[!valid_locs, h_env$loc]), collapse = ", ")))
    }

    # Return updated d, with non-valid region converted to NA
    valid_regions <- d[, h_env$loc] %in% regions
    dplyr::mutate(d, !!h_env$loc := ifelse(valid_regions, !!as.symbol(h_env$loc), NA))
    # d[converted, , drop = FALSE] # Return updated data frame
}