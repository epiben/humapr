generate_mapped_loc <- function(d) { # function(d, loc, side, bridge_side, regions, h, combine) {
    loc <- h_env$loc
    side <- h_env$side
    bridge_side <- h_env$bridge_side
    regions <- h_env$regions
    h <- h_env$body_halves
    combine <- h_env$combine
    map_name <- h_env$map_name

    # Convert from user's left/right/mid indication to those of humapr
    if (is.list(bridge_side)) {
        test <- c("left", "right")
        if (h_env$controls$mid_include)
            test <- c(test, "mid")
        if (!all(test %in% names(bridge_side)))
            stop("Please, supply valid 'side' argument.", call. = FALSE)
        d[!is.na(match(d[, side], bridge_side$left)), side] <- "left"
        d[!is.na(match(d[, side], bridge_side$right)), side] <- "right"
        if (h_env$controls$mid_include)
            d[!is.na(match(d[, side], bridge_side$mid)), side] <- "mid"
        d[, side] <- ifelse(d[, side] %in% test, d[, side], NA)
    }

    # Inelegant "hack" to allow 50% weight of mid-line observations on each side
    # This is disabled by forcing mid_include = FALSE in housekeeping.R
    if (h_env$controls$mid_include) {
        # Basically, we make the data twice as long as originally...
        d <- rbind(d, d[!d[, side] == "mid", ])
        for (x in c("left", "right")) {
            t <- d[d[, side] == "mid", ]
            t[, side] <- x
            d <- rbind(d, t)
        }
        d <- d[!d[, side] == "mid", ]
    }

    # Generate "loc_long" based on user inputs
    if (h == "join" & !h_env$map_name %in% c("internal_organs")) {
        d$loc_long <- if (!is.null(side)) {
            ifelse(d[, side] %in% c("left", "right"), as.character(d[, loc]), NA)
        } else {
            as.character(d[, loc])
        }
        h_env$regions <- paste0("right_", unique(rm_lr(regions)))
    } else if (map_name %in% c("internal_organs")) {
        d$loc <- as.character(d[, loc])
        d$loc <- ifelse(d$loc %in% regions, d$loc, NA)
        d$loc_long <- ifelse(is.na(d$loc), NA, d$loc)
    } else {
        d$loc_long <- ifelse(d[, side] %in% c("left", "right"), as.character(paste0(d[, side], "_", d[, loc])), NA)
    }

    # Make appropriate modifications if some regions are combined
    if (!is.null(combine)) {
        combine_key <- list()
        for (new_loc in names(combine)) {
            for (old_loc in combine[[new_loc]]) {
                if (h == "join") {
                    combine_key[[old_loc]] <- new_loc
                } else if (map_name %in% c("internal_organs")) { # lavish, but simpler for now
                    combine_key[[old_loc]] <- new_loc
                } else {
                    combine_key[[paste0("right_", old_loc)]] <- paste0("right_", new_loc)
                    combine_key[[paste0("left_", old_loc)]] <- paste0("left_", new_loc)
                }
            }
        }
        # It may be worthwhile to use a simple data frame instead to get around the names(combine_key) thing
        h_env$combine_key <- ckey <- do.call(c, combine_key)
        d$mapped_loc <- ifelse(d$loc_long %in% names(ckey), ckey[as.character(d$loc_long)], d$loc_long)
    } else {
        d$mapped_loc <- d$loc_long
    }

    # Make sure mapped_loc is NA when necessary
    d$mapped_loc <- ifelse(is.na(d[, loc]), NA, d$mapped_loc)

    # Find missing values, and notify user if any such found
    missing_mapped_loc <- is.na(d$mapped_loc)
    if (sum(missing_mapped_loc) > 0) {
        missing_loc <- if (is.null(side)) sum(is.na(d[, loc])) else sum(is.na(d[, loc]) & !is.na(d[, side]))
        missing_side <- sum(is.na(d[, side]) & !is.na(d[, loc]))
        missing_both <- sum(is.na(d[, loc]) & is.na(d[, side]))
        message(sprintf("Procesing your data yielded %s missing values:", sum(missing_mapped_loc)))
        if (missing_loc > 0) message(sprintf("- %s from '%s'", missing_loc, loc))
        if (missing_side > 0) message(sprintf("- %s from '%s'", missing_side, side))
        if (missing_both > 0) message(sprintf("- %s from both", missing_both))
    }

    # Return the updated data frame, with missing values removed
    d[!missing_mapped_loc, , drop = FALSE]
}