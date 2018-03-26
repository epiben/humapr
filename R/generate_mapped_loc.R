generate_mapped_loc <- function(d) { # function(d, loc, side, bridge_side, regions, h, combine) {
    loc <- h_env$loc
    side <- h_env$side
    bridge_side <- h_env$bridge_side
    regions <- h_env$regions
    h <- h_env$body_halves
    combine <- h_env$combine

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

    # d[is.na(d[, loc]), ] <- NA # to remove NAs

    # Generate "loc_long" based on user inputs
    if (h == "join") {
        d$loc_long <- if (!is.null(side)) {
            ifelse(d[, side] %in% c("left", "right"), as.character(d[, loc]), NA)
        } else {
            as.character(d[, loc])
        }
        h_env$regions <- paste0("right_", unique(rm_lr(regions)))
    } else if (h %in% c("left", "right")) { # disabled, will be removed later on
    #     d <- dplyr::filter(d, side == h) # To keep only relevant observations in d
    #     d$loc_long <- ifelse(d[, side] == h,
    #                          as.character(paste0(h, "_", d[, loc])),
    #                          NA)
    #     h_env$regions <- grep(paste0(h, "_"), regions, value = TRUE)
    } else {
        d$loc_long <- ifelse(d[, side] %in% c("left", "right"),
                             as.character(paste0(d[, side], "_", d[, loc])),
                             NA)
    }

    # Make appropriate modifications if some regions are combined
    if (!is.null(combine)) {
        combine_key <- list()
        for (new_loc in names(combine)) {
            for (old_loc in combine[[new_loc]]) {
                if (h == "join") {
                    combine_key[[old_loc]] <- new_loc
                } else {
                    combine_key[[paste0("right_", old_loc)]] <- paste0("right_", new_loc)
                    combine_key[[paste0("left_", old_loc)]] <- paste0("left_", new_loc)
                }
            }
        }
        # It may be worthwhile to use a simple data frame instead to get around the names(combine_key) thing
        h_env$combine_key <- do.call(c, combine_key)
        d$mapped_loc <- ifelse(d$loc_long %in% names(h_env$combine_key),
                               h_env$combine_key[as.character(d$loc_long)],
                               d$loc_long)
    } else {
        d$mapped_loc <- d$loc_long
    }

    # Make sure mapped_loc is NA when necessary
    d$mapped_loc <- ifelse(is.na(d[, h_env$loc]), NA, d$mapped_loc)

    # Find missing values, and notify user if any such found
    missing_mapped_loc <- is.na(d$mapped_loc)
    if (sum(missing_mapped_loc) > 0) {
        missing_loc <- is.na(d[, loc])
        missing_side <- is.na(d[, side])
        message(sprintf("Procesing your data yielded %s missing values: %s from the '%s' aesthetic, %s from the '%s' aesthetic, and %s from both.",
            sum(missing_mapped_loc), sum(missing_loc & !missing_side), loc, sum(missing_side & !missing_loc),
            side, sum(missing_loc & missing_side)))
    }

    # Return the updated data frame, with missing values removed
    d[!missing_mapped_loc, , drop = FALSE]
}