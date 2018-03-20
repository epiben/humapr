distribute_coords <- function(coords, pad, type = "smart", sort = TRUE) {
    # type %in% c("minimal", "even", "smart")
    if (type == "smart") {
        x0s <- setNames(coords$x0, row.names(coords))
        if (!length(unique(x0s)) == length(x0s)) x0s <- jitter(x0s, 1, 1)
        x0s <- sort(x0s)
        y0s <- setNames(coords$y0, row.names(coords))[names(x0s)]

        seek_vert <- function (d) { # d = differences
            # pos_top <- y0s[curr] > mean(c(min(tent), max(tent)))
            pos_top <- y0s[curr] > mean(range(tent))
            cp <- round(length(d) / 2) # cut point
            d <- d[if (pos_top) 1:cp else cp:length(d)]
            # This should be a bit more elegant; it shouldn't just cut the diffs
            # in half, but actually find those with values over or below the mean between
            # max and min
            cands <- d[d >= 2 * pad]
            if (length(cands) == 0) { # must go outside the bounds of y1s so far
                y1 <- if (pos_top) max(tent) + pad else min(tent) - pad
            } else {
                cands <- tent[names(cands)] + pad
                ref <- min(abs(cands - y0s[curr]))[1]
                y1 <- cands[abs(cands - y0s[curr]) == ref][1]
            }
            unname(y1)
        }

        seek_hori <- function() {
            browser()
            refs <- tent[!names(tent) == curr] # y1s of all other points so far
            refs2 <- refs[abs(refs) >= abs(y0s[[curr]]) & abs(refs) <= abs(tent[curr])]
                # only points with y1s between the y0 and y1 of current point
            # if (length(refs) == 0) return(unname(x0s[curr]))
            # refs2 <- refs[if (y1s[[curr]] >= y0s[curr]) refs >= y0s[curr] else refs <= y0s[curr]]
            ref <- if(length(refs2) == 0) {
                sort(abs(abs(refs) - abs(tent[curr])))[1]
                # vertically closest point
            } else if (length(refs) == 0) { # when the first point (usually the hand)
                tent
            } else {
                sort(abs(abs(unlist(x0s)[names(refs)]) - abs(x0s[curr])))[1]
                # horisontally closest point
            }
            # ref <- if (length(refs2) == 0) refs[length(refs)] else refs2[length(refs2)]
            unname(x0s[names(ref)])
        }

        x1s <- list()
        y1s <- list()
        # Consider removing unname() and save as y1curr, and then in the end y1s[[curr]] <- unname(y1curr)
        for (i in seq(y0s)) {
            # Vertical distribution (= y1 value)
            curr <- names(y0s[i])
            tent <- c(unlist(y1s), y0s[curr]) # y1s so far ("tentative" vector)
            if (i == 1) { # first point shouldn't move
                y1s[[curr]] <- unname(y0s[curr])
            } else if (tent[curr] == max(tent)) {
                y1s[[curr]] <- if (y0s[curr] - max(tent[!names(tent) == curr]) >= pad) {
                    unname(y0s[curr])
                } else {
                    unname(max(tent[!names(tent) == curr]) + pad)
                }
            } else if (tent[curr] == min(tent)) {
                y1s[[curr]] <- if (min(tent[!names(tent) == curr]) - y0s[curr] >= pad) {
                    unname(y0s[curr])
                } else {
                    unname(min(tent[!names(tent) == curr]) - pad)
                }
            } else {
                diffs <- -1 * diff(sort(tent, decreasing = TRUE))
                nxt <- names(diffs)[which(names(diffs) == curr) + 1] # next point
                y1s[[curr]] <- if (diffs[curr] >= pad && diffs[nxt] >= pad) {
                    unname(y0s[curr])
                } else if (sum(diffs[c(curr, nxt)]) >= 2 * pad) {
                    if (diffs[nxt] > diffs[curr]) {
                        unname(y0s[curr] - (pad - diffs[curr]))
                    } else {
                        unname(y0s[curr] + (pad - diffs[nxt]))
                    }
                } else {
                    seek_vert(diffs)
                }
            }

            # Find break point
            x1s[[curr]] <- seek_hori()
        }

        browser()
        sort_key <- row.names(coords)
        o <- data.frame(x0 = x0s[sort_key], y0 = y0s[sort_key],
                        x1 = unlist(x1s)[sort_key],
                        y1 = unlist(y1s)[sort_key])
        o$x1 <- with(o, ifelse(!y1 == y0 & abs(y1 - y0) < abs(x1 - x0),
                               (x0 + ifelse(x1 - x0 < 0, -1, 1) * abs(y1 - y0)),
                               x1)) # To ensure all slopes >= 45 degrees

        o # Returning data frame with coords
    } else if (type == "minimal") {
        if (sort == TRUE) coords <- sort(coords, decreasing = TRUE)
        start_mid <- mean(c(min(coords), max(coords)))
        pad <- unname(pad)
        curr <- 1

        repeat {
            d_up <- if (curr == 1) {
                Inf
            } else {
                coords[curr - 1] - coords[curr]
            }
            d_down <- if (curr == length(coords)) {
                Inf
            } else {
                coords[curr] - coords[curr + 1]
            }

            if (d_down < 0) {
                coords[curr] <- coords[curr - 1] + 1.1 * pad
                curr <- 1
                next
            } else {
                if (d_down < pad) {
                    coords[curr] <- coords[curr] + (1.1 * pad - d_down)
                    curr <- 1
                    next
                } else if (d_up < pad) {
                    if (d_down > 2 * pad) {
                        coords[curr] <- coords[curr] + dir * (1.1 * pad - d_up)
                        curr <- 1
                        next
                    } else {
                        curr <- curr + 1
                        next
                    }
                } else if (curr == length(coords)) {
                    break
                } else {
                    curr <- curr + 1
                    next
                }
            }
        }
        return(coords - (mean(c(min(coords), max(coords))) - start_mid))
    } else if (type == "even") {
        coords <- sort(coords, decreasing = FALSE)
        range <- max(coords) - min(coords)
        step <- range / (length(coords) - 1)
        i <- seq(length(coords) - 1)
        coords[i] <- min(coords) + (i - 1) * step
        return(coords)
    } else {
        stop("Please, supply valid `type` argument.")
    }
}
