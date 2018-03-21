distribute_coords <- function(coords, pad) {
    x0s <- setNames(coords$x0, row.names(coords))
    if (!length(unique(x0s)) == length(x0s)) {
        # If some x0s are the same, deterministically (alphabetically) add "jitter"
        # so it will always give the same result
        x0s <- x0s[sort(names(x0s))]
        for (i in seq(length(x0s) -1)) {
            x0s[i] <- x0s[i] - 1 * sum(x0s[i] == x0s[-(1:i)])
        }
    }
    x0s <- sort(x0s) # to make sure we start from the lowest x0 (= from lateral)
    y0s <- setNames(coords$y0, row.names(coords))[names(x0s)]

    # Helper function to seek appropriate y1 value (~ vertical offset) for curr
    seek_vert <- function (d) { # d = differences
        # Should new y1 seek empty room at the top or bottom?
        pos_top <- y0s[curr] > mean(range(tent))
        # Find indices of differences with y1 in the upper
        d_index <- tent[names(d)] > mean(range(tent[names(d)]))
        d <- d[d_index == pos_top] # choose just relevant differences
        cands <- d[d >= 2 * pad] # differences with enough room to fit another label
        if (length(cands) == 0) {
            # No label pair can fit a third between them, so y1 must go outside
            # the bounds of the tentative list
            y1 <- if (pos_top) max(tent) + pad else min(tent) - pad
        } else {
            # Re-define cands to include 1 padding size
            cands <- tent[names(cands)] + pad
            ref <- min(abs(cands - y0s[curr]))[1] # point yielding smallest offset
            # Index in case >1 candidate difference satisfy the criterion
            y1 <- cands[abs(cands - y0s[curr]) == ref][1]
        }
        unname(y1) # return unnamed to yield a simple "scalar"
    }

    # Helper function to seek appropriate x1 value (~ horisontal offset) for curr
    seek_hori <- function() {
        # y1s of all other points so far
        refs <- tent[!names(tent) == curr]
        # y1s between the y0 and y1 of curr
        refs2 <- refs[dplyr::between(refs, y0s[[curr]], y1s[[curr]])]
        # ref is the point whose x0 value is used as x1 for curr
        ref <- if(length(tent) == 1) { # when the first point (= most lateral)
            tent # at this point, tent just has one element
        } else if (length(refs2) == 0) {
            # Use vertically-closest point of curr's y1
            sort(abs(refs - y1s[[curr]]))[1]

        } else {
            # Use horisontally-closest point
            sort(abs(unlist(x0s)[names(refs2)] - x0s[curr]))[1]
        }
        # Return the appropriate x0 value, unnamed to yield simple "scalar"
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
        # Find x coordinate of break point (= corner) of line
        x1s[[curr]] <- seek_hori()
    }

    sort_key <- row.names(coords)
    o <- data.frame(x0 = x0s[sort_key], y0 = y0s[sort_key],
                    x1 = unlist(x1s)[sort_key],
                    y1 = unlist(y1s)[sort_key])
    o$x1 <- with(o, ifelse(!y1 == y0 & abs(y1 - y0) < abs(x1 - x0),
                           (x0 + ifelse(x1 - x0 < 0, -1, 1) * abs(y1 - y0)),
                           x1)) # To ensure all slopes >= 45 degrees
    o # Returning data frame with coords
}
