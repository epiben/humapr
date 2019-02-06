distribute_coords <- function(coords, pad) {
    if (nrow(coords) == 0)
        return(coords) # nothing to do, so just return the empty data frame untouched

    x0s <- setNames(coords$x0, coords$region)
    if (!length(unique(x0s)) == length(x0s)) {
        # If some x0s are the same, deterministically (alphabetically) add "jitter"
        # so it will always give the same result
        x0s <- x0s[sort(names(x0s))]
        for (i in seq(length(x0s) -1)) {
            x0s[i] <- x0s[i] - 1 * sum(x0s[i] == x0s[-(1:i)])
        }
    }
    x0s <- sort(x0s) # to make sure we start from the lowest x0 (= from lateral)
    y0s <- setNames(coords$y0, coords$region)[names(x0s)]

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
            # cands <- tent[names(cands)] + pad
            # ref and ref2 are the two points between which to fit curr
            ref <- names(sort(abs(cands - y0s[curr]))[1])
            ref2 <- names(d[which(names(d) == ref) - 1])
            # Distance between curr and the two reference points, minus padding
            ref_dists <- c(setNames(tent[curr] - (y1s[[ref]] + pad), ref),
                   setNames(tent[curr] - (y1s[[ref2]] - pad), ref2))
            # Find the reference point closer to curr
            closest_neighbour <- names(sort(abs(ref_dists))[1])
            y1 <- tent[curr] - ref_dists[closest_neighbour]
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
    for (i in seq(x0s)) {
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
            nxt <- names(diffs)[which(names(diffs) == curr) + 1] # next point(= below)
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

    region <- coords$region
    o <- data.frame(region, x0 = x0s[region], y0 = y0s[region],
                    x1 = unlist(x1s)[region], y1 = unlist(y1s)[region],
                    stringsAsFactors = FALSE) %>%
        # Join with original coords input, except old (x0, y0)
        dplyr::left_join(dplyr::select(coords, -x0, -y0), by = "region") %>%
        # Ensure all slopes >= 45 degrees
        dplyr::mutate(x1 = ifelse(!y1 == y0 & abs(y1 - y0) < abs(x1 - x0),
                                  (x0 + ifelse(x1 - x0 < 0, -1, 1) * abs(y1 - y0)),
                                  x1))
    o # Returning data frame with coords
}
