vadj <- function(coords, pad) {
    coords <- sort(coords, decreasing = TRUE)
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
    coords - (mean(c(min(coords), max(coords))) - start_mid)
}

plot_vadj <- function(testcoords) {
    testdf <- list(original = sort(testcoords, decreasing = TRUE))
    for (i in seq(50, 1000, length = 10)) {
        testdf[[paste0("X", i)]] <- vadj(testdf$original, i)
    }
    testdf <- do.call(cbind, testdf)
    testdf %>% melt() %>% ggplot(aes(x = Var2, y = value, colour = Var1)) + geom_point(position = position_jitter(height = 0, width = 0.3))
}

# This older version starts from the mid-coordinate and works it's way out from there.
# It's MUCH (≈ 100x) faster, but doesn't produce as nice a result
vadj_old <- function(coords, pad) {
    coords <- sort(coords, decreasing = TRUE)
    pad <- unname(pad)
    mid <- 0.5 * (length(coords) + 1)
    curr <- mid - 1

    repeat {
        dir <- if (curr < mid) -1 else 1
        d_up <- dir * coords[curr] - dir * coords[curr + dir] # upstream distance
        if (dir == -1) if(!length(d_up)) d_up <- Inf
        if (dir == 1) if(is.na(d_up)) d_up <- Inf
        d_down <- dir * coords[curr - dir] - dir * coords[curr] # downstream distance

        if (d_down < 0) {
            coords[curr] <- coords[curr - dir] - dir * 1.1 * pad
            curr <- mid + dir
            next
        } else {
            if (d_down < pad) {
                coords[curr] <- coords[curr] - dir * (1.1 * pad - d_down)
                curr <- mid + dir
                next
            } else if (d_up < pad) {
                if (d_down > 2 * pad) {
                    coords[curr] <- coords[curr] + dir * (1.1 * pad - d_up)
                    curr <- mid + dir
                    next
                } else {
                    curr <- curr + dir
                    next
                }
            } else if(curr == 1) {
                curr <- mid + 1
                next
            } else if (curr == length(coords)) {
                break
            } else {
                curr <- curr + dir
                next
            }
        }
    }
    coords
}
#
# plot_vadj <- function(testcoords) {
#     testdf <- list(original = sort(testcoords, decreasing = TRUE))
#     for (i in seq(50, 1000, length = 10)) {
#         testdf[[paste0("X", i)]] <- vadj(testdf$original, i)
#     }
#     testdf <- do.call(cbind, testdf)
#     testdf %>% melt() %>% ggplot(aes(x = Var2, y = value, colour = Var1)) + geom_point(position = position_jitter(height = 0, width = 0.3))
# }
