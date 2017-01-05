find_midpoint <- function (path, region) {
    offsets <- c(face = 100)
    # offset <- offsets[grepl(names(offsets), path)]

    mid <- function (vec) 0.5 * (max(vec) + min(vec))
    c(x0 = mid(path@x), y0 = mid(path@y))
}
