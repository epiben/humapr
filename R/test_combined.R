test_combined <- function(h, combine, regions) {
    combine <- unlist(combine)
    test_locs <- paste0(if (h == "left") "left_" else "right_", combine)
    region_test <- is.na(match(test_locs, regions))
    if (any(region_test)) stop(paste0(combine[region_test],
                                      " is not a valid region for the specified map.",
                                      collapse = " "),
                               call. = FALSE)
}