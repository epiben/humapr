test_combined <- function() {
    if (is.null(h_env$combine)) return() # stop here if no combinations supplied
    combine <- unlist(h_env$combine)
    test_locs <- paste0(if (h_env$body_halves == "left") "left_" else "right_", combine)
    region_test <- is.na(match(test_locs, h_env$regions))
    if (any(region_test))
        stop(paste0(combine[region_test], " not valid region(s) for the specified map.",
                    collapse = " "), call. = FALSE)
}