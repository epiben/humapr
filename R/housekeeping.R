#' Initial housekeeping in \code{humap} call
#'
#' Internal function. Sets up an environment with settings; tests argument values. I cannot think of any scenario in which a user would need to call this function
#'
#' @param user user-given argument; essentially useful stuff from \code{match.call()}
#' @param defs default argument values; essentially useful stuff from \code{formals()}

housekeeping <- function(user, defs) {
    # Sync user-supplied and default arguments
    defs[names(defs) %in% names(user)] <- user
    for (arg in names(defs)) assign(arg, eval(defs[[arg]]), h_env)

    # Choose default and prompt user if invalid argument supplied
    vargs <- list(type = c("simple", "female", "male"),
                  proj = c("front", "back", "neutral"))
    for (arg in names(vargs)) {
        if (!get(arg, h_env) %in% vargs[[arg]]) prompt_inv(arg, vargs[[arg]][1])
    }

    # Check annotation settings
    valid_annotate <- c("freq", "all", NA, NULL, FALSE)
    h_env$gp <- grid::gpar(col = "black", fontsize = 9)
    if (is.list(h_env$annotate)) {
        h_env$gp <- h_env$annotate$gp %||% h_env$gp
        h_env$annotate <- h_env$annotate$detail %||% "freq" # give default
    }
    if (is.null(h_env$annotate) | !h_env$annotate) h_env$annotate <- NA
    if (!h_env$annotate %in% valid_annotate)
        stop(sprintf("Make sure your 'annotate' argument is one of the following: %s",
                     paste0(valid_annotate, collapse = ", ")))

    # Set necessary defaults, if none given by user
    h_env$controls$na_fill <- h_env$controls$na_fill %||% "#FFFFFF"
    h_env$controls$outline_colour <- h_env$controls$outline_colour %||% "#343434"
    # h_env$controls$mid_include <- h_env$controls$mid_include %||% FALSE
    h_env$controls$mid_include <- FALSE
    if (h_env$body_halves %in% c("right", "left")) h_env$body_halves <- "join"
        # I'm disabling this whole half business, people need to give useful data

    # Test argument combinations, and make necessary changes
    if (is.null(h_env$side) & h_env$body_halves != "join") {
        # Pool left- and right-sided data if no way to discriminate them
        h_env$body_halves <- "join"
        message("`side` argument missing. Defaults to body_halves = \"join\".")
    }
    if (h_env$type == "simple") {
        # Ignore projection if type = "simple"
        h_env$proj <- "neutral"
        message("Projections not available for the simple body map; ignores `proj` argument.")
    }
}