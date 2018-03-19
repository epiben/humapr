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
                  proj = c("front", "back", "neutral"),
                  annotate = c("freq", "all", "none"))
    for (arg in names(vargs)) {
        if (!get(arg, h_env) %in% vargs[[arg]]) prompt_inv(arg, vargs[[arg]][1])
    }

    # Set necessary defaults, if none given by user
    h_env$anno_gp <- h_env$anno_gp %||% grid::gpar(col = "black", fontsize = 9)
    h_env$controls$na_fill <- h_env$controls$na_fill %||% "#FFFFFF"
    h_env$controls$outline_colour <- h_env$controls$outline_colour %||% "#343434"
    # h_env$controls$mid_include <- h_env$controls$mid_include %||% FALSE
    h_env$controls$mid_include <- FALSE
    if (h_env$body_halves %in% c("right", "left")) h_env$body_halves <- "join"
        # I'm disabling this whole half business, people need to give useful data

    # Test argument combinations, and make necessary changes
    if (is.null(h_env$side) & h_env$body_halves != "join") {
        # Pool left- and right-sided data if no way to discriminate them
        h_env$side <- "join"
        message("`side` argument missing. Defaults to side = \"join\".")
    }
    if (h_env$type == "simple") {
        # Ignore projection if type = "simple"
        h_env$proj <- "neutral"
        message("Projections not available for the simple body map; ignores `proj` argument.")
    }
}