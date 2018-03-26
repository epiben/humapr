# Initial housekeeping in \code{humap} call
#
# Internal function. Sets up an environment with settings; tests argument values. I cannot think of any scenario in which a user would need to call this function
#
# user: user-given argument; essentially useful stuff from \code{match.call()}
# defs: default argument values; essentially useful stuff from \code{formals()}
# vargs: list of arguments with valid values, specific to each geom_*

housekeeping <- function(user, defs, vargs) {
    # h_env is an internal object in R/sysdata.rda, and is reset here
    rm(list = ls(envir = h_env), envir = h_env)

    # Sync user-supplied and default arguments
    defs[names(defs) %in% names(user)] <- user
    for (arg in names(defs)[names(defs) != "..."])
        assign(arg, eval(defs[[arg]]), h_env)

    # Check that user supplied aes() object
    if (is.null(user[[1]])) stop("Please, specify a mapping.")
    if (is.null(user[[1]]$loc)) stop("Please, specify a 'loc' aesthetic.")

    # Extract 'loc' and 'side' var from aes(), assign to h_env to prevent breaking other scripts
    h_env$loc <- as.character(user[[1]]$loc)
    h_env$side <- user[[1]]$side %||% NULL
    if (!is.null(h_env$side)) h_env$side <- as.character(h_env$side)

    if (exists("side", envir = h_env)) {
        if (is.null(h_env$side)) {
            # do nothing
        } else if (h_env$side == h_env$loc) {
            stop("Your loc and side aesthetics are the same; change one of them.",
                 call. = FALSE)
        } else if(is.null(h_env$body_halves)) {
            h_env$body_halves <- "separate"
        }
    }

    # Choose default and prompt user if invalid argument supplied
    for (arg in names(vargs))
        # Prompts and sets first of each vargs element as default
        if (!get(arg, h_env) %in% vargs[[arg]]) prompt_inv(arg, vargs[[arg]][1])

    # Check annotation settings
    valid_annotate <- c("freq", "all", NA, NULL)
    h_env$gp <- grid::gpar(col = "black", fontsize = 9)
    if (is.list(h_env$annotate)) {
        h_env$gp <- h_env$annotate[["gp"]] %||% h_env$gp # indexing to avoid partial matching
        h_env$gp_text <- h_env$annotate$gp_text %||% h_env$gp
        h_env$gp_lines <- h_env$annotate$gp_lines %||% h_env$gp
        h_env$annotate <- h_env$annotate$detail %||% "freq" # give default
    }
    h_env$annotate <- h_env$annotate %||% NA
    if (!h_env$annotate %in% valid_annotate)
        stop(sprintf("Make sure your 'annotate' argument is one of the following: %s",
                     paste0(valid_annotate, collapse = ", ")), call. = FALSE)

    # Set necessary defaults, if none given by user
    h_env$body_halves <- h_env$body_halves %||% "join"
    h_env$controls$na_fill <- h_env$controls$na_fill %||% "#000000"
    h_env$controls$outline_colour <- h_env$controls$outline_colour %||% "#343434"
    h_env$controls$mid_include <- h_env$controls$mid_include %||% FALSE
    h_env$controls$round_counts <- h_env$controls$round_counts %||% FALSE

    # Test argument combinations, and make necessary changes
    if (is.null(h_env$side) & h_env$body_halves != "join") {
        # Pool left- and right-sided data if no way to discriminate them
        h_env$body_halves <- "join"
        message("`side` aesthetic missing. Defaults to body_halves = \"join\".")
    }
    # if (h_env$type == "simple" && h_env$proj != "neutral") {
    #     # Ignore projection if type = "simple"
    #     h_env$proj <- "neutral"
    #     message("Projections not available for the simple body map; ignores `proj` argument.")
    # }
}