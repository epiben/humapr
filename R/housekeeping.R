housekeeping <- function(user, defs) {
    # user = user-specified argument values
    # defs = default argument values

    # Set up new environment for the humap
    h_env <<- new.env(parent = emptyenv())

    # Syncing user-supplied and default arguments
    defs[names(defs) %in% names(user)] <- user
    for (arg in names(defs)) assign(arg, eval(defs[[arg]]), h_env)

    # If invalid argument supplied, default chosen and user prompted
    vargs <- list(type = c("body"),
                  proj = c("front", "back", "simple"),
                  annotate = c("freq", "all", "none"))
    for (arg in names(vargs)) {
        if (!get(arg, h_env) %in% vargs[[arg]]) prompt_inv(arg, vargs[[arg]][1])
    }
    h_env$anno_gp <- h_env$anno_gp %||% grid::gpar(col = "black",
                                                   fontsize = 9 / ggplot2::.pt)
    # Not sure this gives the desired result! Need to investigate more
    h_env$controls$na_fill <- h_env$controls$na_fill %||% "#FFFFFF"
    h_env$controls$outline_colour <- h_env$controls$outline_colour %||% "#343434"
    # h_env$controls$mid_include <- h_env$controls$mid_include %||% FALSE
    h_env$controls$mid_include <- FALSE
    if (h_env$half %in% c("right", "left")) h_env$half <- "mirror"
        # I'm disabling this whole half business, people need to give useful data

    if (is.null(h_env$lr.var) & h_env$half != "mirror") {
        h_env$half <- "mirror"
        message("`lr.var` argument missing. Defaults to half = \"mirror\".")
    }

    # Create viewport stack with appropriate margins
    # h_env$vps <- function(x_range, y_range, li_margin, longest_label, half) {
    #     la_margin <- grid::stringWidth(longest_label)
    #     the_layout <- function (x_range, y_range, li_margin, la_margin, half) {
    #         the_widths <- switch(half,
    #                              left = grid::unit.c(unit(diff(x_range) + li_margin$main, "null"), la_margin),
    #                              mirror = ,
    #                              right = grid::unit.c(la_margin, unit(diff(x_range) + li_margin$main, "null")),
    #                              grid::unit.c(la_margin, unit(diff(x_range) + li_margin$main, "null"),
    #                                           la_margin))
    #
    #         ncols <- if (half == "both") 3 else 2
    #         grid::grid.layout(1, ncols, widths = the_widths,
    #                           heights = diff(y_range), respect = TRUE)
    #     }
    #
    #     main <- grid::viewport(width = 0.9, height = 0.9,
    #                            layout = the_layout(x_range, y_range, li_margin,
    #                                                la_margin, half),
    #                            gp = h_env$anno_gp)
    #
    #     map <- grid::viewport(layout.pos.row = 1,
    #                           layout.pos.col = switch(half, left = 1, 2),
    #                           xscale = x_range + li_margin$map,
    #                           yscale = y_range)
    #
    #     grid::vpStack(main, map)
    # }
}