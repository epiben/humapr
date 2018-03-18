# Use default, if no other value specified
`%||%` <- function(a, b) if(!is.null(a)) a else b

line_coords <- function(df, cols) c(apply(df[, cols], 1, c))

# Remove "left_" and "right_" from vector of localizers
rm_lr <- function(x) substring(x, regexpr("_", x) + 1)

lr_conc <- function(x) c(paste0("left_", x), paste0("right_", x))

# Prompt on invalid argument
prompt_inv <- function(arg, val){
    message("Invalid `", arg, "` argument. Defaults to \"", val, "\".")
    assign(arg, val, h_env)
}

def_dist <- function (x, p = h_env$controls$label_pad, a = h_env$controls$vert_adj) {
    distribute_coords(x, p, a)
}

inverse_coords <- function (x, cols = "x0", patt = "left_") {
    patt <- grepl(patt, row.names(x))
    if (all(patt == FALSE)) return(x)
    xt <- x[patt, ]
    for (col in cols) xt[patt, col] <- max(xt[, cols]) - xt[, col] + min(xt[, cols])
    x[patt, ] <- xt
    na.omit(x)
}

line_coords <- function(df, cols) c(apply(df[, cols], 1, c))

# Function to make viewport able to handle the humap
vp <- function(x_range, y_range, li_margin, max_label_length, half) {
    la_margin <- grid::stringWidth(rep(" ", max_label_length))
        # Consider sending in all labels, and use grid::unit.pmax() on the string widths
    # Set up layout for the viewport with appropriate settings
    the_layout <- function (x_range, y_range, li_margin, la_margin, half) {
        the_widths <- switch(
            half,
            left = grid::unit.c(unit(diff(x_range) + li_margin$main, "null"), la_margin),
            mirror = ,
            right = grid::unit.c(la_margin, unit(diff(x_range) + li_margin$main, "null")),
            grid::unit.c(la_margin, unit(diff(x_range) + li_margin$main, "null"), la_margin))
        ncols <- if (half == "both") 3 else 2
        grid::grid.layout(1, ncols, widths = the_widths, heights = diff(y_range), respect = TRUE)
    }
    main <- grid::viewport(width = 0.9, height = 0.9, gp = h_env$anno_gp,
                           layout = the_layout(x_range, y_range, li_margin, la_margin, half))
    map <- grid::viewport(layout.pos.row = 1, layout.pos.col = switch(half, left = 1, 2),
                          xscale = x_range + li_margin$map, yscale = y_range)
    grid::vpStack(main, map)
}