# Use default, if no other value specified
`%||%` <- function(a, b) if(!is.null(a)) a else b
    # heavily inspired by the utility function used in the Tidyverse

line_coords <- function(df, cols) c(apply(df[, cols], 1, c))

# Remove "left_" and "right_" from vector of localizers
rm_lr <- function(x) substring(x, regexpr("_", x) + 1)

lr_conc <- function(x) c(paste0("left_", x), paste0("right_", x))

# Prompt on invalid argument
prompt_inv <- function(arg, val){
    message("Invalid `", arg, "` argument. Defaults to \"", val, "\".")
    assign(arg, val, h_env)
}

def_dist <- function (x, p = h_env$controls$label_pad) {
    distribute_coords(x, p)
}

inverse_coords <- function (x, cols = "x0") {
    # i = index vector of those to convert
    i <- x$x0 > mean(range(x$x0))
        # for symmetric plots, this will just be left-side annotations
    if (sum(i) == 0) return(x)
    # temp_c <- coords[to_invert, ]
    for (col in cols)
        # temp_c[, col] <- max(temp_c[, cols]) - temp_c[, col] + min(temp_c[, cols])
        x[i, col] <- max(x[i, cols]) - x[i, col] + min(x[i, cols])
    # coords[to_invert, ] <- temp_c
    na.omit(x)
}

line_coords <- function(df, cols) c(apply(df[, cols], 1, c))

# Function to make viewport able to handle the humap
humap_vp <- function(x_range, y_range, li_margin, long_label, half) {
    # label margin, defined by width of widest label
    la_margin <- grid::stringWidth(long_label)

    # 3-element vector grid units
    widths <- grid::unit.c(la_margin, grid::unit(diff(x_range) + li_margin$main, "null"), la_margin)

    # Defines the layout of the main viewport
    main_layout <- grid::grid.layout(1, 3, widths = widths, heights = diff(y_range),
                                     respect = TRUE)

    # Main map setting appropriate margins
    main <- grid::viewport(width = 0.9, height = 0.9, gp = h_env$gp, layout = main_layout)

    # Map viewport, placed in second column of main viewport
    map <- grid::viewport(xscale = x_range + li_margin$map, yscale = y_range,
                          layout.pos.row = 1, layout.pos.col = 2)

    # Combine the two viewports into a stack
    grid::vpStack(main, map)
}