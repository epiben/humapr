`%||%` <- function(a, b) if(!is.null(a)) a else b

line_coords <- function(df, cols) c(apply(df[, cols], 1, c))

rm_lr <- function(x) substring(x, regexpr("_", x) + 1)

prompt_inv <- function(arg, val){ # Prompt on invalid argument
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

lr_conc <- function(x) c(paste0("left_", x), paste0("right_", x))

line_coords <- function(df, cols) c(apply(df[, cols], 1, c))


