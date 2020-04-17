.onLoad <- function(libname = find.package("humapr"), pkgname = "humapr") {
    if (getRversion() >= "2.15.1") {
        utils::globalVariables(c(
            # humapr-specific variables
             "x0", "x1", "x2", "y0", "y1", "region", "long", "lat", "id", "plot_mid", "side_mid", "label_side",
             # We use magrittr's pipe
             ".",
             # We use := from rlang
             ":=",
             # We use desc from dplyr
             "desc"
        )) # see: https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
    }
}