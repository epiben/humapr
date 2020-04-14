# Create textGrob for humap annotation
#
# Internal function called within \code{geom_humap}. I cannot think of any
# scenario in which a user would need to call this function.
#
# @param id internal id (e.g., right_abdomen) of label for which to create
#   textGrob
# @param data the original data sent into \code{humap} by \code{ggplot2}
# @param local_coords data fram holding coordinates for all annotations;
#   \code{id} is used to pick appropriate coordinates

make_label <- function (id, data, local_coords) {
    # The data input is the label_data data frame (= original data produced by ggplot)
    # Temporary id, tid
    tid <- if (!h_env$body_halves == "join") id else substring(id, regexpr("_", id) + 1)

    # Base label with absolute and relative frequencies
    if (is.null(h_env$fill)) {
        abs_freq <- data[data$label == tid, "count", drop = TRUE]
        rel_freq <- round(data[data$label == tid, "prop", drop = TRUE] * 100, 0)
    } else {
        abs_freq <- data[data$label == tid, "y", drop = TRUE]
        rel_freq <- round(abs_freq / sum(data$y) * 100, 0)
    }
    label <- sprintf("%s (%s%%)", scales::label_comma()(abs_freq), rel_freq)

    # If desired by user, expand label to include also name of region
    if (h_env$annotate == "all") {
        # If user has combined regions, use appropriate region name
        lab <- if (id %in% names(h_env$combine_key)) {
            h_env$combine_key[names(h_env$combine_key) == id]
        } else {
            id
        }

        # Divide string by underscores
        # FIX: use stringr instead, perhaps also: __ => ", " and _ => " "
        lab <- regmatches(lab, gregexpr("_", lab), invert = TRUE)[[1]]
        if (!h_env$body_halves == "join" & !h_env$map_name %in% c("internal_organs"))
            lab <- lab[-1] # Remove "left"/"right"
        lab <- paste0(lab, collapse = " ")
        label <- paste0(toupper(substr(lab, 1, 1)), substring(lab, 2), ": ", label)
    }

    if (h_env$body_halves == "join") id <- paste0("right_", id)
    coords <- dplyr::filter(local_coords, region == id) %>%
        dplyr::select(y1, x2, label_side)
    x <- grid::unit(coords$x2, "native") +
        grid::unit(1, "strwidth", "  ") * if (coords$label_side == "left") -1 else 1
            # ensure distance to annotation line
    y <- grid::unit(coords$y1, "native") + grid::unit(0.1, "lines")
    just <- if (coords$label_side == "right") "left" else "right"

    grid::textGrob(label, x = x, y = y, default.units = "native", just = just,
                   gp = h_env$gp_text)
}