make_label <- function (id, data, local_coords, label_pad) {
    # The data input is the label_data data frame (= original data produced by ggplot)
    # Temporary id, tid
    tid <- if (!h_env$half == "mirror") id else substring(id, regexpr("_", id) + 1)

    # Base label with absolute and relative frequencies
    label <- sprintf("%s (%s%%)", # "%s (%s%s)",
                     data[data$label == tid, "count"] / (if (h_env$controls$mid_include) 2 else 1),
                     round(data[data$label == tid, "prop"] * 100, 0)) #, "%")

    # If desired by user, expand label to include also name of region
    if (h_env$annotate == "all") {
        # If user has combined regions, use appropriate region name
        lab <- if (id %in% names(h_env$combine_key)) {
            h_env$combine_key[names(h_env$combine_key) == id]
        } else {
            id
        }
        # Divide string by underscores
        lab <- regmatches(lab, gregexpr("_", lab), invert = TRUE)[[1]]
        if (!h_env$half == "mirror") lab <- lab[-1] # Remove "left"/"right"
        lab <- paste0(lab, collapse = " ") # Join using wide spaces
        # lab <- if (h_env$half == "mirror") {
        #     paste0(regmatches(lab, gregexpr("_", lab),
        #                       invert = TRUE)[[1]], collapse = " ")
        # } else {
        #     paste0(regmatches(lab, gregexpr("_", lab),
        #                       invert = TRUE)[[1]][-1], collapse = " ")
        # }
        # Convert first letter to capital, and combine with frequencies
        label <- paste0(toupper(substr(lab, 1, 1)), substring(lab, 2), ": ", label)
    }

    if (h_env$half == "mirror") id <- paste0("right_", id)
    x_coord <- unit(local_coords[id, "x2"], "native") +
        unit(1, "strwidth", "  ") * if (local_coords[id, "side"] == "left") 1 else -1

    textGrob(label, default.units = "native", just = local_coords[id, "side"], x = x_coord,
             y = unit(local_coords[id, "y1"], "native") + unit(0.1, "lines"))
}