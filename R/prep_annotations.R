prep_annotations <- function(mapped_loc, map_name = NULL, symmetric_map = TRUE) {
    if (!h_env$annotate %in% c("all", "freq")) return() # do nothing

    combine <- h_env$combine
    proj <- h_env$proj
    body_halves <- h_env$body_halves

    # Applying defaults
    h_env$controls$label_pad <- diff(sp::bbox(h_env$map)["y", ]) *
        (h_env$controls$label_pad %||% 3.5) / 100

    # Importing file with predefined (x0, y0)'s or just compute them
    # if (type == "simple" && proj == "neutral") {
        # This one should check that the computed (x0, y0) is actually inside
        # the relevant region, and find a new one if it isn't
        coords <- h_env$mapdf %>%
            dplyr::mutate(plot_mid = mean(range(long)), # is useful to mirror and inverse coordinates later
                          label_side = ifelse(long > plot_mid, "right", "left")) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(x0 = mean(range(long)),
                             y0 = mean(range(lat)),
                             plot_mid = plot_mid[1],
                             label_side = label_side[1]) %>%
            dplyr::group_by(label_side) %>%
            dplyr::mutate(side_mid = mean(range(x0))) %>%
            dplyr::rename(region = id) %>%
            as.data.frame()
        # rownames(h_env$anno_coords) <- h_env$anno_coords$region
        rownames(coords) <- h_env$coords$region
    # } else {
        # h_env$anno_coords <- read.csv2(sprintf("data/coords_%s_%s_%s_%s.csv", type,
        #                                        map, gender, proj), row.names = 1)
        # h_env$anno_coords <- h_env$anno_coords[h_env$regions, ]
        # If we want to use pre-defined (x0, y0)'s, we should keep them in R/sysdata.rda
    # }

    # In the end, which regions are actually mapped?
    mapped_regions <- if (body_halves == "join") mapped_loc else rm_lr(mapped_loc)
    mapped_regions <- as.vector(na.exclude(unique(mapped_regions)))
    if (!is.null(combine)) {
        # Consider checking that combine input has appropriate form (particularly,
        # the names of the combinations)
        for (combine_name in names(combine))
            mapped_regions[mapped_regions == combine_name] <- combine[[combine_name]][[1]]
        coords <- dplyr::filter(coords, region %in% lr_conc(mapped_regions))
    }

    # Computing vertical positions of labels
    if (body_halves == "join") {
        coords <- dplyr::filter(coords,
                                region %in% paste0("right_", mapped_regions))
        if (symmetric_map)
            coords <- dplyr::arrange(coords, desc(y0)) %>%
                dplyr::mutate(label_side = ifelse(seq(region) %% 2 == 1, "left", "right"))
    } else {
        # coords <- rbind(coords[paste0("left_", mapped_regions), ],
        #                 coords[paste0("right_", mapped_regions), ]) %>%
        coords <- dplyr::filter(coords, region %in% lr_conc(mapped_regions)) %>%
            inverse_coords("x0")
            # Inversing to use same distribution algorithm for both sides
    }

    coords <- rbind(def_dist(coords[coords$label_side == "left", ]),
                    def_dist(coords[coords$label_side == "right", ]))

    # Fix and tweak coordinates to be appropriate for the chosen map
    if (body_halves == "separate")
        # Inverse back, to get correct location of points
        coords <- inverse_coords(coords, c("x0", "x1"))
    if (body_halves == "join" & symmetric_map)
        coords <- dplyr::mutate(coords,
            x0 = mirror_coord(x0, plot_mid, label_side, "right"), # mirror around mid of plot
            x1 = mirror_coord(x1, plot_mid, label_side, "right")) # idem
    if (body_halves == "join" & !symmetric_map)
        coords <- dplyr::mutate(coords,
            x0 = mirror_coord(x0, side_mid, label_side, "right"), # mirror around mid of sub-plot
            x1 = mirror_coord(x1, side_mid, label_side, "right")) # idem

    h_env$anno_coords <- coords
}