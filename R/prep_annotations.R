prep_annotations <- function(mapped_loc, map_name = NULL) {
    if (!h_env$annotate %in% c("all", "freq")) return() # do nothing

    combine <- h_env$combine
    proj <- h_env$proj
    body_halves <- h_env$body_halves

    # Applying defaults
    h_env$controls$label_pad <- diff(sp::bbox(h_env$map)["y", ]) *
        (h_env$controls$label_pad %||% 3.5) / 100
    h_env$controls$vert_adj <- h_env$controls$vert_adj %||% "smart"

    # Importing file with predefined (x0, y0)'s or just compute them
    # if (type == "simple" && proj == "neutral") {
        # This one should check that the computed (x0, y0) is actually inside
        # the relevant region, and find a new one if it isn't
        h_env$anno_coords <- h_env$mapdf %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(x0 = mean(range(long)), y0 = mean(range(lat))) %>%
            dplyr::rename(region = id) %>%
            as.data.frame()
        rownames(h_env$anno_coords) <- h_env$anno_coords$region
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
        for (combine_name in names(combine)) {
            mapped_regions[mapped_regions == combine_name] <- combine[[combine_name]][[1]]
        }
        h_env$anno_coords <- h_env$anno_coords[lr_conc(mapped_regions), ]
    }

    # Computing vertical positions of labels
    if (h_env$controls$vert_adj == "smart") {
        coords <- switch(body_halves,
                    left = h_env$anno_coords[paste0("left_", mapped_regions), ],
                    right = , # Takes the next value
                    join = h_env$anno_coords[paste0("right_", mapped_regions), ],
                    rbind(h_env$anno_coords[paste0("left_", mapped_regions), ],
                       h_env$anno_coords[paste0("right_", mapped_regions), ])) %>%
            inverse_coords("x0", "left_")
                # Inversing to use same distribution algorithm for both sides

        coords <- if (body_halves %in% c("left", "right", "join")) {
            def_dist(coords)
        } else {
            rbind(def_dist(coords[grepl("left_", row.names(coords)), ]),
                  def_dist(coords[grepl("right_", row.names(coords)), ]))
        }

        if (body_halves %in% c("separate", "left"))
            coords <- inverse_coords(coords, c("x0", "x1"), "left_")
        h_env$anno_coords <- coords
    } else {
        lr <- if (h_env$body_halves == "left") "left_" else "right_"
        y0 <- setNames(h_env$anno_coords[paste0(lr, mapped_regions), "y0"],
                       row.names(h_env$anno_coords[paste0(lr, mapped_regions), ]))
        y1 <- distribute_coords(y0, h_env$controls$label_pad)
        h_env$anno_coords$y1 <- 0
        if (body_halves == "separate") {
            h_env$anno_coords[paste0("right_", mapped_regions), "y1"] <-
                h_env$anno_coords[paste0("left_", mapped_regions), "y1"] <-
                y1[paste0("right_", mapped_regions)]
        } else {
            h_env$anno_coords[paste0(lr, mapped_regions), "y1"] <-
                y1[paste0(lr, mapped_regions)]
        }
        h_env$anno_coords$offset <- ifelse(h_env$anno_coords$y1 > 0,
                                           h_env$anno_coords$y1 - h_env$anno_coords$y0,
                                           NA)
    }
}