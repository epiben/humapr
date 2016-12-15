for (pack in c("grImport")) library(pack, character.only = T)

# ==== 1st generation: works ====
GeomBody1 <- ggproto("GeomBody1", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    s <- grImport::readPicture("data/body_male_front.xml") # surface
    for (i in seq(s@summary@numPaths - 1)) {
      s@paths[[i]]@rgb <- if (!is.na(coords[i, "fill"])) {
        coords[i, "fill"]
      } else {
        "#FFFFFF"
      }
    }
    grImport::pictureGrob(s)
  }
)

geom_body1 <- function(mapping = NULL, data = NULL, stat = "count",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBody1, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ==== 2nd generation: faster ====
GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    s <- surface_env$surf
    for (i in seq(s@summary@numPaths - 1)) {
      s@paths[[i]]@rgb <- if (!is.na(coords[i, "fill"])) {
        coords[i, "fill"]
      } else {
        "#FFFFFF"
      }
    }
    grImport::pictureGrob(s)
  }
)

geom_body <- function(mapping = NULL, data = NULL, stat = "count",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBody, mapping = mapping,data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# ==== The humap() call ====
# @param data: tidy data frame
# @param
humap <- function(data, region) {
  # data = tidy data frame
  # region = name of columns in data frame that contains counts for each region
  # tab = logical flag indicating whether input data are already tabulated

  surface_env <- new.env(parent = .GlobalEnv)
  surface_env$surf <- grImport::readPicture("data/body_male_front.xml")
    # This needs to take into account further specification from the user

  pars <- as.character(structure(as.list(match.call())[-1], class = "uneval")) # => user may supply parameters w/o quotation marks

  ggplot(data, aes_string(x = pars["region"], fill = "-..count..")) +
    geom_body()
    # + theme(...) at some point
}

g <- function (a, b, c) {
  as.character(structure(as.list(match.call())[-1], class = "uneval"))
}

# ==== Performance tests ====
p1 <- ggplot(thdat, aes(x = loc, fill = -..count..)) +
  geom_body() +
  facet_grid(typetrauma ~ gender, margins = TRUE)
  # This one takes about 17-18 seconds
p2 <- humap(thdat, "loc") +
  facet_grid(typetrauma ~ gender, margins = TRUE)
  # This one takes about 8-9 seconds (so, we cut off about 0.5 seconds per panel)
  # - it's about twice the time of a bar chart with the same information

ggplot(thdat, aes(x = loc)) + geom_bar() + facet_grid(typetrauma ~ gender, margins = TRUE)
