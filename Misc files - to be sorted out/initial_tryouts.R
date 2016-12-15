for (pack in c("grImport", "dplyr", "colorspace")) library(pack, character.only = T)

# For qualitative palettes:
quali_palette <- function (n, c = 50, l = 70) {
  rainbow_hcl(n, c, l, start = 0, end = 360 * (n - 1)/n)
}

# For quantitative palettes:
quant_palette <- function ()

# Test data
thdat <- read.csv("data/thesis_data.csv", sep = ",", header = TRUE) %>%
  filter(relevant == 1) %>%
  rename(id = X) %>%
  mutate(loc = floor(loc/10))

PostScriptTrace("../figs/bean.ps", "../figs/bean.xml")
bean <- readPicture("../figs/bean.xml")
str(bean)

# Lookup table for assigning proper numbering to the body regions
crosswalk <- c(
squares <- readPicture("data/squares.xml")
names(squares@paths)  <- c("leftUpper", "rightUpper", "leftLower", "rightLower")
grid.picture(squares[c("leftUpper", "rightLower", "rightUpper")])

PostScriptTrace("data/body_male_front.ps", "data/body_male_front.xml")
male_front <- readPicture("data/body_male_front.xml")
picturePaths(male_front, nr = 2, nc = 5)
grid.picture(male_front)


draw_body <- function(data, var, g = "m") { # This one works

  s <- grImport::readPicture("data/body_male_front.xml") # s = surface

  n <- table(data[, var])
  pal <- colorspace::sequential_hcl(max(n)) # pal = palette
  for (i in seq(s@summary@numPaths - 1)) {
    s@paths[[i]]@rgb <- pal[n[i]]
  }
  grid.picture(s)
}

GeomBody <- ggproto("GeomBody", Geom,
  required_aes = "z",
  default_aes = NULL,
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord#$transform(data, panel_scales)
    s <- grImport::readPicture("data/body_male_front.xml") # surface
    n <- table(coords$z)
    pal <- colorspace::sequential_hcl(max(n))

    for (i in seq(s@summary@numPaths - 1)) {
      s@paths[[i]]@rgb <- pal[n[i]]
    }
    grImport::grid.picture(s)
  }
)

geom_body <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBody, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(thdat, aes(z = loc)) + geom_body()

## =================

GeomBodyPoint <- ggproto("GeomBodyPoint", Geom,
  required_aes = "y",
  default_aes = aes(colour = "black", fill = "black", shape = 15, size = 20),
  draw_key = draw_key_point,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    # grid::pointsGrob(
    #   coords$x, coords$y,
    #   pch = coords$shape,
    #   gp = grid::gpar(col = coords$colour)
    # )
    s <- grImport::readPicture("data/body_male_front.xml")
    grImport::grid.picture(s, x = 0.5, y = 0.5, width = 0.8, height = 0.8,
      gp = gpar(col = coords$fill)
    )
    # s <- grImport::readPicture("data/body_male_front.xml") # surface
    #
    # # for (i in seq(s@summary@numPaths - 1)) {
    # #   s@paths[[i]]@rgb <- coords$colour
    # # }
    # grImport::grid.picture(s, x = 0.5, y = 0.5, width = 0.1, height = 0.1)
  }
)


geom_bodypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBodyPoint, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(df, aes(y = y)) + geom_bodypoint()

# ==== THIS WORKS =====
GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(shape = 19, colour = "black", size = 4),
  draw_key = draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    grid::pointsGrob(
      coords$x, coords$y,
      pch = coords$shape,
      gp = gpar(col = coords$colour)
    )
  }
)

geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(x = displ, y = hwy)) + geom_simple_point()

# ==== NOW TRYING WITH THE BODY ====
GeomBody <- ggproto("GeomBody", Geom,
  required_aes = c("x", "fill"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    s <- grImport::readPicture("data/body_male_front.xml") # surface
    for (i in seq(s@summary@numPaths - 1)) {
      s@paths[[i]]@rgb <- if (!is.na(coords[i, "fill"])) coords[i, "fill"] else "#FFFFFF"
    }
    grImport::pictureGrob(s)
  }
)

geom_body <- function(mapping = NULL, data = NULL, stat = "count",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomBody, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(thdat, aes(x = loc, fill = ..count..)) + geom_body() + facet_grid(typetrauma ~ gender, margins = TRUE)



ggbody <- function(data, fill, facets) { # Is to be a nice wrapper for our ggplot call
  # fill = names of variable used to colour the body regions
  # facets = vector of variables used for facetting
  facets <- call(facets)

  data <- data
  #ggplot(thdat, aes(fill = loc)) + geom_body()
}


