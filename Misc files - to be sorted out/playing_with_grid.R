dev.new()
dev.next()
pushViewport(viewport(y = unit(3, "lines"), width = 0.9, height = 0.8,
                      just = "bottom", xscale = c(0, 100)))
grid.rect(gp = gpar(col = "grey"))
grid.xaxis()
pushViewport(viewport(x = unit(60, "native"), y = unit(0.5, "npc"),
                      width = unit(1, "strwidth", "   coordinates for everyone   !"),
                      height = unit(3, "inches")))
grid.rect()
grid.text("coordinates for everyone!")

dev.new()
pushViewport(viewport(layout = grid.layout(4, 5)))
grid.rect(gp = gpar(col = "grey"))
grid.segments(c(1:4/5, rep(0, 3)), c(rep(0, 4), 1:3/4),
              c(1:4/5, rep(1, 3)), c(rep(1, 4), 1:3/4),
              gp = gpar(col = "grey"))
pushViewport(viewport(layout.pos.col = 2:3, layout.pos.row = 3))
grid.rect(gp = gpar(lwd = 3, fill = 0))
popViewport(2)

dev.new()
pushViewport(viewport(gp = gpar(fill = "grey", fontface = "italic")))
grid.rect()
grid.rect(width = 0.8, height = 0.6, gp = gpar(fill = "white"))
grid.text(paste("This text and the inner rectangle",
                "have specific their own gpar setting", sep = "\n"),
          y = 0.75, gp = gpar(fontface = "plain"))
grid.text(paste("This text and the outer rectangle",
                "accept the gpar settings of the viewport", sep = "\n"),
          y = 0.25)
popViewport()

dev.new()
x <- y <- 1:10
pushViewport(plotViewport(c(5.1, 4.1, 4.1, 2.1)))
pushViewport(dataViewport(x, y))
grid.rect()
grid.xaxis()
grid.yaxis()
grid.points(x, y)
grid.text("1:10", x = unit(-3, "lines"), rot = 90)
popViewport(2)

dev.new()
barData <- matrix(sample(1:4, 16, replace = TRUE), ncol = 4)
boxColours <- 1:4
bp <- function(barData) {
  nbars <- dim(barData)[2]
  nmeasures <- dim(barData)[1]
  barTotals <- rbind(rep(0, nbars), apply(barData, 2, cumsum))
  barYscale <- c(0, max(barTotals) * 1.05)
  pushViewport(plotViewport(c(5, 4, 4, 1),
                            yscale = barYscale,
                            layout = grid.layout(1, nbars)))
  grid.rect()
  grid.yaxis()
  for (i in seq(nbars)) {
    pushViewport(viewport(layout.pos.col = i, yscale = barYscale))
    grid.rect(x = rep(0.5, nmeasures),
              y = unit(barTotals[1:nmeasures, i], "native"),
              height = unit(diff(barTotals[, i]), "native"),
              width = 0.8, just = "bottom", gp = gpar(fill = boxColours))
    popViewport()
  }
}

legLabels <- c("Group A", "Group B", "Group C", "Something longer")
boxSize <- unit(0.5, "inches")

leg <- function(legLabels) {
  nlabels <- length(legLabels)
  pushViewport(viewport(layout = grid.layout(nlabels, 1)))
  for (i in seq(nlabels)) {
    pushViewport(viewport(layout.pos.row = i))
    grid.rect(width = boxSize, height = boxSize, just = "bottom",
              gp = gpar(fill = boxColours[i]))
    grid.text(legLabels[i], y = unit(0.5, "npc") - unit(1, "lines"))
    popViewport()
  }
  popViewport()
}

dev.new()
grid.rect(gp = gpar(lty = "dashed"))
legend.width <- max(unit(rep(1, length(legLabels)),
                         "strwidth", as.list(legLabels)) + unit(2, "lines"),
                    unit(0.5, "inches") + unit(2, "lines"))
pushViewport(viewport(layout = grid.layout(1, 2, widths = unit.c(unit(1, "null"), legend.width))))
pushViewport(viewport(layout.pos.col = 1))
bp(barData)
popViewport(2)
pushViewport(viewport(layout.pos.col = 2))
pushViewport(plotViewport(c(5, 0, 4, 0)))
leg(legLabels)

