
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Axes for isometric coord system
#' 
#' @param N number of grid units
#' @inheritParams isocubesGrob
#' @return grob
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isoaxesGrob <- function(N = 5,  
                        size          = 5,
                        x             = NULL, 
                        y             = NULL,
                        col           = 'black',
                        default.units = 'npc',
                        default.units.cube = 'mm',
                        xyplane       = 'right',
                        handedness    = 'left',
                        verbosity     = 0, ...) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Origin of grid
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  x <- x %||% grid::unit(0.5, 'npc')
  y <- y %||% grid::unit(0.5, 'npc')
  
  if (!grid::is.unit(x)) {
    x <- grid::unit(x, units = default.units)
  }  
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, units = default.units)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Axis labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lab <- c('x', 'y', 'z')
  coords <- data.frame(
    x = (c(N, 0, 0) + 0.5) * size, 
    y = (c(0, N, 0) + 0.5) * size, 
    z = (c(0, 0, N) + 0.5) * size
  )
  
  ix <- ((coords$x - coords$z) * cos(pi/6))
  iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  ix <- x + unit(ix            , default.units.cube)
  iy <- y + unit(iy - sin(pi/3), default.units.cube)
  
  l1 <- grid::textGrob(lab[1], ix[1], iy[1], gp = gpar(col = 'red'))
  l2 <- grid::textGrob(lab[2], ix[2], iy[2], gp = gpar(col = 'green'))
  l3 <- grid::textGrob(lab[3], ix[3], iy[3], gp = gpar(col = 'blue'))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Lines from origin
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- data.frame(
    x = c(0, N,  0, 0,  0, 0) * size, 
    y = c(0, 0,  0, N,  0, 0) * size, 
    z = c(0, 0,  0, 0,  0, N) * size
  )
  
  ix <- ((coords$x - coords$z) * cos(pi/6))
  iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  ix <- x + unit(ix            , default.units.cube)
  iy <- y + unit(iy - sin(pi/3), default.units.cube)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Full axes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grobTree(
    grid::polylineGrob(ix, iy, id.lengths = c(2, 2, 2), gp = gpar(col = c('red', 'green', 'blue'), lwd = 2)),
    l1, l2, l3,
    grid::pointsGrob(x, y, pch = 19, gp = gpar(col = 'hotpink'))
  )
}


if (FALSE) {
  
  N             = 50
  size          = 5
  x             = NULL 
  y             = NULL
  col           = 'black'
  default.units = 'npc'
  default.units.cube = 'mm'
  verbosity     = 0
  
  
  library(grid)
  coords <- r_coords
  
  x <- 0.4
  y <- 0.1
  size  <- 5
  
  cubes <- isocubesGrob(coords, x = x, y = y, size = size)
  gnd   <- isolinesGrob(x = x, y = y, size = size)
  axes  <- isoaxesGrob(x = x, y = y, size = size)  
  
  grid.newpage()
  grid.rect(gp = gpar(fill = 'lightblue'))
  grid.draw(gnd)
  grid.draw(cubes)
  grid.draw(axes)
}



