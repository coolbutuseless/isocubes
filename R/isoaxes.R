
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
  # Orientation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ord <- c(1, 2, 3)
  sgn <- c(1, 1, 1)
  if (xyplane == 'right' && handedness == "left") {
    # do nothing
  } else if (xyplane == 'right' && handedness == "right") {
    sgn[3] <- -1
  } else if (xyplane == 'left' && handedness == "left") {
    # newz = -x,   newx = z
    # Swap x,z.  Negate z
    ord <- c(3, 2, 1)
    sgn[3] <- -1
  } else if (xyplane == 'left' && handedness == "right") {
    # newz = -x,   newx = -z
    # Swap x,z.  Negate x. negate z
    ord <- c(3, 2, 1)
    sgn[1] <- -1
    sgn[3] <- -1
  } else if (xyplane %in% c('top', 'flat') && handedness == 'right') {
    # Swap yz
    ord <- c(1, 3, 2)
  } else if (xyplane %in% c('top', 'flat') && handedness == 'left') {
    # Swap yz. Negate z
    ord <- c(1, 3, 2)
    sgn[3] <- -1
  } else {
    stop("Not a supported coordinate system: xyplane: ", xyplane, "  hand: ", handedness)
  }
  sgn <- sgn[ord]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Axis labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lab <- c('x', 'y', 'z')[ord]
  col <- c('red', 'seagreen3', 'blue')[ord]
  
  coords <- data.frame(
    x = (c(N, 0, 0) + 0.5) * size * sgn[1], 
    y = (c(0, N, 0) + 0.5) * size * sgn[2], 
    z = (c(0, 0, N) + 0.5) * size * sgn[3]
  )
  
  ix <- ((coords$x - coords$z) * cos(pi/6))
  iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  ix <- x + unit(ix            , default.units.cube)
  iy <- y + unit(iy - sin(pi/3), default.units.cube)
  
  l1 <- grid::textGrob(lab[1], ix[1], iy[1], gp = gpar(col = col[1]))
  l2 <- grid::textGrob(lab[2], ix[2], iy[2], gp = gpar(col = col[2]))
  l3 <- grid::textGrob(lab[3], ix[3], iy[3], gp = gpar(col = col[3]))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Lines from origin
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- data.frame(
    x = c(0, N,  0, 0,  0, 0) * size * sgn[1], 
    y = c(0, 0,  0, N,  0, 0) * size * sgn[2], 
    z = c(0, 0,  0, 0,  0, N) * size * sgn[3]
  )
  
  ix <- ((coords$x - coords$z) * cos(pi/6))
  iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  ix <- x + unit(ix            , default.units.cube)
  iy <- y + unit(iy - sin(pi/3), default.units.cube)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Full axes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grobTree(
    grid::polylineGrob(ix, iy, id.lengths = c(2, 2, 2), gp = gpar(col = col, lwd = 4)),
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
  y <- 0.3
  size  <- 5
  
  xyplane <- 'right'
  hand    <- 'right'
  cubes <- isocubesGrob(coords, x = x, y = y, size = size, xyplane = xyplane, handedness = hand)
  gnd   <- isolinesGrob(        x = x, y = y, size = size, xyplane = xyplane, handedness = hand, col = 'grey60')
  axes  <- isoaxesGrob(         x = x, y = y, size = size, xyplane = xyplane, handedness = hand)  
  
  grid.newpage()
  grid.rect(gp = gpar(fill = 'lightblue'))
  grid.draw(gnd)
  grid.draw(cubes)
  grid.draw(axes)
  
}



