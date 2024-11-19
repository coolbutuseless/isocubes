
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Isometric grid of points
#' 
#' @param pch plotting character. default '.'
#' @inheritParams isolinesGrob
#' @return isometric point grid
#' @examples
#' grid <- isopointsGrob()
#' grid::grid.draw(grid)
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isopointsGrob <- function(N             = 50,
                          size          = 5,
                          x             = NULL, 
                          y             = NULL,
                          col           = 'black',
                          pch           = '.',
                          default.units = 'npc',
                          default.units.cube = 'mm',
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
  # Points
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- expand.grid(x = seq(-N, N) * size, z = seq(-N, N) * size, y = 0)
  ix <- ((coords$x - coords$z) * cos(pi/6))
  iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  ix <- x + unit(ix, default.units.cube)
  iy <- y + unit(iy - sin(pi/3), default.units.cube)
  
  grid::pointsGrob(ix, iy, pch =pch, gp = gpar(col = col, ...))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Isometrix grid of lines
#' @param N extents
#' @inheritParams isocubesGrob
#' @return isometric line grid
#' @examples
#' grid <- isolinesGrob()
#' grid::grid.draw(grid)
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isolinesGrob <- function(N             = 50,
                         size          = 5,
                         x             = NULL, 
                         y             = NULL,
                         col           = 'black',
                         default.units = 'npc',
                         default.units.cube = 'mm',
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
  # First lines
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- data.frame(
    x = rep(seq(-N, N), each = 2) * size, 
    y = 0, 
    z = c(-N, N) * size
  )
  
  lx1 <- ((coords$x - coords$z) * cos(pi/6))
  ly1 <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  lx1 <- x + unit(lx1, 'mm')
  ly1 <- y + unit(ly1 - sin(pi/3), 'mm')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Orthogonal lines
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords[, c('x', 'z')] <- coords[, c('z', 'x')]
  
  lx2 <- ((coords$x - coords$z) * cos(pi/6))
  ly2 <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  lx2 <- x + unit(lx2, 'mm')
  ly2 <- y + unit(ly2 - sin(pi/3), 'mm')
  
  
  grid::grobTree(
    grid::polylineGrob(lx1, ly1, gp = gpar(col = col, ...), id.lengths = rep.int(2, length(lx1)/2)),
    grid::polylineGrob(lx2, ly2, gp = gpar(col = col, ...), id.lengths = rep.int(2, length(lx2)/2))
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Letter R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(grid)
  
  N             = 50
  size          = 5
  x             = NULL 
  y             = NULL
  col           = 'black'
  default.units = 'npc'
  default.units.cube = 'mm'
  verbosity     = 0
  
  scoords <- r_coords
  scoords$z <- -1
  shad   <- isocubesGrob(scoords, size = 5, y = 0.1, xyplane = 'flat', handedness = 'right', 
                         fill_left = '#00000000', fill_right = '#00000000', col = NA, 
                         fill = '#00000080')
  cubes  <- isocubesGrob(r_coords, size = 5, y = 0.1)
  gnd    <- isolinesGrob(N = 50, size = 5, y = 0.1, col = 'grey30')
  # pnt    <- isopointsGrob(N = 50, size = 5, y = 0)

  grid.newpage()
  grid.rect(gp = gpar(fill = 'deepskyblue1'))
  grid.draw(gnd)
  grid.draw(shad)
  grid.draw(cubes)  
    
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Organic
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(grid)
  
  x <- 0.5
  y <- 0.5
  size <- 1

  coords  <- organic_coords  
  coords$x <- coords$x - min(coords$x)
  # coords$y <- coords$y - min(coords$y)
  # coords$z <- coords$z - min(coords$z)
  scoords <- coords
  scoords$z <- -1
  shad   <- isocubesGrob(scoords, size = size, x = x, y = y, xyplane = 'flat', handedness = 'right', 
                         fill_left = '#00000000', fill_right = '#00000000', col = NA, 
                         fill = '#00000080')
  cubes  <- isocubesGrob(coords, size = size, x = x, y = y, col = -1)
  gnd    <- isolinesGrob(N = 50, size = size * 4, x = x, y = y, col = 'grey80')

  grid.newpage()
  grid.rect(gp = gpar(fill = 'deepskyblue1'))
  grid.draw(gnd)
  grid.draw(shad)
  grid.draw(cubes)  
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rectangular block
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(grid)
  
  x <- 0.5
  y <- 0.5
  size <- 5
  
  N <- 2
  coords <- expand.grid(x = seq(0, N) + 1, y = seq(0, 2 * N) + 2, z = seq(0, N))
  
  scoords <- coords
  zmin <- min(scoords$z)
  
  scoords[,c('z', 'y')] <- scoords[,c('y','z')]
  scoords$y <- -1
  
  scoords$z <- scoords$z + zmin
  
  shad   <- isocubesGrob(scoords, size = size, x = x, y = y, 
                         fill_left = '#00000000', fill_right = '#00000000', col = NA, 
                         fill = '#00000080')
  cubes  <- isocubesGrob(coords, size = size, x = x, y = y, col = 'black')
  gnd    <- isolinesGrob(N = 50, size = size * 1, x = x, y = y, col = 'grey80')
  
  grid.newpage()
  grid.rect(gp = gpar(fill = 'deepskyblue1'))
  grid.draw(gnd)
  grid.draw(shad)
  grid.draw(cubes)  
  grid.points(x, y, default.units = 'npc', pch = 19, gp = gpar(col = 'red'))
  
}








