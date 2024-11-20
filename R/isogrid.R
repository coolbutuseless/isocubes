
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











