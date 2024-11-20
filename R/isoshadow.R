
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
  size <- 2
  
  N <- 2
  coords <- expand.grid(x = seq(0, N) + 1, y = seq(0, 2 * N) + 0, z = seq(0, N) + 1)
  coords <- sphere_coords
  coords <- coords[coords$y >= 0, ]
  
  scoords <- coords
  zmin <- min(scoords$z)
  
  scoords[,c('z', 'y')] <- scoords[,c('y','z')]
  scoords$y <- -1
  
  scoords$z <- scoords$z + zmin + 1L
  
  shad   <- isocubesGrob(scoords, size = size, x = x, y = y, 
                         fill_left = '#00000000', fill_right = '#00000000', col = NA, 
                         fill = '#00000080')
  cubes  <- isocubesGrob(coords, size = size, x = x, y = y, col = 'black')
  gnd    <- isolinesGrob(N = 50, size = size * 1, x = x, y = y, col = 'grey80')
  axes   <- isoaxesGrob(size = size, x = x, y = y)
  
  grid.newpage()
  grid.rect(gp = gpar(fill = 'deepskyblue1'))
  grid.draw(gnd)
  grid.draw(shad)
  grid.draw(cubes)  
  grid.draw(axes)
}
