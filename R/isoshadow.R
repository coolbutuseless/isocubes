

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# rectangular block
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(grid)
  
  x <- 0.5
  y <- 0.1
  size <- 4
  
  N <- 2
  # coords <- expand.grid(x = seq(0, N) + 3, y = seq(0, 2 * N) + 5, z = seq(0, N) - 2)
  coords <- r_coords
  coords$y <- coords$y - min(coords$y) + 0
  coords$z <- coords$z - 0
  
  coords <- rotate_y(coords, -20 * pi/180)
  
  # min z coordinate at ymin
  # TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
  # The zmin offset needs to be calcualted for each y coordinate
  # Use the rotated 'R' object to debug.
  ii <- which(coords$y == min(coords$y))
  zmin <- min(coords$z[ii])
  
  scoords <- coords
  scoords[,c('z', 'y')] <- scoords[,c('y','z')]
  scoords$y <- -1
  scoords$z <- scoords$z + zmin
  
  shad   <- isocubesGrob(scoords, size = size, x = x, y = y, 
                         fill_left = '#00000000', fill_right = '#00000000', col = NA, 
                         fill = '#00000080')
  cubes  <- isocubesGrob(coords, size = size, x = x, y = y, col = -1)
  gnd    <- isolinesGrob(N = 50, size = size * 4, x = x, y = y, col = 'grey80')
  axes   <- isoaxesGrob(size = size, x = x, y = y)
  
  grid.newpage()
  grid.rect(gp = gpar(fill = 'deepskyblue1'))
  grid.draw(gnd)
  grid.draw(shad)
  grid.draw(cubes)  
  grid.draw(axes)
  
  
}
