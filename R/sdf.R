


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://www.ronja-tutorials.com/post/035-2d-sdf-combination/
# ToDo:
#  - chamfer union, intersection, subtract
#  - 
# - elongation
# - primitives: quad, tri, octahedron, pyramid, ellipsoid,
#   rounded cylinder, etc
#
# deformations and distortions
#  - displacement, twist, bend, 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render a set of objects within a voxel volume
#' 
#' @param scene sdf object
#' @param N Defines the extents of the x,y,z voxel volume 
#'          i.e. from -N:N along each axis
#'        
#' @return Return a data.frame of all the integer x,y,z coordinates which 
#'               are occupied voxels
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_render <- function(scene, N) {
  
  # create the voxel grid
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
  
  # Evaluate the distance fields within this grid
  dist  <- scene(coords)
  
  # Find where the voxels are "inside" an object i.e. dist < 0
  inside <- dist <= 0
  
  # Subset the coordinates and colours which are inside
  coords[inside,]
}





if (FALSE) {
  library(grid)
  library(dplyr)
  
  
  scene <- sdf_torus(3, 1) %>% sdf_scale(5)
  coords <- sdf_render(scene, 30)
  
  cubes  <- isocubesGrob(coords, ysize = 1/50, xo = 0.5, yo = 0.5, fill = 'lightblue')
  grid.newpage(); grid.draw(cubes)
  
}








if (FALSE) {
  library(grid)
  library(dplyr)
  
  scene <- sdf_subtract(
    sdf_box() %>% sdf_scale(8),
    sdf_plane() %>%
      sdf_translate(y = -4) %>%
      sdf_rotatex(3 * pi/4)
  )
  
  coords <- sdf_render(scene, 30)
  
  cubes  <- isocubesGrob(coords, ysize = 1/50, xo = 0.5, yo = 0.5, fill = 'brown')
  grid.newpage(); grid.draw(cubes)
  
}





if (FALSE) {
  
  library(grid)
  
  
  sphere <- sdf_sphere() %>%
    sdf_scale(40)
  
  box <- sdf_box() %>%
    sdf_scale(32)
  
  cyl <- sdf_cylinder() %>%
    sdf_scale(16)
  
  
  scene <- sdf_subtract_smooth(
    sdf_intersect(box, sphere),
    sdf_union(
      cyl,
      sdf_rotatey(cyl, pi/2),
      sdf_rotatex(cyl, pi/2)
    )
  )
  
  # scene <- sdf_union(box, sphere)
  # scene <- sdf_union(
  #   cyl,
  #   sdf_rotatey(cyl, pi/2),
  #   sdf_rotatex(cyl, pi/2)
  # )
  
  coords <- sdf_render(scene, 50)
  cubes  <- isocubesGrob(coords, ysize = 1/100, xo = 0.5, yo = 0.5)
  grid.newpage(); grid.draw(cubes)
  
  
  # inside <- world(coords) <= 0
  # cubes  <- isocubesGrob(coords[inside,], ysize = 1/110, xo = 0.5, yo = 0.5)
  # grid.newpage()
  # grid.draw(cubes)
  
  
  x11(type = 'dbcairo', width = 10, height = 10)
  dev.control('inhibit')
  
  thetas <- seq(0, 2*pi, length.out = 180)
  system.time({
    for (i in seq_along(thetas)) {
      theta <- thetas[i]
      cat(".")
      rot_scene <- scene %>% 
        sdf_rotatey(theta) %>%
        sdf_rotatex(theta + pi/2) %>%
        sdf_rotatez(theta + pi/3)
      
      world <- sdf_render(rot_scene, 50)
      cubes  <- isocubesGrob(world$coords, ysize = 1/110, xo = 0.5, yo = 0.5)
      
      # dev.hold()
      # grid.rect(gp = gpar(fill = 'white'))
      # grid.draw(cubes)
      # dev.flush()
      
      png_filename <- sprintf("working/anim/%03i.png", i)
      png(png_filename, width = 800, height = 800)
      grid.draw(cubes)
      dev.off()
    }
  })
  
  # 324, 53, 375
  
  # ffmpeg -y -framerate 20 -pattern_type glob -i 'anim/*.png' -c:v libx264 -pix_fmt yuv420p -s 800x800 'anim.mp4'
  
}














