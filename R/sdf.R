


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
#' Create a unit shape at the origin
#' 
#' Many SDF functions from \url{https://iquilezles.org/articles/distfunctions/}
#'
#' \describe{
#'  \item{cylinder}{infinitely long unit cylinder at the origin aligned along z axis}
#' }
#'
#' @param r1 radius from centre of torus
#' @param r2 radius of the ring.    r2 < r1
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_torus <- function(r1, r2) {
  function(coords) {
    x1 <- with(coords, sqrt( x^2 + z^2 ) - r1)
    d  <- sqrt(x1*x1 + coords$y^2)
    
    d - r2
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_sphere <- function() {
  function(coords) {
    with(coords, x^2 + y^2 + z^2 - 1)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_plane <- function() {
  function(coords) {
    coords$y # unit plane with n = c(0, 1, 0)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_box <- function() {
  function(coords) {
    # vec3 q = abs(p) - b;
    qx <- abs(coords$x) - 1L
    qy <- abs(coords$y) - 1L
    qz <- abs(coords$z) - 1L
    
    # return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
    c1 <- sqrt(
      pmax.int(qx, 0)^2 +
        pmax.int(qy, 0)^2 +
        pmax.int(qz, 0)^2
    )
    
    c2 <- pmin.int(pmax.int(qx, qy, qz), 0)
    
    c1 + c2
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an infinitely long unit cylinder at the origin aligned along z axis
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_cyl <- function() {
  function(coords) {
    sqrt(coords$x^2 + coords$y^2) - 1 # r = 1
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate an object 
#' 
#' @param f field function
#' @param x,y,z traslation amount. defaults to 0
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_translate <- function(f, x=0, y=0, z=0) {
  function(coords) {
    coords$x <- coords$x - x
    coords$y <- coords$y - y
    coords$z <- coords$z - z
    f(coords)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate an object 
#' 
#' @param f field function
#' @param xscale,yscale,zscale Scale amount. xscale defaults to 1. yscale
#'        and zscale default to the value of xscale
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_scale <- function(f, xscale = 1, yscale = xscale, zscale = xscale) {
  function(coords) {
    coords$x <- coords$x / xscale
    coords$y <- coords$y / yscale
    coords$z <- coords$z / zscale
    f(coords)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Finite/Infinite repetition 
#' 
#' @param f field function
#' @param x,y,z repetition distance along each axis
#' @param lx,ly,lz repeitition times along each axis
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_repeat_infinite <- function(f, x, y = x, z = x) {
  function(coords) {
    coords$x <- round( (coords$x + 0.5 * x) %% x - 0.5 * x )
    coords$y <- round( (coords$y + 0.5 * y) %% y - 0.5 * y )
    coords$z <- round( (coords$z + 0.5 * z) %% z - 0.5 * z )
    f(coords)
  }
}


clamp <- function(x, lo, hi) {
  ifelse(x < lo, 
         lo, 
         ifelse(x > hi, 
                hi,
                x))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_repeat_infinite
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_repeat_finite <- function(f, x, lx, y = x, ly = lx, z = x, lz = lx) {
  function(coords) {
    coords$x <- round( coords$x - x * clamp(round(coords$x/x), -lx, lx) )
    coords$y <- round( coords$y - y * clamp(round(coords$y/y), -ly, ly) )
    coords$z <- round( coords$z - z * clamp(round(coords$z/z), -lz, lz) )
    f(coords)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate a signed distance field around the z axis
#' 
#' @param f field function
#' @param theta angle in radians
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_rotatez <- function(f, theta) {
  function(coords) {
    tmpx     <- coords$x * cos(-theta) - coords$y * sin(-theta)
    coords$y <- coords$x * sin(-theta) + coords$y * cos(-theta)
    coords$x <- tmpx
    f(coords)
  }
}


#' @rdname sdf_rotatez
#' @export
sdf_rotatey <- function(f, theta) {
  function(coords) {
    tmpx     <- coords$x * cos(-theta) - coords$z * sin(-theta)
    coords$z <- coords$x * sin(-theta) + coords$z * cos(-theta)
    coords$x <- tmpx
    f(coords)
  }
}


#' @rdname sdf_rotatez
#' @export
sdf_rotatex <- function(f, theta) {
  function(coords) {
    tmpy     <- coords$y * cos(-theta) - coords$z * sin(-theta)
    coords$z <- coords$y * sin(-theta) + coords$z * cos(-theta)
    coords$y <- tmpy
    f(coords)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a "thin" version of a given object.
#' 
#' @param f field function
#' @param thickness thickness of shell
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_onion <- function(f, thickness) {
  function(coords) {
    abs(f(coords)) - thickness
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rounded version of a given object.
#' 
#' @param f field function
#' @param r radius of rounding
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_round <- function(f, r) {
  force(r) 
  function(coords) {
    f(coords) - r
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @param ... field functions
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_union <- function(...) {
  function(coords) {
    fs  <- list(...)
    res <- lapply(fs, function(f) f(coords))
    do.call(pmin.int, res)
    # pmin.int(
    #   f1(coords),
    #   f2(coords)
    # )
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Smoothed union of two objects
#' 
#' @param f1,f2 field functions
#' @param k smoothing parameter
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_union_smooth <- function(f1, f2, k) {
  force(k)
  function(coords) {
    d1 <- f1(coords)
    d2 <- f2(coords)
    h  <- clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 )
    (1 - h) * d2 + h * d1 - k * h * (1 - h)
    # pmin.int(
    #   f1(coords),
    #   f2(coords)
    # )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @param f1,f2 field functions
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_intersect <- function(f1, f2) {
  function(coords) {
    pmax.int(
      f1(coords),
      f2(coords)
    )
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @param f1,f2 field functions
#' @param k smoothing parameter
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_intersect_smooth <- function(f1, f2, k) {
  force(k)
  function(coords) {
    d1 <- f1(coords)
    d2 <- f2(coords)
    h  <- clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 )
    (1 - h) * d2 + h * d1 + k * h * (1 - h)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subtract on object from another
#' 
#' @param f1,f2 field functions
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_subtract <- function(f1, f2) {
  function(coords) {
    pmax.int(
      f1(coords),
      -f2(coords)
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subtract on object from another
#' 
#' @param f1,f2 field functions
#' @param k smoothing parameter
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_subtract_smooth <- function(f1, f2, k) {
  force(k)
  function(coords) {
    d1 <- f1(coords)
    d2 <- f2(coords)
    h  <- clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 )
     (1 - h) * d1 -  h * d2 + k * h * (1 - h)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Linearly interpolate between two objects
#'
#' @param f1,f2 field functions
#' @param amount proportion of final field which comes from second field. 
#'        \code{amount} should be a numeric value in the range [0,1]
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_interpolate <- function(f1, f2, amount) {
  function(coords) {
    (1 - amount) * f1(coords) +
      amount  * f2(coords)
  }
}





if (FALSE) {
  library(grid)
  library(dplyr)
  
  N  <- 30
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))

  
  # world <- sdf_intersect(
  #   sdf_box() %>% sdf_scale(8) %>% sdf_onion(1),
  #   sdf_box() %>% sdf_scale(12) %>% sdf_translate(x = -10)
  # )
  world <- sdf_subtract(
    sdf_box() %>% sdf_scale(8),
    sdf_plane() %>% 
      sdf_translate(y = -4) %>%
      sdf_rotatex(3 * pi/4)
  )
  inside <- world(coords) <= 0
  
  
  coords <- coords[inside,]
  
  cubes  <- isocubesGrob(coords, max_y = 115, xo = 0.5, yo = 0.5)
  grid.newpage()
  grid.draw(cubes)

}


if (FALSE) {
  
  library(grid)
  
  
  sphere <- sdf_sphere() %>%
    sdf_scale(40)

  box <- sdf_box() %>%
    sdf_scale(32)

  cyl <- sdf_cyl() %>%
    sdf_scale(16)


  world <- sdf_subtract(
    sdf_intersect(box, sphere),
    sdf_union(
      cyl,
      sdf_rotatey(cyl, pi/2),
      sdf_rotatex(cyl, pi/2)
    )
  )
  
  
  N  <- 50
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
  
  inside <- world(coords) <= 0
  cubes  <- isocubesGrob(coords[inside,], max_y = 110, xo = 0.5, yo = 0.5)
  grid.newpage()
  grid.draw(cubes)
  
  
  x11(type = 'dbcairo', width = 10, height = 10)
  dev.control('inhibit')
  
  thetas <- seq(0, 2*pi, length.out = 180)
  for (i in seq_along(thetas)) {
    theta <- thetas[i]
    cat(".")
    final <- world %>% 
      sdf_rotatey(theta) %>%
      sdf_rotatex(theta + pi/2) %>%
      sdf_rotatez(theta + pi/3)
    inside <- final(coords) <= 0
    cubes  <- isocubesGrob(coords[inside,], max_y = 110, xo = 0.5, yo = 0.5)
    
    # dev.hold()
    # grid.rect(gp = gpar(fill = 'white'))
    # grid.draw(cubes)
    # dev.flush()
    
    png_filename <- sprintf("working/anim/%03i.png", i)
    png(png_filename, width = 800, height = 800)
    grid.draw(cubes)
    dev.off()
    
  }
  
  # ffmpeg -y -framerate 20 -pattern_type glob -i 'anim/*.png' -c:v libx264 -pix_fmt yuv420p -s 800x800 'anim.mp4'
  
}














