


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://www.ronja-tutorials.com/post/035-2d-sdf-combination/
# ToDo:
#  - rounded union
#  - rounded intersection
#  - rounded subtract
#  - chamfer union, intersection, subtract
#  - 
#
# ToDo make this generic so that it can take N different objects
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



# Infinite + Finite repetition
# deformations and distortions
#  - displacement, twist, bend, 


if (FALSE) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise Rendering volume
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  N  <- 30
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sphere <- sdf_sphere() %>% 
    sdf_scale(20) 
  
  box <- sdf_box() %>%
    sdf_scale(16) 
  
  cyl <- sdf_cyl() %>%
    sdf_scale(8)
  
  
  world <- sdf_subtract(
    sdf_intersect(box, sphere),
    sdf_union(
      cyl,
      sdf_rotatey(cyl, pi/2),
      sdf_rotatex(cyl, pi/2)
    )
  )
  inside <- world(coords) <= 0
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep only cubes which are interior to an SDF object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- coords[inside,]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cubes  <- isocubesGrob(coords, max_y = 60, xo = 0.5, yo = 0.5)
  grid.newpage()
  grid.draw(cubes)
  
  


}














