#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create sphere centered at the origin with r = 1
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere()
#' @export
#' @family SDF object creation functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_sphere <- function() {
  function(coords) {
    dist <-  with(coords, x^2 + y^2 + z^2 - 1)
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create torus (donut) at origin
#'
#' @param r1 radius from centre of torus
#' @param r2 radius of the ring.    r2 < r1
#' 
#' @return SDF field function
#' @examples
#' sdf_torus(5, 1)
#' @export
#' @family SDF object creation functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_torus <- function(r1, r2) {
  stopifnot(r2 < r1)
  function(coords) {
    x1   <- with(coords, sqrt( x^2 + z^2 ) - r1)
    d    <- sqrt(x1*x1 + coords$y^2)
    dist <- d - r2
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create semi-infinite volume encompassing all voxels with negative y values
#'
#' This is equivalent to a plane in the XY axis with its normal facing
#' in the positive y direction
#'
#' @return SDF field function
#' @examples
#' sdf_plane()
#' @export
#' @family SDF object creation functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_plane <- function() {
  function(coords) {
    dist <- coords$y # unit plane with n = c(0, 1, 0)
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create 2x2x2 cube centred at origin
#'
#' @return SDF field function
#' @examples
#' sdf_box()
#' @export
#' @family SDF object creation functions
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
    
    dist <- c1 + c2
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create infinitely long cylinder at the origin aligned along z axis. r = 1
#'
#' @return SDF field function
#' @examples
#' sdf_cylinder()
#' @export
#' @family SDF object creation functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_cylinder <- function() {
  function(coords) {
    dist <- sqrt(coords$x^2 + coords$y^2) - 1 # r = 1
    
    dist
  }
}
