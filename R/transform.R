
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate around y axis
#' 
#' @param coords data.frame
#' @param theta radians
#' @return rotated dataset
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rotate_y <- function(coords, theta) {
  tmp      <- coords$x * cos(theta) + coords$z * -sin(theta)
  coords$z <- coords$x * sin(theta) + coords$z *  cos(theta)
  coords$x <- tmp
  
  coords
}

