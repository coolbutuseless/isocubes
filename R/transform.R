
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Align the object with the given coordinates
#'
#' @param coords data.frame with 'x', 'y' and 'z' coordinates
#' @param loc location to align to.  Default: c(0, 0, 0) 
#' @param x,y,z how to align the x coordinates to the given location.
#'         Default: 'mean'.  Valid values 'min', 'mean', 'max', 'identity', 'median'
#' @return data.frame of transformed coordinates
#' @importFrom stats median
#' @examples
#' obj_sphere |>
#'    coord_align(z = 'max', y = 'min') |>
#'    isocubesGrob(size = 3) |>
#'    grid::grid.draw()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coord_align <- function(coords, loc = c(0, 0, 0), x = 'mean', y = 'mean', z = 'mean') {
  
  stopifnot(exprs = {
    x %in% c('min', 'mean', 'max', 'identity', 'none', 'median')
    y %in% c('min', 'mean', 'max', 'identity', 'none', 'median')
    z %in% c('min', 'mean', 'max', 'identity', 'none', 'median')
    length(loc) == 3
    !anyNA(loc)
    is.numeric(loc)
  })
  
  zero <- function(x) { 0 }
  fs <- list(min = min, mean = mean, max = max, median = median,
             identity = zero, none = zero)
  
  fx <- fs[[x]]; coords$x <- coords$x - fx(coords$x)
  fy <- fs[[y]]; coords$y <- coords$y - fy(coords$y)
  fz <- fs[[z]]; coords$z <- coords$z - fz(coords$z)
    
  coords
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate object around a coordinate axis
#' 
#' @inheritParams coord_align
#' @param theta angle in radians. 
#' @param axis axis to rotate around. Default: 'z'. Valid values: 'x', 'y', 'z'
#' @return data.frame of transformed coordinates
#' @examples
#' obj_letter |>
#'    coord_rotate(pi/2, 'y') |>
#'    isocubesGrob() |>
#'    grid::grid.draw()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coord_rotate <- function(coords, theta, axis = 'z') {
  
  stopifnot(exprs = {
    axis %in% c('x', 'y', 'z')
    is.numeric(theta)
  })
  
  if (axis == 'x') {
    tmpy     <- coords$y * cos(-theta) - coords$z * sin(-theta)
    coords$z <- coords$y * sin(-theta) + coords$z * cos(-theta)
    coords$y <- tmpy
  } else if (axis == 'y') {
    tmpx     <- coords$x * cos(-theta) - coords$z * sin(-theta)
    coords$z <- coords$x * sin(-theta) + coords$z * cos(-theta)
    coords$x <- tmpx
  } else if (axis == 'z') {
    tmpx     <- coords$x * cos(-theta) - coords$y * sin(-theta)
    coords$y <- coords$x * sin(-theta) + coords$y * cos(-theta)
    coords$x <- tmpx
  }
  
  coords
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate object 
#' 
#' @inheritParams coord_align
#' @param x,y,z amount to translate along each axis. Default: 0
#' @return data.frame of transformed coordinates
#' @examples
#' obj_sphere |>
#'    coord_translate(x = 20, z = 40) |>
#'    isocubesGrob(size = 2) |>
#'    grid::grid.draw()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coord_translate <- function(coords, x = 0, y = 0, z = 0) {
  
  coords$x <- coords$x + x
  coords$y <- coords$y + y
  coords$z <- coords$z + z
  
  coords
}

