

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate voxel coordinates defined by an implicit function
#' 
#' @param f function of the form \code{f(x, y, z)} wrhite returns a numeric value
#' @param lower,upper The range of values which will be part of the object.
#'        default [-Inf, 0]
#' @param nx,ny,nz the dimensions of the volume within which the function 
#'        will be evaluated
#' @param scale extra scaling factor applied to coordinates before calling 
#'        function
#' @return data.frame of coordinates
#' @examples
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Create a sphere of radius 10
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' coords <- gen_isosurface(
#'   f = function(x, y, z) {x^2 + y^2 + z^2},
#'   upper = 10^2
#' ) 
#' 
#' coords |>
#'   isocubesGrob() |> 
#'   grid::grid.draw()
#'   
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Create a complex shape
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' grid::grid.newpage()
#' f <- function(x, y, z) {
#'   (x-2)^2 * (x+2)^2 + 
#'   (y-2)^2 * (y+2)^2 + 
#'   (z-2)^2 * (z+2)^2 +
#'   3 * (x^2 * y^2 + x^2 * z^2 + y^2 * z^2) +
#'   6 * x * y * z -
#'   10 * (x^2 + y^2 + z^2) + 22
#' }
#' 
#' gen_isosurface(
#'   f = f,
#'   scale = 0.1,
#'   nx = 70
#' ) |> 
#'   isocubesGrob(size = 2) |> 
#'   grid::grid.draw()
#' @importFrom methods formalArgs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gen_isosurface <- function(f, upper = 0, lower = -Inf, scale = 1, nx = 51, ny = nx, nz = nx) {

  stopifnot(exprs = {
    is.function(f)
    identical(formalArgs(f), c('x', 'y', 'z'))
  })
  
  Nx <- as.integer(round(nx/2))
  Ny <- as.integer(round(ny/2))
  Nz <- as.integer(round(nz/2))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaludate the function within a regular grid
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- expand.grid(x = seq(-Nx, Nx), y = seq(-Ny, Ny), z = seq(-Nz, Nz))
  values <- with(coords, f(x * scale, y * scale, z * scale))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep only voxels where the value at or below the thresholds
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords[values > lower & values <= upper, ] 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate voxel coordinates for a sphere
#' @param r radius
#' @return data.frame of voxel coordinates
#' @examples
#' gen_sphere(1) |>
#'   isocubesGrob() |>
#'   grid::grid.draw()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gen_sphere <- function(r = 10) {
  gen_isosurface(
    f = function(x, y, z) {x^2 + y^2 + z^2},
    upper = r^2,
    nx = r * 2 + 1
  ) 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate a rectangular prism
#' 
#' To simplify implementation, only odd side lengths are generated.
#' 
#' @param x,y,z prism dimensions. Default 5x5x5
#' @return data.frame of voxel coordinates
#' @examples
#' gen_prism(3, 5, 7) |>
#'    isocubesGrob() |>
#'    grid::grid.draw()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gen_prism <- function(x = 5, y = x, z = x) {
  
  x <- ifelse(x %% 2 == 0, x, x - 1)
  y <- ifelse(y %% 2 == 0, y, y - 1)
  z <- ifelse(z %% 2 == 0, z, z - 1)
  
  stopifnot(x >= 0, y >= 0, z >= 0)
  
  expand.grid(
    x = seq(-x/2, x/2), 
    y = seq(-y/2, y/2), 
    z = seq(-z/2, z/2)
  ) 
}


