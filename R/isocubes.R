

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob of isocubes
#' 
#' @param coords data.frame of x,y,z coordinates for the cubes (integer coordinates)
#' @param fill fill colour for top face of cube.
#' @param fill_left,fill_right fill colours for left and right faces of
#'        cube.  If set to NULL (the default) then the left and right 
#'        faces will be darkened versions of the given \code{fill} color.   
#' @param ysize the size of a cube as a fraction of the window height. Default: 0.05, 
#'        which means that, at most, 20 cubes could stack from top to bottom and be 
#'        visible.
#' @param xo,yo the origin of the isometric coordinate system in 'snpc' coordinates.
#'        These values should be given as vanilla floating point values.
#'        Be default the origin is the middle bottom of the graphics device 
#'        i.e. \code{(xo, yo) = (0.5, 0)}
#' @param occlusion_depth How deep should the occlusion checking go in order to 
#'        find cubes which are not visible.  Higher numbers will take more time,
#'        but will remove move cubes which are not visible.
#'        The default value of 2 is a good tradeoff that works well for solid
#'        structures while removing some hidden cubes from sparse structures.
#'        A value of 0 means to not remove any hidden cubes.
#' @param verbose Be verbose? Default FALSE.
#' @param ... other values passed to \code{gpar()} to set the graphical
#'        parameters e.g. \code{lwd} and \code{col} for the linewidth and colour
#'        of the outline stroke for each cube face.
#' 
#' @import grid
#' @import colorspace
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isocubesGrob <- function(coords, fill = 'grey90', fill_left = NULL, fill_right = NULL, 
                         ysize = 1/20, xo = 0.5, yo = ysize, occlusion_depth = 2L,
                         verbose = FALSE, ...) {
  
  if (nrow(coords) == 0) {
    return(grid::nullGrob())
  }
  
  sf <- 1/ysize # Scale-factor
  
  # depth sort the cubes
  coords$x <- as.integer(round(coords$x))
  coords$y <- as.integer(round(coords$y))
  coords$z <- as.integer(round(coords$z))
  
  sort_order <- with(coords, order(-x, -z, y))
  coords     <- coords[sort_order,]
  
  # which cubes are actually visible
  N       <- nrow(coords)
  visible <- visible_cubes(coords, n = occlusion_depth)
  if (verbose) message("Visible cubes: ", sum(visible), " / ", nrow(coords))
  coords  <- coords[visible,]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare the fill colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(fill) == 1) {
    fill <- rep(fill, N)
  } else if (length(fill) != N) {
    stop("'fill' must be length = 1 or N")
  }
  
  # Rearrange colours to match depth-sorted cubes
  fill <- fill[sort_order]
  fill <- fill[visible]
  N    <- nrow(coords)
  
  if (is.null(fill_left)) {
    fill_left <- colorspace::darken(fill, 0.3)
  }
  
  if (is.null(fill_right)) {
    fill_right <- colorspace::darken(fill, 0.6)
  }
  
  # rearrange the colour vector to match the polygons being drawn, 
  # i.e. (fill, fill_L, fill_R, fill, fill_L, fill_R, ...)
  colors <- as.vector(rbind(fill, fill_left, fill_right))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Template for the cube at (0, 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  theta <- seq(90, 390, 60) * pi/180 
  x     <- cos(theta)
  y     <- sin(theta)
  xall  <- c(x[1], x[2], 0, x[6],  x[2], x[3], x[4], 0,  x[4], x[5], x[6], 0)/sf + xo
  yall  <- c(y[1], y[2], 0, y[6],  y[2], y[3], y[4], 0,  y[4], y[5], y[6], 0)/sf + yo
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the offset coordinates for each cube
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ix <-1/sf * ((coords$x - coords$z) * cos(pi/6))
  iy <-1/sf * ((coords$x + coords$z) * sin(pi/6) + coords$y)
  
  
  gp <- gpar(...)
  gp$fill <- colors
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a single polygonGrob representing *all* the faces
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cube <- polygonGrob(
    x             = xall + rep(ix, each = 12), 
    y             = yall + rep(iy, each = 12), 
    id.lengths    = rep(4, 3 * N), 
    default.units = 'snpc',
    gp            = gp
  )
  
  cube
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine which cubes are hidden behind other cubes.
#'
#' This is quite a naive algorithm which only removes cubes which are
#' immediately behind other cubes.
#' 
#' @param coords integer coordinates of isocubes positions
#' @param n blocking depth to check.  Higher numbers mean more work
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visible_cubes <- function(coords, n) {
  
  hash         <- with(coords , x + 256L * y + 256L * 256L * z)
  is_blocked   <- logical(nrow(coords))
  blocked      <- coords
  
  for (i in seq_len(n)) {
    blocked$x <- blocked$x + 1L
    blocked$y <- blocked$y - 1L
    blocked$z <- blocked$z + 1L
    
    hash_blocked <- with(blocked, x + 256L * y + 256L * 256L * z)
    
    is_blocked <- is_blocked | (hash %in% hash_blocked)
  }
  
  !is_blocked
}


