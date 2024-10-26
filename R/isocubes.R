



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cheap version of darkening a colour. much chepaer than colorspace package
#' @param fill vector of R colours
#' @param amount fraction to darken by
#' @noRd
#' @importFrom grDevices rgb col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cheap_darken <- function(fill, amount) {
  mat <- col2rgb(fill, alpha = TRUE)
  mat[1:3,] <- mat[1:3,] * (1 - amount)
  rgb(mat[1,], mat[2,], mat[3,], mat[4,], maxColorValue = 255)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob of isocubes
#' 
#' @param coords data.frame of x,y,z coordinates for the cubes (integer coordinates)
#' @param fill fill colour for the main face of cube as specified by the \code{light} 
#'        argument.  By default, this will be the colour of the top face of the cube, 
#'        as the default \code{light = 'top-left'}.
#' @param fill2,fill3 fill colours for secondary and tertiary faces of
#'        cube - as specified in the \code{light} argument.  
#'        If set to NULL (the default) then cube faces will be darkened versions 
#'        of the main \code{fill} color.   
#' @param light direction of light.  This is a two-word argument nominating the 
#'        main light direction and secondary light direction. The default value of
#'        'top-left' indicates the light is brightest from the top, and then 
#'        from the left side of the cube - with the right side of the cube being darkest.
#'        This argument is only used if \code{fill2} and \code{fill3} are unspecified.
#'        Possible values are: top-left, top-right, left-top, left-right, right-top,
#'        right-left.
#' @param xo,yo the origin of the isometric coordinate system in 'snpc' coordinates.
#'        These values should be given as vanilla floating point values.
#'        Be default the origin is the middle bottom of the graphics device 
#'        i.e. \code{(xo, yo) = (0.5, 0)}
#' @param verbosity Verbosity level. Default: 0
#' @param ... other values passed to \code{gpar()} to set the graphical
#'        parameters e.g. \code{lwd} and \code{col} for the linewidth and colour
#'        of the outline stroke for each cube face.
#' @param default.units Default unit is 'npc'
#' @param size dimensions of cube i.e. the length of the vertical edge of the cube.
#'        Default: 5mm
#' 
#' @return grid \code{grob} object
#' @examples
#' coords <- expand.grid(x = 0:1, y=0:2, z=0:3)
#' cols <- rainbow(nrow(coords))
#' iso <- isocubesGrob(coords, fill = cols)
#' grid::grid.draw(iso)
#' 
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isocubesGrob <- function(coords,  
                         fill          = 'grey90',  
                         fill2         = NULL, 
                         fill3         = NULL, 
                         light         = 'top-left',
                         size          = grid::unit(5  , 'mm'),
                         xo            = grid::unit(0.5, 'npc'), 
                         yo            = grid::unit(0.0, 'npc'),
                         default.units = 'npc',
                         verbosity     = 0, ...) {
  
  if (nrow(coords) == 0) {
    return(grid::nullGrob())
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot('x' %in% names(coords))
  stopifnot('y' %in% names(coords))
  stopifnot('z' %in% names(coords))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote units if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!grid::is.unit(size)) {
    size <- grid::unit(size, units = default.units)
  }  
  if (!grid::is.unit(xo)) {
    xo <- grid::unit(xo, units = default.units)
  }  
  if (!grid::is.unit(yo)) {
    yo <- grid::unit(yo, units = default.units)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # depth sort the cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords$x <- as.integer(round(coords$x))
  coords$y <- as.integer(round(coords$y))
  coords$z <- as.integer(round(coords$z))
  
  sort_order <- with(coords, order(-x, -z, y))
  coords     <- coords[sort_order,]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # which cubes are actually visible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Norig <- nrow(coords)
  visible <- visible_cubes(coords)
  if (verbosity) message("Visible cubes: ", sum(visible), " / ", nrow(coords))
  coords  <- coords[visible,]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare the fill colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(fill) == 1) {
    fill <- rep(fill, Norig)
  } else if (length(fill) != Norig) {
    stop("'fill' must be length = 1 or N")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rearrange colours to match depth-sorted cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill <- fill[sort_order]
  fill <- fill[visible]
  N    <- nrow(coords)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If 'fill2' not provided then just darken the provided colour by a factor of 0.3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(fill2)) {
    fill2 <- cheap_darken(fill, 0.3)
  } else {
    if (length(fill2) == 1) {
      fill2 <- rep(fill2, N)
    } else if (length(fill2) != Norig) {
      stop("'fill2' must be length = 1 or ", N, ", not ", length(fill2))
    }
    fill2 <- fill2[sort_order]
    fill2 <- fill2[visible]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If 'fill2' not provided then just darken the provided colour by a factor of 0.6
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(fill3)) {
    fill3 <- cheap_darken(fill, 0.6)
  } else {
    if (length(fill3) == 1) {
      fill3 <- rep(fill3, Norig)
    } else if (length(fill3) != Norig) {
      stop("'fill3' must be length = 1 or N")
    }
    fill3 <- fill3[sort_order]
    fill3 <- fill3[visible]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # rearrange the colour vector to match the polygons being drawn, 
  # i.e. (fill, fill_L, fill_R, fill, fill_L, fill_R, ...)
  # Polygons for faces are always drawn TOP, LEFT, then RIGHT
  # colors <- as.vector(rbind(fill, fill2, fill3))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  colors <- switch (
    light,         #               TOP ,  LEFT, RIGHT
    'top-left'   = as.vector(rbind(fill , fill2, fill3)),
    'top-right'  = as.vector(rbind(fill , fill3, fill2)),
    'left-top'   = as.vector(rbind(fill2, fill , fill3)),
    'left-right' = as.vector(rbind(fill3, fill , fill2)),
    'right-top'  = as.vector(rbind(fill2, fill3, fill )),
    'right-left' = as.vector(rbind(fill3, fill2, fill )),
    stop("'light' argument is not valid: ", light)
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Template for the cube at (0, 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  theta <- seq(90, 390, 60) * pi/180 
  x     <- cos(theta)
  y     <- sin(theta)
  xall  <- c(x[1], x[2], 0, x[6],  x[2], x[3], x[4], 0,  x[4], x[5], x[6], 0) * size + xo
  yall  <- c(y[1], y[2], 0, y[6],  y[2], y[3], y[4], 0,  y[4], y[5], y[6], 0) * size + yo
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the offset coordinates for each cube
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ix <- size * ((coords$x - coords$z) * cos(pi/6))
  iy <- size * ((coords$x + coords$z) * sin(pi/6) + coords$y) + size
  
  
  gp <- gpar(...)
  gp$fill <- colors
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a single polygonGrob representing *all* the faces
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cube <- polygonGrob(
    x             = xall + rep(ix, each = 12), 
    y             = yall + rep(iy, each = 12), 
    id.lengths    = rep(4, 3 * N), 
    gp            = gp
  )
  
  cube
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine which cubes are visible
#'
#'
#' @param coords integer coordinates of isocubes positions. This function
#'        assumes that coordintates have already sorted from front to back.
#'
#' @return logical vector indicating which cubes are visible
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visible_cubes <- function(coords) {
  
  rev(
    !duplicated(
      rev((coords$x - coords$z) * 1024L + ((coords$x + coords$z) * 0.5 + coords$y))
    )
  )
  
}
