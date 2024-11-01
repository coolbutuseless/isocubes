


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



theta <- seq(90, 390, 60) * pi/180 
xc0 <- cos(theta)
yc0 <- sin(theta)
# define polygons for the 3 faces of the cube
xc0 <- c(xc0[1], xc0[2], 0, xc0[6],   xc0[2], xc0[3], xc0[4], 0,   xc0[4], xc0[5], xc0[6], 0) 
yc0 <- c(yc0[1], yc0[2], 0, yc0[6],   yc0[2], yc0[3], yc0[4], 0,   yc0[4], yc0[5], yc0[6], 0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob of isocubes
#' 
#' @param coords data.frame of x,y,z coordinates for the cubes (integer coordinates)
#' @param fill fill colour for the top face of cube. Default: NULL will attempt
#'        to use the 'fill' colour in the coords data.frame, otherwise 'grey50'
#' @param fill_left,fill_right fill colours for left and fight faces of
#'        cube. 
#' @param intensity c(1, 0.3, 0.6) Intensity shading for fill, fill_left and
#'        fill_right
#' @param x,y the origin of the isometric coordinate system in 'snpc' coordinates.
#'        These values should be given as vanilla floating point values.
#'        Be default the origin is the middle bottom of the graphics device 
#'        i.e. \code{(x, y) = (0.5, 0)}
#' @param verbosity Verbosity level. Default: 0
#' @param ... other values passed to \code{gpar()} to set the graphical
#'        parameters e.g. \code{lwd} and \code{col} for the linewidth and colour
#'        of the outline stroke for each cube face.
#' @param default.units Default unit for (x,y) position is 'npc'
#' @param default.units.cube Default unit for size of a cube is 'mm'
#' @param size dimensions of cube i.e. the length of the vertical edge of the cube.
#'        Default: 5mm
#' @param xyplane How is the xyplane oriented with respect to the unit isometric
#'        cube?.  "left", "right", "top" (or "flat").
#'        Default: "right"
#' @param handedness How is the z-axis positioned with respect to the xy-plane?
#'        I.e. is this a right-handed or left-handed coordinate system?
#'        Default: "left"
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
                         fill          = NULL,  
                         fill_left     = NULL, 
                         fill_right    = NULL, 
                         intensity     = c(1, 0.6, 0.3),
                         size          = 5,
                         x             = NULL, 
                         y             = NULL,
                         default.units = 'npc',
                         default.units.cube = 'mm',
                         xyplane       = 'right',
                         handedness    = 'left',
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
  # Orient the axes
  # Default: xy-plane is the *right-hand* face of the cube
  #          z-axis goes into the plane
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cols <- match(c('x', 'y', 'z'), colnames(coords))
  xidx <- cols[1]
  yidx <- cols[2]
  zidx <- cols[3]
  if (xyplane == 'right' && handedness == "left") {
    # do nothing
  } else if (xyplane == 'right' && handedness == "right") {
    coords[[zidx]] <- -coords[[zidx]]
  } else if (xyplane == 'left' && handedness == "left") {
    # newz = -x,   newx = z
    # Swap x,z.  Negate z
    coords[, c(xidx, zidx)] <- coords[, c(zidx, xidx)]
    coords$z <- -coords$z
  } else if (xyplane == 'left' && handedness == "right") {
    # newz = -x,   newx = -z
    # Swap x,z.  Negate x. negate z
    coords[, c(xidx, zidx)] <- coords[, c(zidx, xidx)]
    coords$z <- -coords$z
    coords$x <- -coords$x
  } else if (xyplane %in% c('top', 'flat') && handedness == 'right') {
    # Swap yz
    coords[, c(yidx, zidx)] <- coords[, c(zidx, yidx)]
  } else if (xyplane %in% c('top', 'flat') && handedness == 'left') {
    # Swap yz. Negate z
    coords[, c(yidx, zidx)] <- coords[, c(zidx, yidx)]
    coords$z <- -coords$z
  } else {
    stop("Not yet supported: xyplane: ", xyplane, "  hand: ", handedness)
  }
  
  
  x <- x %||% grid::unit(0.5, 'npc')
  y <- y %||% grid::unit(0.5, 'npc')
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote units if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!grid::is.unit(size)) {
    size <- grid::unit(size, units = default.units.cube)
  }  
  if (!grid::is.unit(x)) {
    x <- grid::unit(x, units = default.units)
  }  
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, units = default.units)
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
  fill <- fill %||% coords[['fill']] %||% 'grey70'
  
  if (length(fill) == 1) {
    fill <- rep(fill, Norig)
  } else if (length(fill) != Norig) {
    stop("'fill' must be length = 1 or N")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If 'fill_left' not provided then just darken the provided colour by a factor of 0.3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_left <- fill_left %||% coords[['fill_left']] %||% cheap_darken(fill, intensity[2])
  
  if (length(fill_left) == 1) {
    fill_left <- rep(fill_left, Norig)
  } else if (length(fill_left) != Norig) {
    stop("'fill_left' must be length = 1 or ", Norig, ", not ", length(fill_left))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If 'fill_left' not provided then just darken the provided colour by a factor of 0.6
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_right <- fill_right %||% coords[['fill_right']] %||% cheap_darken(fill, intensity[3])
  
  if (length(fill_right) == 1) {
    fill_right <- rep(fill_right, Norig)
  } else if (length(fill_right) != Norig) {
    stop("'fill_right' must be length = 1 or ", Norig, ", not ", length(fill_left))
  }
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rearrange colours to match depth-sorted cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill       <- fill      [sort_order]
  fill_left  <- fill_left [sort_order]
  fill_right <- fill_right[sort_order]
  
  fill       <- fill      [visible]
  fill_left  <- fill_left [visible]
  fill_right <- fill_right[visible]
  
  N    <- nrow(coords)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Interleave colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  colors <- as.vector(rbind(fill, fill_left, fill_right))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Template for the cube at (0, 0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xc <- xc0 * size + x
  yc <- yc0 * size + y
  
  
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
    x             = xc + rep(ix, each = 12), 
    y             = yc + rep(iy, each = 12), 
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
