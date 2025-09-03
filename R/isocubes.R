



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cheap version of darkening a colour. much chepaer than colorspace package
#' @param fill vector of R colours
#' @param frac intensity as a fraction
#' @noRd
#' @importFrom grDevices rgb col2rgb
#' @import colorfast
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_intensity <- function(fill, frac) {
  .Call(set_intensity_, fill, frac)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Template for fully visible isocube TYPE = 7 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theta <- seq(90, 390, 60) * pi/180 
xc0 <- cos(theta)
yc0 <- sin(theta)
# define polygons for the 3 faces of the cube
#       TOP                           LEFT                         RIGHT
# xc0 <- c(xc0[1], xc0[2], 0, xc0[6],   xc0[2], xc0[3], xc0[4], 0,   xc0[4], xc0[5], xc0[6], 0) 
# yc0 <- c(yc0[1], yc0[2], 0, yc0[6],   yc0[2], yc0[3], yc0[4], 0,   yc0[4], yc0[5], yc0[6], 0) 


# 0 0 1 = 1 = TOP
# 0 1 0 = 2 = LEFT
# 0 1 1 = 3 = TOP + LEFT
# 1 0 0 = 4 = RIGHT
# 1 0 1 = 5 = TOP + RIGHT
# 1 1 0 = 6 = TOP + LEFT
# 1 1 1 = 7 = TOP + LEFT + RIGHT


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0 0 1 = 1 = TOP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc1 <- c(xc0[1], xc0[2], 0, xc0[6]) 
yc1 <- c(yc0[1], yc0[2], 0, yc0[6]) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0 1 0 = 2 = LEFT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc2 <- c(xc0[2], xc0[3], xc0[4], 0) 
yc2 <- c(yc0[2], yc0[3], yc0[4], 0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0 1 1 = 3 = TOP + LEFT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc3 <- c(xc0[1], xc0[2], 0, xc0[6],   xc0[2], xc0[3], xc0[4], 0) 
yc3 <- c(yc0[1], yc0[2], 0, yc0[6],   yc0[2], yc0[3], yc0[4], 0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 0 0 = 4 = RIGHT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc4 <- c(xc0[4], xc0[5], xc0[6], 0) 
yc4 <- c(yc0[4], yc0[5], yc0[6], 0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 0 1 = 5 = TOP + RIGHT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc5 <- c(xc0[1], xc0[2], 0, xc0[6],   xc0[4], xc0[5], xc0[6], 0) 
yc5 <- c(yc0[1], yc0[2], 0, yc0[6],   yc0[4], yc0[5], yc0[6], 0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 1 0 = 6 = LEFT + RIGHT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc6 <- c(xc0[2], xc0[3], xc0[4], 0,   xc0[4], xc0[5], xc0[6], 0) 
yc6 <- c(yc0[2], yc0[3], yc0[4], 0,   yc0[4], yc0[5], yc0[6], 0) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 1 1 = 7 = TOP + LEFT + RIGHT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xc7 <- c(xc0[1], xc0[2], 0, xc0[6],   xc0[2], xc0[3], xc0[4], 0,   xc0[4], xc0[5], xc0[6], 0) 
yc7 <- c(yc0[1], yc0[2], 0, yc0[6],   yc0[2], yc0[3], yc0[4], 0,   yc0[4], yc0[5], yc0[6], 0) 

vertx = list(xc1, xc2, xc3, xc4, xc5, xc6, xc7)
verty = list(yc1, yc2, yc3, yc4, yc5, yc6, yc7)

face_nverts <- c(
  4L, # 0 0 1 = 1 = TOP
  4L, # 0 1 0 = 2 = LEFT
  8L, # 0 1 1 = 3 = TOP + LEFT
  4L, # 1 0 0 = 4 = RIGHT
  8L, # 1 0 1 = 5 = TOP + RIGHT
  8L, # 1 1 0 = 6 = TOP + LEFT
  12L  # 1 1 1 = 7 = TOP + LEFT + RIGHT
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob of isocubes representing the voxels at the given coordiantes
#' 
#' @param coords data.frame of x,y,z coordinates for the cubes (integer coordinates)
#' @param fill fill colour for the top face of cube. Default: NULL will attempt
#'        to use the 'fill' colour in the coords data.frame, otherwise 'grey50'
#' @param fill_left,fill_right fill colours for left and fight faces of
#'        cube. 
#' @param col Stroke colour for outline of cube faces. Default: black. If \code{NA}
#'        then no outlines will be drawn.  If negative, then outline colour
#'        will be the same as the face colour.
#' @param intensity c(1, 0.3, 0.6) Intensity shading for \code{fill} for the 
#'        top, left and right faces respectively.  Note: this setting has no effect 
#'         on the shading of the left face if \code{fill_left} has been set 
#'         explicitly by the user; same for the right face.
#' @param x,y the origin of the isometric coordinate system.
#'        If these values are given as vanilla floating point values, they 
#'        will be interpreted as 'npc' units, otherwise a valid grid unit 
#'        object must be supplied.
#'        By default the origin is the middle of the graphics device 
#'        i.e. \code{(x, y) = (0.5, 0.5)}
#' @param verbosity Verbosity level. Default: 0
#' @param ... other values passed to \code{gpar()} to set the graphical
#'        parameters e.g. \code{lwd} and \code{col} for the linewidth and colour
#'        of the outline stroke for each cube face.
#' @param default.units Default unit for size of a cube is 'mm'
#' @param size dimensions of cube i.e. the length of the vertical edge of the cube.
#'        Default: 5mm
#' @param xyplane How is the xyplane oriented with respect to the unit isometric
#'        cube?.  "left", "right", "flat" (or "top").
#'        Default: "flat".
#' @param handedness How is the z-axis positioned with respect to the xy-plane?
#'        I.e. is this a right-handed or left-handed coordinate system?
#'        Default: "right"
#' 
#' @return grid \code{grob} object
#' @examples
#' fill <- rainbow(nrow(obj_sphere))
#' isocubesGrob(obj_sphere, fill = fill, size = 2) |>
#'   grid::grid.draw()
#' 
#' # The 'obj_organic' data.frame includes a 'fill' column which will be
#' # used by default
#' isocubesGrob(obj_organic, size = 2) |>
#'   grid::grid.draw()
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isocubesGrob <- function(coords,  
                         fill          = NULL,  
                         fill_left     = NULL, 
                         fill_right    = NULL, 
                         intensity     = c(1, 0.3, 0.7),
                         size          = 5,
                         x             = 0.5, 
                         y             = 0.5,
                         col           = 'black',
                         default.units = 'mm',
                         xyplane       = 'flat',
                         handedness    = 'right',
                         verbosity     = 0, ...) {
  
  if (nrow(coords) == 0) {
    return(grid::nullGrob())
  }
  
  # Use it as a vanilla data.frame 
  coords <- as.data.frame(coords)
  
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
    coords$z <- -coords$z
    coords[, c(yidx, zidx)] <- coords[, c(zidx, yidx)]
  } else {
    stop("Not a supported coordinate system: xyplane: ", xyplane, "  hand: ", handedness)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote units if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!grid::is.unit(size)) {
    size <- grid::unit(size, units = default.units)
  }  
  if (!grid::is.unit(x)) {
    x <- grid::unit(x, units = 'npc')
  }  
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, units = 'npc')
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # depth sort the cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords$x <- as.integer(round(coords$x))
  coords$y <- as.integer(round(coords$y))
  coords$z <- as.integer(round(coords$z))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # which cubes are actually visible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Norig <- nrow(coords)
  visible_df <- visible_cubes_c(coords)
  visible <- visible_df$idx
  if (verbosity) message("Visible cubes: ", length(visible), " / ", nrow(coords))
  coords <- coords[visible,]
  N      <- nrow(coords)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare the fill colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill <- fill %||% coords[['fill']] %||% '#B3B3B3FF'
  
  if (length(fill) == 1) {
    fill <- rep(fill, N)
  } else if (length(fill) == N) {
    # all good
  } else if (length(fill) == Norig) {
    fill <- fill[visible]
  } else {
    stop("Bad fill length: ", length(fill))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If 'fill_left' not provided then just darken the provided colour by a factor of 0.3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_left <- fill_left %||% coords[['fill_left']] %||% set_intensity(fill, intensity[2])
  
  if (length(fill_left) == 1) {
    fill_left <- rep(fill_left, N)
  } else if (length(fill_left) == N) {
    # All good
  } else if (length(fill_left) == Norig) {
    fill_left <- fill_left[visible]
  } else {
    stop("Bad fill_left length: ", length(fill_left))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If 'fill_left' not provided then just darken the provided colour by a factor of 0.6
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_right <- fill_right %||% coords[['fill_right']] %||% set_intensity(fill, intensity[3])
  
  if (length(fill_right) == 1) {
    fill_right <- rep(fill_right, N)
  } else if (length(fill_right) == N) {
    # All good
  } else if (length(fill_right) == Norig) {
    fill_right <- fill_right[visible]
  } else {
    stop("Bad fill_right length: ", length(fill_right))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set intensity of the primary colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill <- set_intensity(fill, intensity[1])
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Still need to do reordering for rendering from back-to-front as
  # per-face visibility still requires it.
  # Need to shift to half-face-visibility checks if I wanted to totally
  # avoid this sort. 
  # Notes on half-face visibilty
  #   - each half-face is a 1/6 segment of a hexagon
  #   - The same number of polygons are drawn for half-face visiblity as 
  #     needed for per-face visiblity
  #   - half-face visibliity would avoid having to update sort ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sort_order <- order(-coords$x, -coords$z, coords$y)
  coords     <- coords[sort_order, c('x', 'y', 'z')]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rearrange colours to match depth-sorted cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill       <- fill      [sort_order]
  fill_left  <- fill_left [sort_order]
  fill_right <- fill_right[sort_order]
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the offset coordinates for each cube
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ix <- ((coords$x - coords$z) * cos(pi/6))
  iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
  
  
  gp <- gpar(...)
  
  
  if (TRUE) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Per-face visibility
    # Each different cube has a different number of verts based upon its visibility type
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    type <- visible_df$type[sort_order]

    fill      [!bitwAnd(type, 1L)] <- NA
    fill_left [!bitwAnd(type, 2L)] <- NA
    fill_right[!bitwAnd(type, 4L)] <- NA
    
    
    all_fills <- as.vector(rbind(fill, fill_left, fill_right))
    all_fills <- all_fills[!is.na(all_fills)]
    gp$fill <- all_fills
    
    
    if (!is.na(col[[1]]) && col[[1]] < 0) {
      gp$col <- all_fills
    } else {
      gp$col <- col
    }

    
    nverts_per_cube <- face_nverts[type]
    id.lengths <- rep.int(4L, sum(nverts_per_cube)/4)
    xc <- vertx[type] |> unlist(recursive = FALSE, use.names = FALSE)
    yc <- verty[type] |> unlist(recursive = FALSE, use.names = FALSE)
    
    offx <- rep.int(ix, nverts_per_cube)
    offy <- rep.int(iy, nverts_per_cube)
    
    stopifnot(length(xc) == length(offx))
    stopifnot(length(yc) == length(offy))
    
    # message("N locs: ", nrow(visible_df))
    # message("nverts_per_cube: ", deparse1(nverts_per_cube))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a single polygonGrob representing *all* the faces
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cube <- polygonGrob(
      #             scale  *   template poly +   offsets
      x             = size * (xc + offx),
      y             = size * (yc + offy),
      id.lengths    = id.lengths, 
      gp            = gp,
      vp = grid::viewport(x, y, just = c(0, 0))
    )
    
  } else {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a single polygonGrob representing *all* the faces
    # This uses per-cube visibility meaning that a full fube (3 polygons)
    # is drawn at each step.  
    # This may be faster to calculate, but the actual rendering 
    # benefits from per-face visiblity and having fewer polygons to plot
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    type <- visible_df$type[sort_order]
    
    fill      [!bitwAnd(type, 1L)] <- NA
    fill_left [!bitwAnd(type, 2L)] <- NA
    fill_right[!bitwAnd(type, 4L)] <- NA
    
    all_fills <- as.vector(rbind(fill, fill_left, fill_right))
    gp$fill <- all_fills
    
    cube <- polygonGrob(
      #             scale  *   template poly +   offsets
      x             = size * (xc7           +   rep(ix, each = 12)),
      y             = size * (yc7           +   rep(iy, each = 12)),
      id.lengths    = rep(4, 3 * N), 
      gp            = gp,
      vp = grid::viewport(x, y, just = c(0, 0))
    )
  }
  
  cube
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine which isocubes are visible
#'
#' @param coords integer coordinates of voxel positions. This function
#'        assumes that coordinates have already sorted from front to back.
#'        i.e.   \code{sort_order <- order(-coords$x, -coords$z, coords$y);
#'        coords <- coords[sort_order,]}
#'
#' @return interger vector of indcies of visible isocubes in the \code{coords}
#'         dataset
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visible_cubes_r <- function(coords) {
  
  which(
    !duplicated(
      (coords$x - coords$z) * 1024L + ((coords$x + coords$z) * 0.5 + coords$y),
      fromLast = TRUE
    )
  )
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine which isocubes are visible
#'
#' @param coords integer coordinates of voxel positions. This function does
#'        not require any specific ordering of the voxel coordinates.
#'
#' @return interger vector of indcies of visible voxels in the \code{coords}
#'         dataset
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visible_cubes_c <- function(coords) {
  vis_df <- .Call(visibility_, coords$x, coords$y, coords$z)
  
  if (any(vis_df$type == 0)) {
    warning("------------------------------------------------->  type = 0")
  }
  
  vis_df
}


