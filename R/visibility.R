
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate indices of visible voxels when rendered from the specified view.
#' 
#' Returned value is depth-sorted in back-to-front rendering order
#' 
#' @inheritParams isocubesGrob
#' @param value type of value to return. Default: 'index'.  Valid values are
#'        'index' and 'full'.  If 'index', then returns an integer vector of 
#'        which rows to render in back-to-front ordering.  'full' returns
#'        more information in a data.frame
#' @return if value argument is 'index' then integer vector of visible 
#'         vertices in back-to-front draw ordering.
#'         For \code{value = 'full'} return a data.frame with more complete
#'         information.
#' @examples
#' nrow(obj_sphere)
#' calc_visibility(obj_sphere) |>
#'    length()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_visibility <- function(coords,  
                            xyplane       = 'flat',
                            handedness    = 'right',
                            value         = 'index',
                            verbosity     = 0, ...) {
  
  if (nrow(coords) == 0) {
    return(grid::nullGrob())
  }
  
  # Use it as a vanilla data.frame 
  coords <- as.data.frame(coords)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    all(c('x', 'y', 'z') %in% names(coords))
    value %in% c('index', 'full')
  })
  
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
  # depth sort the cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords$x <- as.integer(round(coords$x))
  coords$y <- as.integer(round(coords$y))
  coords$z <- as.integer(round(coords$z))
  coords$idx <- seq_len(nrow(coords))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # which cubes are actually visible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Norig <- nrow(coords)
  visible_df <- visible_cubes_c(coords)
  visible <- visible_df$idx
  if (verbosity) message("Visible cubes: ", length(visible), " / ", nrow(coords))
  
  if (value == 'full') {
    coords <- coords[visible,]
    N      <- nrow(coords)
    
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
    coords     <- coords[sort_order, ]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate the offset coordinates for each cube
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    coords$ix <- ((coords$x - coords$z) * cos(pi/6))
    coords$iy <- ((coords$x + coords$z) * sin(pi/6) + coords$y + 1)
    
    coords 
  } else if (value == 'index') {
    sort_order <- order(-coords$x[visible], -coords$z[visible], coords$y[visible])
    visible[sort_order]
  } else {
    stop("nope")
  }
}




if (FALSE) {
  calc_visibility(obj_sphere, value = 'full') -> df
  calc_visibility(obj_sphere, value = 'index') -> idx
  plot(df$ix, df$iy)
  
  grid.newpage()
  
  for (i in seq_len(nrow(df))) {
    grid.polygon(
      x = unit(vertx[[7]] * 5 + 5 * df$ix[i], 'mm') + unit(0.5, 'npc'), 
      y = unit(verty[[7]] * 5 + 5 * df$iy[i], 'mm') + unit(0.5, 'npc'), 
      id.lengths = c(4, 4, 4),
      gp = gpar(fill = c('grey90', 'gray60', 'gray40'))
    )  
  }
  
  grid::grid.draw(isocubesGrob(obj_sphere))
}


