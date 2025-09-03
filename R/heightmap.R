

globalVariables('y')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate voxel coordinates from a matrix where values indicate height
#' 
#' @param mat integer matrix. The matrix will be interpreted as cubes flat on the
#'        page, with the value in the matrix interpreted as the height above the page.
#' @param fill matrix of colours the same dimensions as the \code{mat} argument.
#'        Default: NULL.   If \code{fill} is not NULL, 
#'        then a \code{fill} column will be included in the final returned coordinates.
#' @param scale scale factor for values in matrix. Default = 1
#' @param solid Should the heightmap be made 'solid' i.e. without holes?
#'        default: TRUE.  This can be an expensive operation in terms of 
#'        both memory and CPU, but should be OK for simple examples.
#'        Set to FALSE if things take too long.  This operation works by 
#'        extruding cubes down from the top of the height map to the floor to 
#'        ensure gaps do not appear when the slope is too great.
#' @param verbose Be verbose? default: FALSE
#' @param flipx,flipy Should the matrix be flipped in the horizontal/vertical directions (respectively)?
#'        Default: \code{flipx = FALSE}, \code{flipy = TRUE}.
#'        
#'        Note: \code{flipy} defaults to \code{TRUE} as matrices are indexed
#'        from the top-down, but the isometric coordinate space is increasing
#'        from the bottom up.   Flipping the matrix vertically is usually 
#'        what you want.
#' @param ground Orientation of the ground plane. Default: 'xy'.  Possible
#'        values 'xz', 'xy'
#' @param check_visibility Should non-visible cubes be removed? Default: FALSE.
#'        If you plan on rotating or manipulating the returned coordinates then
#'        this should definitely by FALSE.  If TRUE, then non-visible voxels 
#'        will be entirely removed from the returned coordinates i.e. 
#'        they will be missing if you change the rendering viewpoint from
#'        the default.   
#' @return data.frame of voxel coordinates
#' @examples
#' # Plot the standard volcano 
#' mat <- volcano
#' 
#' # normalise height
#' mat <- mat - min(mat)
#' 
#' # Assign a distinct colour for each height value
#' val <- as.vector(mat)
#' val <- round(255 * val / max(val))
#' fill <- matrix("", nrow=nrow(mat), ncol=ncol(mat))
#' fill[] <- terrain.colors(256)[val + 1L]
#' 
#' # Calculate coordinates of heightmap, render as isocubes
#' coords <- calc_heightmap_coords(mat, fill = fill, scale = 0.3)
#' head(coords)
#' isocubesGrob(coords, size = 2, y = 0) |>
#'   grid::grid.draw()
#' @importFrom grDevices terrain.colors
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_heightmap_coords <- function(mat, fill = NULL, scale = 1, flipx = FALSE, flipy = TRUE, 
                             ground = 'xy', solid = TRUE, 
                             check_visibility = FALSE,
                             verbose = FALSE) {
  
  verbose <- isTRUE(verbose)
  solid   <- isTRUE(solid)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check matrix sizes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(fill)) {
    stopifnot(identical(dim(fill), dim(mat)))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flip Matrrix Horizontally
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(flipx)) {
    mat <- mat[,rev(seq_len(ncol(mat)))]
    if (!is.null(fill)) {
      fill <- fill[,rev(seq_len(ncol(fill))), drop = FALSE]
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flip matrix vertically
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(flipy)) {
    mat <- mat[rev(seq_len(nrow(mat))),]
    if (!is.null(fill)) {
      fill <- fill[rev(seq_len(nrow(fill))), , drop = FALSE]
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- data.frame(
    x = rep(seq_len(ncol(mat)), each = nrow(mat)),
    z = rep(seq_len(nrow(mat)), times = ncol(mat)),
    y = round(as.vector(mat) * scale)
  )
  
  coords$fill <- as.vector(fill)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extrude the cubes down to the ground
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (solid) {
    start <- nrow(coords)
    coords_list <- lapply(seq(max(coords$y)), function(yfloor) {
      df <- subset(coords, y >= yfloor)
      df$y <- yfloor 
      df
    })
    
    coords <- do.call(rbind, coords_list)
    if (verbose) message("Making solid expands cube count from ", start, " to ", nrow(coords))
  }  
  
  if (ground == 'xz') {
    # Do nothing
  } else if (ground == 'xy') {
    # Swap y/z coordinates
    tmp      <- coords$z
    coords$z <- coords$y
    coords$y <- tmp
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep only visible cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (check_visibility) {
    idx <- visible_cubes_c(coords)$idx
    coords <- coords[idx, ]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Min cube should be (0, 0, 0) not (1, 1, 10)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords$x <- coords$x - 1L
  coords$y <- coords$y - 1L
  coords$z <- coords$z - 1L
  
  coords
}



