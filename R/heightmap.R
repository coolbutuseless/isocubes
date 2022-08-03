

globalVariables('y')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate isocubes coordinates from a height matrix
#' 
#' @param mat integer matrix. The matrix will be interpreted as cubes flat on the
#'        page, with the value in the matrix interpreted as the height above the page.
#' @param col matrix of colours the same dimensions as the \code{mat} argument.
#'        Default: NULL.   If \code{col} is not NULL, 
#'        then a \code{col} column will be included in the final returned coordinates.
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
#' @param ground Orientation of the ground plane. Default: 'xz'.  Possible
#'        values 'xz', 'xy'
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords_heightmap <- function(mat, col = NULL, scale = 1, flipx = FALSE, flipy = TRUE, 
                             ground = 'xz', solid = TRUE, verbose = FALSE) {
  
  verbose <- isTRUE(verbose)
  solid   <- isTRUE(solid)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check matrix sizes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(col)) {
    stopifnot(identical(dim(col), dim(mat)))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flip Matrrix Horizontally
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(flipx)) {
    mat <- mat[,rev(seq_len(ncol(mat)))]
    if (!is.null(col)) {
      col <- col[,rev(seq_len(ncol(col)))]
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flip matrix vertically
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(flipy)) {
    mat <- mat[rev(seq_len(nrow(mat))),]
    if (!is.null(col)) {
      col <- col[rev(seq_len(nrow(col))),]
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
  
  coords$col <- as.vector(col)
  
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
    coords$z <- -coords$y
    coords$y <- tmp
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep only visible cubes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  start <- nrow(coords)
  idx <- visible_cubes(coords)
  coords <- coords[idx, ]
  
  
  coords
}









if (FALSE) {
  coords <- coords_sphere(0, 18, 0, 10)
  cubes <- isocubesGrob(coords, ysize = 1/45, fill = 'red', fill2 = 'white', fill3 = 'blue')
  grid.newpage()
  grid.draw(cubes)
  
}



if (FALSE) {
  mat <- volcano
  mat[seq(nrow(mat)),] <- mat[rev(seq(nrow(mat))),]
  val <- as.vector(mat)
  val <- round(255 * (val - min(val)) / diff(range(val)))
  col <- matrix("", nrow=nrow(mat), ncol=ncol(mat))
  col[] <- viridisLite::inferno(256)[val + 1L]
  
  coords <- coords_heightmap(mat - min(mat), col = col, scale = 0.3)
  cubes  <- isocubesGrob(coords, ysize = 1/100, fill = coords$col)
  
  x11(type = 'dbcairo', width = 10, height = 10)
  dev.control('inhibit')
  grid.newpage()
  dev.hold()
  grid.draw(cubes)
  dev.flush()  
}












