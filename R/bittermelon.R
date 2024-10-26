


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 'bittermelon' bitmap list into a set of voxel coordinates 
#' 
#' @param bnl \code{bittermelon} list as created with \code{bittermelon::as_bm_list()}
#' @param sep x separation between characters. If NULL (the default), then 
#'     separation between characters will be determined from the \code{bml} data
#' 
#' @return data.frame of x,y,z coordinates
#' @examplesIf interactive()
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font      <- bittermelon::read_hex(font_file)
#' bml       <- bittermelon::as_bm_list("#RStats!", font = font)
#' coords    <- bm_coords(bml, font)
#' coords
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bm_coords <- function(bml, sep = NULL) {
  
  if (length(bml) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), z = numeric(0)))
  }
  
  if (is.null(sep)) {
    sep = ncol(bml[[1]]) + 0L
  }
  
  coords <- lapply(seq_along(bml), function(i) {
    mat <- bml[[i]]
    mat <- t(mat)
    vec <- which(unclass(mat) == 1)
    coords <- arrayInd(vec, dim(mat))
    coords <- as.data.frame(coords)
    names(coords) <- c('x', 'y')
    coords$idx <- i
    coords$x <- coords$x + i * sep
    coords
  })
  
  coords <- do.call(rbind, coords)
  coords$z <- 0
  
  coords
}


if (FALSE) {
  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
  font      <- bittermelon::read_hex(font_file)
  bml       <- bittermelon::as_bm_list("#RStats!", font = font)
  coords    <- bm_coords(bml)
  coords
}