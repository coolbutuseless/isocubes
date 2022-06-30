

if (FALSE) {
  mat <- col2rgb(colours())
  rgb(mat[1,], mat[2,], mat[3,], maxColorValue = 255)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a colour into a standard hex representation
#' 
#' @param colour could be an R colour like 'red', 'lightblue'
#' 
#' @return colour as hex colour with '#' prefix
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
canonicalise_colour <- function(colour) {
  stopifnot(length(colour) == 1 && is.character(colour) && !is.na(colour))
  if (startsWith(colour, '#')) return(colour)
  mat <- col2rgb(colour)
  rgb(mat[1,], mat[2,], mat[3,], maxColorValue = 255)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Naive linear mixing of colours in RGB space
#' 
#' @param colour1,colour2 colours to mix
#' @param frac mix fraction
#' @noRd
#'
#' @importFrom grDevices rgb col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mix_colours <- function(colour1, colour2, frac) {
  res1 <- t(col2rgb(colour1))
  res2 <- t(col2rgb(colour2))
  res <- (1 - frac) * res1  + frac * res2
  rgb(res[,1], res[,2], res[,3], maxColorValue = 255)
}


if (FALSE) {
  
  N <- 100*100*100
  mat     <- col2rgb(colours())
  colour  <- rgb(mat[1,], mat[2,], mat[3,], maxColorValue = 255)
  
  colour1 <- sample(colour, N, replace = TRUE)
  colour2 <- sample(colour, N, replace = TRUE)
  frac    <- runif(N)
  system.time({
    final   <- mix_colours(colour1, colour2, frac)
  })
}