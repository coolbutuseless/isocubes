
"%||%" <- function (x, y) 
  if (is.null(x)) y else x


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate a random colour palette
#' 
#' @param N number of colors
#' @param seed integer seed. Default: NULL
#' @return character vector of colors
#' @examples
#' rand_palette(N = 20)
#' @importFrom grDevices colors
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rand_palette <- function(N = 256, seed = NULL) {
  
  # Random palette generator for R
  # https://iquilezles.org/articles/palettes/
  
  if (!is.null(seed)) set.seed(seed)
  
  a <- c(0.5, 0.5, 0.5)
  b <- (sample(colors(), 1) |> col2rgb() |> as.vector()) / 255
  c <- (sample(colors(), 1) |> col2rgb() |> as.vector()) / 255
  d <- (sample(colors(), 1) |> col2rgb() |> as.vector()) / 255
  
  mycol <- function(t) {
    a + b * cos(2 * pi * (c * t + d))
  }
  
  pal <- vapply(seq(0, 1, length.out = N), mycol, double(3))
  pal[pal > 1] <- 1
  
  rgb(t(abs(pal)))
}


if (FALSE) {
  library(grid)
  i <- 1
  coords <- gen_sphere(r = 20) 
  
  for (i in seq(100)) {
  
    fill <- rand_palette(N = nrow(coords), seed = i)  
    filename <- sprintf("working/pal/im-%03i.png", i)
    png(filename, width = 800, height = 800)
    isocubesGrob(coords, size = 3, fill = fill) |> grid.draw()
    # scales::show_col(cols, labels = FALSE, borders = FALSE)  
    dev.off()
  }
  
}


