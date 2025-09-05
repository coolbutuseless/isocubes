

library(grid)
library(isocubes)

A <- cospi(3/4); B <- sinpi(3/4)
f <- function(x, y, z) {
  z^4*B^2 + 4*x*y^2*A*B^2 + x*z^2*A*B^2 - 2*z^4*A - 4*x*y^2*B^2 - x*z^2*B^2 + 
    3*z^2*A*B^2 - 2*z^4 - x*A*B^2 - 2*z^2*A + x*B^2 + A*B^2 + 2*z^2 - B^2
}

N <- 31
x <- y <- z <- seq(-N, N) 
coords <- expand.grid(x = x, y = y, z = z)
keep <- with(
  coords, 
  sqrt(x*x + y*y + z*z) < 10*3 & f(x/10, y/10, z/10) < 0 & f(x/10, y/10, z/10) > -2
) 
coords <- coords[keep,]

coords$fill <- rgb(red = 1 + coords$x/N, 1 + coords$y/N, 1 + coords$z/N, maxColorValue = 2)
coords[, c('x', 'y', 'z')] <- coords[ , c('z', 'x', 'y')]


obj_organic <- coords[, c('x', 'y', 'z', 'fill')]
row.names(obj_organic) <- NULL
obj_organic <- dplyr::as_tibble(obj_organic)


cubes <- isocubesGrob(obj_organic, size = 2)
grid.newpage()
grid.draw(cubes)



usethis::use_data(obj_organic, internal = FALSE, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sphere
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(grid)
library(isocubes)

N      <- 16
coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
keep   <- with(coords, sqrt(x * x + y * y + z * z)) < N
obj_sphere <- coords[keep,]


obj_sphere <- obj_sphere[, c('x', 'y', 'z')]
row.names(obj_sphere) <- NULL
obj_sphere <- dplyr::as_tibble(obj_sphere)



cubes <- isocubesGrob(obj_sphere, size = 2)
grid.newpage()
grid.draw(cubes)

usethis::use_data(obj_sphere, internal = FALSE, overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- c(9, 8, 7, 6, 5, 4, 3, 2, 10, 9, 3, 2, 11, 10, 3, 2, 11, 10, 
       3, 2, 11, 10, 3, 2, 11, 10, 3, 2, 10, 9, 3, 2, 9, 8, 7, 6, 5, 
       4, 3, 2, 10, 9, 3, 2, 11, 10, 3, 2, 11, 10, 3, 2, 11, 10, 3, 
       2, 11, 10, 3, 2, 11, 10, 3, 2, 11, 10, 3, 2) - 2

y <- c(15, 15, 15, 15, 15, 15, 15, 15, 14, 14, 14, 14, 13, 13, 13, 
       13, 12, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 
       9, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 
       4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1) - 1


obj_letter <- data.frame(x = as.integer(x), y = as.integer(y), z = 0L)

usethis::use_data(obj_letter, internal = FALSE, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cube
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
obj_cube <- expand.grid(x = seq(-1, 1), y = seq(-1, 1), z = seq(-1, 1))
obj_cube$fill <- rev(rainbow(nrow(obj_cube)))

usethis::use_data(obj_cube, internal = FALSE, overwrite = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# block axis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
obj_test <- rbind(
  data.frame(
    x    = 1:5,
    y    = 0,
    z    = 0,
    fill = 'red'
  ),
  data.frame(
    x    = 0,
    y    = 1:5,
    z    = 0,
    fill = 'green'
  ),
  data.frame(
    x    = 0,
    y    = 0,
    z    = 1:5,
    fill = 'blue'
  ),
  data.frame(x = 0, y = 0, z = 0, fill = 'grey50')
)

usethis::use_data(obj_test, internal = FALSE, overwrite = TRUE)















