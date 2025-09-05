

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Voxel coordinates for an organic shape
#' 
#' @examples
#' head(obj_organic)
#' cubes <- isocubesGrob(obj_organic, size = 2) |>
#'   grid::grid.draw()
#' @family datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"obj_organic"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Voxel coordinates for the letter R
#' 
#' @examples
#' head(obj_letter)
#' isocubesGrob(obj_letter, size = 5, y = 0.05) |>
#'   grid::grid.draw()
#' @family datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"obj_letter"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Voxel coordinates for a test object useful for debugging orientation
#' and visibility checks
#' 
#' @examples
#' head(obj_test)
#' isocubesGrob(obj_test, size = 5, y = 0.05) |>
#'   grid::grid.draw()
#' @family datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"obj_test"
