
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @param ... field functions
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_union <- function(...) {
  fs  <- list(...)
  function(coords) {
    dists <- lapply(fs, function(f) f(coords))
    dist  <- do.call(pmin.int, dists)
    
    dist
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Smoothed union of two objects
#' 
#' @param f1,f2 field functions
#' @param k smoothing or interpolationg parameter in range [0, 1]. Default: 0.5
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_union_smooth <- function(f1, f2, k = 0.5) {
  function(coords) {
    d1   <- f1(coords)
    d2   <- f2(coords)
    h    <- clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 )
    dist <- (1 - h) * d2 + h * d1 - k * h * (1 - h)
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @inheritParams sdf_union
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_intersect <- function(...) {
  fs <- list(...)
  function(coords) {
    dists <- lapply(fs, function(f) f(coords))
    dist  <- do.call(pmax.int, dists)
    
    dist
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @inheritParams sdf_union_smooth
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_intersect_smooth <- function(f1, f2, k = 0.5) {
  force(k)
  function(coords) {
    d1   <- f1(coords)
    d2   <- f2(coords)
    h    <- clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 )
    dist <- (1 - h) * d2 + h * d1 + k * h * (1 - h)
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subtract on object from another
#' 
#' @inheritParams sdf_union_smooth
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_subtract <- function(f1, f2) {
  function(coords) {
    d1   <- f1(coords)
    d2   <- f2(coords)
    dist <- pmax.int(d1, -d2)
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subtract on object from another
#' 
#' @inheritParams sdf_union_smooth
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_subtract_smooth <- function(f1, f2, k = 0.25) {
  function(coords) {
    d1   <- f1(coords)
    d2   <- f2(coords)
    h    <- clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 )
    dist <- (1 - h) * d1 -  h * d2 + k * h * (1 - h)
    
    dist
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Linearly interpolate between two objects
#'
#' @inheritParams sdf_union_smooth
#' 
#' @return SDF field function
#' @examples
#' sdf_sphere() |>
#'    sdf_translate(3, 4, 5)
#' @export
#' @family SDF Constructive Solid Geometry Operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_interpolate <- function(f1, f2, k = 0.5) {
  function(coords) {
    d1   <- f1(coords)
    d2   <- f2(coords)
    dist <- d1 * (1 - k) + d2 * k
    
    dist
  }
}
