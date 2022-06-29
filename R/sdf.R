


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://www.ronja-tutorials.com/post/035-2d-sdf-combination/
# ToDo:
#  - chamfer union, intersection, subtract
#  - 
# - elongation
# - primitives: quad, tri, octahedron, pyramid, ellipsoid,
#   rounded cylinder, etc
#
# deformations and distortions
#  - displacement, twist, bend, 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a unit shape at the origin
#' 
#' Many SDF functions from \url{https://iquilezles.org/articles/distfunctions/}
#'
#' \describe{
#'  \item{cylinder}{infinitely long unit cylinder at the origin aligned along z axis}
#' }
#'
#' @param colour object colour
#' @param r1 radius from centre of torus
#' @param r2 radius of the ring.    r2 < r1
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_torus <- function(r1, r2, colour = 'hotpink') {
  force(colour)
  function(coords) {
    x1   <- with(coords, sqrt( x^2 + z^2 ) - r1)
    d    <- sqrt(x1*x1 + coords$y^2)
    dist <- d - r2
    
    list(
      colour = rep.int(colour, nrow(coords)),
      dist   = dist
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_sphere <- function(colour = 'hotpink') {
  function(coords) {
    list(
      colour = rep.int(colour, nrow(coords)),
      dist   = with(coords, x^2 + y^2 + z^2 - 1)
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_plane <- function(colour = 'hotpink') {
  function(coords) {
    dist <- coords$y # unit plane with n = c(0, 1, 0)
    
    list(
      colour = rep.int(colour, nrow(coords)),
      dist   = dist
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_box <- function(colour = 'hotpink') {
  function(coords) {
    # vec3 q = abs(p) - b;
    qx <- abs(coords$x) - 1L
    qy <- abs(coords$y) - 1L
    qz <- abs(coords$z) - 1L
    
    # return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
    c1 <- sqrt(
      pmax.int(qx, 0)^2 +
        pmax.int(qy, 0)^2 +
        pmax.int(qz, 0)^2
    )
    
    c2 <- pmin.int(pmax.int(qx, qy, qz), 0)
    
    dist <- c1 + c2
    
    list(
      colour = rep.int(colour, nrow(coords)),
      dist   = dist
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an infinitely long unit cylinder at the origin aligned along z axis
#' 
#' @inheritParams sdf_torus
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_cyl <- function(colour = 'hotpink') {
  function(coords) {
    dist <- sqrt(coords$x^2 + coords$y^2) - 1 # r = 1
    
    list(
      colour = rep.int(colour, nrow(coords)),
      dist   = dist
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate an object 
#' 
#' @param f field function
#' @param x,y,z traslation amount. defaults to 0
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_translate <- function(f, x=0, y=0, z=0) {
  function(coords) {
    coords$x <- coords$x - x
    coords$y <- coords$y - y
    coords$z <- coords$z - z
    f(coords)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Translate an object 
#' 
#' @param f field function
#' @param xscale,yscale,zscale Scale amount. xscale defaults to 1. yscale
#'        and zscale default to the value of xscale
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_scale <- function(f, xscale = 1, yscale = xscale, zscale = xscale) {
  function(coords) {
    coords$x <- coords$x / xscale
    coords$y <- coords$y / yscale
    coords$z <- coords$z / zscale
    f(coords)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Finite/Infinite repetition 
#' 
#' @param f field function
#' @param x,y,z repetition distance along each axis
#' @param lx,ly,lz repeitition times along each axis
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_repeat_infinite <- function(f, x, y = x, z = x) {
  function(coords) {
    coords$x <- round( (coords$x + 0.5 * x) %% x - 0.5 * x )
    coords$y <- round( (coords$y + 0.5 * y) %% y - 0.5 * y )
    coords$z <- round( (coords$z + 0.5 * z) %% z - 0.5 * z )
    f(coords)
  }
}


clamp <- function(x, lo, hi) {
  ifelse(x < lo, 
         lo, 
         ifelse(x > hi, 
                hi,
                x))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sdf_repeat_infinite
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_repeat_finite <- function(f, x, lx, y = x, ly = lx, z = x, lz = lx) {
  function(coords) {
    coords$x <- round( coords$x - x * clamp(round(coords$x/x), -lx, lx) )
    coords$y <- round( coords$y - y * clamp(round(coords$y/y), -ly, ly) )
    coords$z <- round( coords$z - z * clamp(round(coords$z/z), -lz, lz) )
    f(coords)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Rotate a signed distance field around the z axis
#' 
#' @param f field function
#' @param theta angle in radians
#' 
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_rotatez <- function(f, theta) {
  function(coords) {
    tmpx     <- coords$x * cos(-theta) - coords$y * sin(-theta)
    coords$y <- coords$x * sin(-theta) + coords$y * cos(-theta)
    coords$x <- tmpx
    f(coords)
  }
}


#' @rdname sdf_rotatez
#' @export
sdf_rotatey <- function(f, theta) {
  function(coords) {
    tmpx     <- coords$x * cos(-theta) - coords$z * sin(-theta)
    coords$z <- coords$x * sin(-theta) + coords$z * cos(-theta)
    coords$x <- tmpx
    f(coords)
  }
}


#' @rdname sdf_rotatez
#' @export
sdf_rotatex <- function(f, theta) {
  function(coords) {
    tmpy     <- coords$y * cos(-theta) - coords$z * sin(-theta)
    coords$z <- coords$y * sin(-theta) + coords$z * cos(-theta)
    coords$y <- tmpy
    f(coords)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a "thin" version of a given object.
#' 
#' @param f field function
#' @param thickness thickness of shell
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_onion <- function(f, thickness) {
  function(coords) {
    res <- f(coords)
    list(
      colour = res$colour,
      dist   = abs(res$dist) - thickness
    )
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set the colour
#' 
#' @param f field function
#' @param colour colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_set_colour <- function(f, colour) {
  function(coords) {
    res <- f(coords)
    res$colour[] <- colour
    res
  }  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rounded version of a given object.
#' 
#' @param f field function
#' @param r radius of rounding
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_round <- function(f, r) {
  force(r) 
  function(coords) {
    res <- f(coords)
    list(
      colour = res$colour,
      dist   = f(coords) - r
    )
  }
}


which.pmin.int <- function(vs) {
  list(
    min = do.call(pmin.int, vs),
    index = max.col(-do.call(cbind, vs), ties.method = 'last')
  )
}

which.pmax.int <- function(vs) {
  list(
    min = do.call(pmax.int, vs),
    index = max.col(do.call(cbind, vs), ties.method = 'last')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @param ... field functions
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_union <- function(...) {
  function(coords) {
    fs  <- list(...)
    res <- lapply(fs, function(f) f(coords))
    
    dists   <- lapply(res, function(x) x$dist)
    colours <- lapply(res, function(x) x$colours)
    
    pmin_info <- which.pmin.int(dists)
    dist      <- pmin_info$min
    # colour    <- lapply(seq_along(dist), function(i) {
    #   idx <- pmin_info$index[i]
    #   colours[[idx]][i]
    # })
    # colour <- unlist(colour)
    colours <- do.call(cbind, colours)
    idx     <- seq_along(dist) + length(dist) * (pmin_info$index - 1L)
    colour <- colours[idx]
    
    list(
      colour = colour,
      dist   = dist
    )
    
    # Union of just TWO signed distance fields
    # pmin.int(
    #   f1(coords),
    #   f2(coords)
    # )
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Smoothed union of two objects
#' 
#' @param f1,f2 field functions
#' @param k smoothing parameter
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_union_smooth <- function(f1, f2, k) {
  force(k)
  function(coords) {
    res1 <- f1(coords)
    res2 <- f2(coords)
    d1 <- res1$dist
    d2 <- res2$dist
    h  <- clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 )
    dist <- (1 - h) * d2 + h * d1 - k * h * (1 - h)

    list(
      colour = res1$colour,
      dist   = dist
    )
    
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects

#' @inheritParams sdf_union
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_intersect <- function(...) {
  function(coords) {
    # pmax.int(
    #   f1(coords),
    #   f2(coords)
    # )
    
    fs  <- list(...)
    res <- lapply(fs, function(f) f(coords))
    
    dists   <- lapply(res, function(x) x$dist)
    colours <- lapply(res, function(x) x$colour)
    
    pmax_info <- which.pmax.int(dists)
    dist      <- pmax_info$min
    
    colours <- do.call(cbind, colours)
    idx     <- seq_along(dist) + length(dist) * (pmax_info$index - 1L)
    colour <- colours[idx]

    list(
      colour = colour,
      dist   = dist
    )
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Intersection of multiple objects
#' 
#' @param f1,f2 field functions
#' @param k smoothing parameter
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_intersect_smooth <- function(f1, f2, k) {
  force(k)
  function(coords) {
    res1 <- f1(coords)
    res2 <- f2(coords)
    d1 <- res1$dist
    d2 <- res2$dist
    h  <- clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 )
    dist <- (1 - h) * d2 + h * d1 + k * h * (1 - h)
    
    # colour <- mix_colours(res1$colour, res2$colour, h)
    colour <- ifelse(h <= 0, res1$colour,
                     ifelse(h >= 1, res2$colour, 'green'))
    
    # colour <- ifelse(h <= 0, 'black',
                     # ifelse(h >= 1, res2$colour, 'green'))
    # colour <- grey(h)
    
    
    list(
      colour = colour,
      dist   = dist
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subtract on object from another
#' 
#' @param f1,f2 field functions
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_subtract <- function(f1, f2) {
  function(coords) {
    
    res1 <- f1(coords)
    res2 <- f2(coords)
    d1 <- res1$dist
    d2 <- res2$dist
    
    dist   <- pmax.int(d1, -d2)
    colour <- res1$colour
    
    list(
      colour = colour,
      dist   = dist
    )
    
    # pmax.int(
    #   f1(coords),
    #   -f2(coords)
    # )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Subtract on object from another
#' 
#' @param f1,f2 field functions
#' @param k smoothing parameter
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_subtract_smooth <- function(f1, f2, k) {
  force(k)
  function(coords) {
    res1 <- f1(coords)
    res2 <- f2(coords)
    d1 <- res1$dist
    d2 <- res2$dist
    h  <- clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 )
    dist <- (1 - h) * d1 -  h * d2 + k * h * (1 - h)
    
    list(
      colour = res1$colour,
      dist   = dist
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Linearly interpolate between two objects
#'
#' @param f1,f2 field functions
#' @param amount proportion of final field which comes from second field. 
#'        \code{amount} should be a numeric value in the range [0,1]
#' @param colour_opt how to handle colours when interpolating. 1 = use
#'        colour from first object (default). 2 = use colour from second
#'        object.   Any other value will do a naive linear interpolation
#'        between the colours of the objects
#' @export
#'
#' @family sdf_transforms
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdf_interpolate <- function(f1, f2, amount, colour_opt = 1L) {
  function(coords) {
    res1 <- f1(coords)
    res2 <- f2(coords)
    d1 <- res1$dist
    d2 <- res2$dist
    
    dist <- d1 * (1 - amount) + d2 * amount
    
    if ( (is.null(colour_opt) || is.na(colour_opt)) && !colour_opt %in% 1:2 ) {
      colour <- mix_colours(res1$colour, res2$colour, amount)
    } else if (colour_opt == 1) {
      colour <- res1$colour
    } else if (colour_opt == 2) {
      colour <- res2$colour
    }
    
    list(
      colour = res1$colour,
      dist   = dist
    )
    
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' mix colours
#' 
#' @param colour1,colour2 colours to mix
#' @param frac mix fraction
#' @noRd
#'
#' @importFrom grDevices rgb col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mix_colours <- function(colour1, colour2, frac) {
  res1 <- col2rgb(colour1)
  res2 <- col2rgb(colour2)
  res <- (1 - frac) * res1  + frac * res2
  rgb(res[1,], res[2,], res[3,], maxColorValue = 255)
}





if (FALSE) {
  library(grid)
  library(dplyr)
  
  N  <- 30
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
  
  world <- sdf_torus(3, 1) %>% sdf_scale(5)
  
  scene  <- world(coords)
  inside <- scene$dist <= 0
  table(inside)
  
  coords_inside <- coords[inside, ]
  colour_inside <- scene$colour[inside]
  
  cubes  <- isocubesGrob(coords_inside, max_y = 50, xo = 0.5, yo = 0.5, fill = colour_inside)
  grid.newpage(); grid.draw(cubes)
  
}



if (FALSE) {
  library(grid)
  library(dplyr)
  
  N  <- 30
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
  
  world <- sdf_subtract(
    sdf_box() %>% sdf_scale(8),
    sdf_plane(colour = 'white') %>%
      sdf_translate(y = -4) %>%
      sdf_rotatex(3 * pi/4)
  )
  # world <- sdf_intersect(
  #   sdf_box(),
  #   sdf_plane(colour = 'white') 
  # )
  
  sdf_render <- function(scene, coords) {
    scene  <- world(coords)
    inside <- scene$dist <= 0
    table(inside)
    
    coords_inside <- coords[inside, ]
    colour_inside <- scene$colour[inside]
    
    list(
      coords = coords_inside,
      colour = colour_inside
    )
  }
  
  res <- sdf_render(world, coords)
  
  cubes  <- isocubesGrob(res$coords, max_y = 50, xo = 0.5, yo = 0.5, fill = res$colour)
  grid.newpage(); grid.draw(cubes)
  
}





if (FALSE) {
  
  library(grid)
  
  
  sphere <- sdf_sphere() %>%
    sdf_scale(40)

  box <- sdf_box(colour = 'yellow') %>%
    sdf_scale(32)

  cyl <- sdf_cyl() %>%
    sdf_scale(16)

  
  world <- sdf_subtract(
    sdf_intersect(box, sphere),
    sdf_union(
      cyl,
      sdf_rotatey(cyl, pi/2),
      sdf_rotatex(cyl, pi/2)
    )
  )
  world <- sdf_interpolate(box, sphere, amount = 0.8)
  
  
  N  <- 50
  coords <- expand.grid(x=seq(-N, N), y = seq(-N, N), z = seq(-N, N))
  
  
  res <- sdf_render(world, coords)
  cubes  <- isocubesGrob(res$coords, max_y = 100, xo = 0.5, yo = 0.5, fill = res$colour, col = NA)
  grid.newpage(); grid.draw(cubes)
  
  
  # inside <- world(coords) <= 0
  # cubes  <- isocubesGrob(coords[inside,], max_y = 110, xo = 0.5, yo = 0.5)
  # grid.newpage()
  # grid.draw(cubes)
  
  
  x11(type = 'dbcairo', width = 10, height = 10)
  dev.control('inhibit')
  
  thetas <- seq(0, 2*pi, length.out = 180)
  for (i in seq_along(thetas)) {
    theta <- thetas[i]
    cat(".")
    final <- world %>% 
      sdf_rotatey(theta) %>%
      sdf_rotatex(theta + pi/2) %>%
      sdf_rotatez(theta + pi/3)
    inside <- final(coords) <= 0
    cubes  <- isocubesGrob(coords[inside,], max_y = 110, xo = 0.5, yo = 0.5)
    
    # dev.hold()
    # grid.rect(gp = gpar(fill = 'white'))
    # grid.draw(cubes)
    # dev.flush()
    
    png_filename <- sprintf("working/anim/%03i.png", i)
    png(png_filename, width = 800, height = 800)
    grid.draw(cubes)
    dev.off()
    
  }
  
  # ffmpeg -y -framerate 20 -pattern_type glob -i 'anim/*.png' -c:v libx264 -pix_fmt yuv420p -s 800x800 'anim.mp4'
  
}














