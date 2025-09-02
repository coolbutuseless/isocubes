


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a grob representing the axis orientation.
#' 
#' The x, y and z axes are drawn in red, green and blue respectively.
#' 
#' @inheritParams isocubesGrob
#' @param size length of each axis  in \code{default.units}
#' 
#' @return grid \code{grob} object
#' @import grid
#' @examples
#' isoaxesGrob() |> 
#'   grid::grid.draw()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isoaxesGrob <- function(size          = 5,
                        x             = NULL, 
                        y             = NULL,
                        default.units = 'mm',
                        xyplane       = 'flat',
                        handedness    = 'right',
                        verbosity     = 0, ...) {
  
  
  
  x <- x %||% grid::unit(0.5, 'npc')
  y <- y %||% grid::unit(0.5, 'npc')
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote units if not given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!grid::is.unit(size)) {
    size <- grid::unit(size, units = default.units)
  }  
  if (!grid::is.unit(x)) {
    x <- grid::unit(x, units = 'npc')
  }  
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, units = 'npc')
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # set graphics parameters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp <- do.call(grid::gpar, list(...))
  gp$lwd <- 3
  gpx <- gp; gpx$col <- 'red'
  gpy <- gp; gpy$col <- 'green'
  gpz <- gp; gpz$col <- 'blue'
  
  y <- y + size/2
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Orient the axes
  # Default: xy-plane is the *right-hand* face of the cube
  #          z-axis goes into the plane
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (xyplane == 'right' && handedness == "left") {
    xaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpx)
    yaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x                   , y1 = y + size            , gp = gpy)
    zaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x - size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpz)
  } else if (xyplane == 'right' && handedness == "right") {
    xaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpx)
    yaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x                   , y1 = y + size            , gp = gpy)
    zaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y - size * sin(pi/6), gp = gpz)
  } else if (xyplane == 'left' && handedness == "left") {
    xaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y - size * sin(pi/6), gp = gpx)
    yaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x                   , y1 = y + size            , gp = gpy)
    zaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpz)
  } else if (xyplane == 'left' && handedness == "right") {
    xaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y - size * sin(pi/6), gp = gpx)
    yaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x                   , y1 = y + size            , gp = gpy)
    zaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x - size * cos(pi/6), y1 = y - size * sin(pi/6), gp = gpz)
  } else if (xyplane %in% c('top', 'flat') && handedness == 'left') {
    xaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpx)
    yaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x - size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpy)
    zaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x                   , y1 = y - size            , gp = gpz)
  } else if (xyplane %in% c('top', 'flat') && handedness == 'right') {
    xaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x + size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpx)
    yaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x - size * cos(pi/6), y1 = y + size * sin(pi/6), gp = gpy)
    zaxis <- grid::segmentsGrob(x0 = x, y0 = y, x1 = x                   , y1 = y + size            , gp = gpz)
  } else {
    stop("Not a supported coordinate system: xyplane: ", xyplane, "  hand: ", handedness)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble axis
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grobTree(
    xaxis,
    yaxis,
    zaxis,
    grid::pointsGrob(x = x, y  = y, pch = '.')
  )
}




if (FALSE) {
  
  xyplane = 'right'; handedness = 'left'
  xyplane = 'right'; handedness = 'right'
  xyplane = 'left' ; handedness = 'left' 
  xyplane = 'left' ; handedness = 'right'
  xyplane = 'top'  ; handedness = 'left' 
  xyplane = 'top'  ; handedness = 'right'
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- data.frame(x = 0, y = 0, z = 0)
  cubes  <- isocubesGrob(coords, xyplane = xyplane, handedness = handedness, fill = "#88888888")
  grid.newpage(); 
  grid::grid.draw(isoaxesGrob(size = 10, xyplane = xyplane, handedness = handedness ))
  grid.draw(cubes)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  N <- 4
  x <- data.frame(x = seq(1, N), y = 0, z = 0)
  y <- data.frame(x = 0, y = seq(1, N), z = 0)
  z <- data.frame(x = 0, y = 0, z = seq(1, N))
  o <- data.frame(x = 0, y = 0, z = 0)
  
  coords <- do.call(rbind, list(o, x, y, z))
  fill <- rep(c('red', 'green', 'blue'), each = (N))
  fill <- c('grey50', fill)
  coords$fill <- fill
  
  cubes  <- isocubesGrob(coords, xyplane = xyplane, handedness = handedness)
  grid.newpage(); 
  grid.draw(cubes)
  grid::grid.draw(isoaxesGrob(size = 10, xyplane = xyplane, handedness = handedness ))
  
}


