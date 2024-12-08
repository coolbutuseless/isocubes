% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isogrid.R
\name{isolinesGrob}
\alias{isolinesGrob}
\title{Isometrix grid of lines}
\usage{
isolinesGrob(
  N = 50,
  size = 5,
  x = NULL,
  y = NULL,
  col = "black",
  default.units = "npc",
  default.units.cube = "mm",
  verbosity = 0,
  ...
)
}
\arguments{
\item{N}{extents}

\item{size}{dimensions of cube i.e. the length of the vertical edge of the cube.
Default: 5mm}

\item{x, y}{the origin of the isometric coordinate system in 'snpc' coordinates.
These values should be given as vanilla floating point values.
Be default the origin is the middle bottom of the graphics device 
i.e. \code{(x, y) = (0.5, 0)}}

\item{col}{Stroke colour for outline of cube faces. Default: black. If \code{NA}
then no outlines will be drawn.  If negative, then outline colour
will be the same as the face colour.}

\item{default.units}{Default unit for (x,y) position is 'npc'}

\item{default.units.cube}{Default unit for size of a cube is 'mm'}

\item{verbosity}{Verbosity level. Default: 0}

\item{...}{other values passed to \code{gpar()} to set the graphical
parameters e.g. \code{lwd} and \code{col} for the linewidth and colour
of the outline stroke for each cube face.}
}
\value{
isometric line grid
}
\description{
Isometrix grid of lines
}
\examples{
grid <- isolinesGrob()
grid::grid.draw(grid)
}
