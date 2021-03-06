% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_star_click.R
\name{create_star_click}
\alias{create_star_click}
\title{Create a Star by clicking on a base raster}
\usage{
create_star_click(BaseRaster, BeamWidth = 1000, BeamLength = 50000,
  NSegments = 50, NBeams = 8)
}
\arguments{
\item{BaseRaster}{a RasterLayer to be clicked on}

\item{BeamWidth}{width of each beam in meters}

\item{BeamLength}{length of each beam in meters}

\item{NSegments}{number of segments of each beam}

\item{NBeams}{the number of beams of the star}
}
\value{
SpatialPolygonsDataframe
}
\description{
Create a Star by clicking on a base raster
}
\details{
This function plots a \code{BaseRaster} and, on \code{click()}, creates a Star object around the clicked point.
It consists of \code{NBeams} times \code{NSegments} equally sized polygons of length \code{BeamLength/NSegments} and width \code{BeamWidth},
arranged in \code{NBeams} equally spaced beams of length \code{Beamlength}.
 The returned Star is a SpatialPolygonsDataFrame and takes the CRS of the \code{BaseRaster}.
 It contains a field "OverallId" which contains unique IDs for each segment, the first 6 numbers indicating the beam, the latter 6 numbers indicating the segment.
}
\examples{
create_star_click()

}
\seealso{
\code{\link{create_beam}},
\code{\link{create_star}}
}
\author{
Johannes Mast
}
