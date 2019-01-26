#' Create a Star by clicking on a base raster
#'
#'
#' @details This function plots a \code{BaseRaster} and, on \code{click()}, creates a Star object around the clicked point.
#' It consists of \code{NBeams} times \code{NSegments} equally sized polygons of length \code{BeamLength/NSegments} and width \code{BeamWidth},
#' arranged in \code{NBeams} equally spaced beams of length \code{Beamlength}.
#'  The returned Star is a SpatialPolygonsDataFrame and takes the CRS of the \code{BaseRaster}.
#'  It contains a field "OverallId" which contains unique IDs for each segment, the first 6 numbers indicating the beam, the latter 6 numbers indicating the segment.
#'
#'
#' @param BaseRaster a RasterLayer to be clicked on
#' @param BeamWidth width of each beam in meters
#' @param BeamLength length of each beam in meters
#' @param NSegments number of segments of each beam
#' @param NBeams the number of beams of the star
#'
#' @return SpatialPolygonsDataframe
#'
#' @examples
#' create_star_click()
#'
#' @seealso
#'\code{\link{create_beam}},
#'\code{\link{create_star}}
#' @import raster
#' @import sp
#' @export
#' @author Johannes Mast




##################################
# This function calls the function create_star
# with the added convenience of just requiring a click on a map

#It takes as inputs a raster which serves as a basemap
#Plus additional optional arguments (see create_star() for more detail)
#
#It returns the created star and plots it on the raster
# The created star takes the coordinate system of the base raster
####################################

create_star_click <- function(BaseRaster,BeamWidth=1000,BeamLength=50000,NSegments=50,NBeams=8){
  #Plot the Base Raster to be selected from
  plot(BaseRaster[[1]])

  centre <- click()
  centre <- as.data.frame(centre)
  coordinates(centre) <- ~x+y
  centre@proj4string <- BaseRaster@crs

  Star <- create_star(UrbanCenter = centre,BeamWidth=BeamWidth,BeamLength = BeamLength,NSegments = NSegments,NBeams = NBeams)
  Star <- spTransform(Star,crs(BaseRaster@crs))
  plot(Star,add=T)
  return(Star)
}
