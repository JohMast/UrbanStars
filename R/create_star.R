#' Create a Star object around a central point
#'
#' @details This function creates a Star object around a given \code{UrbanCenter}.
#' It consists of \code{NBeams} times \code{NSegments} equally sized polygons of length \code{BeamLength/NSegments} and width \code{BeamWidth},
#' arranged in \code{NBeams} equally spaced beams of length \code{Beamlength}.
#'  The returned Star is a SpatialPolygonsDataFrame and is unprojected in WGS84 (EPSG:4326).
#'  It contains a field "OverallId" which contains unique ids for each segment, the first 6 numbers indicating the beam, the latter 6 numbers indicating the segment.
#'
#'
#' @param UrbanCenter a SpatialPoint object
#' @param BeamWidth width of each beam in meters
#' @param BeamLength length of each beam in meters
#' @param NSegments number of segments of each beam
#' @param NBeams the number of beams of the star
#'
#' @return SpatialPolygonsDataframe
#'
#' @examples
#' create_star_click(UrbanCenter)
#'
#' @seealso
#' \code{\link{create_beam}},
#'\code{\link{create_star_click}}
#'
#' @export
#' @import raster
#' @import sp
#' @author Johannes Mast


create_star <- function(UrbanCenter,BeamWidth=10000,BeamLength=70000,NSegments=10,NBeams=8){
  #Calculate the offset angle between each beam
  OffAngleInc <- 360 / NBeams

  #In a loop, call create_beam NBeams times, each time
  # the angle (starting at 0) is increased by OffAngleInc

  for (i in 1:NBeams) {
    Beam <- create_beam(
      UrbanCenter = UrbanCenter,
      BeamWidth = BeamWidth,
      BeamLength = BeamLength,
      NSegments = NSegments,
      OffAngle = (i - 1) * OffAngleInc
    )

    #To identify the individual segments later, columns in the attribute table are created
    Beam$BeamId <- i
    Beam$SegmentId <- Beam$id
    Beam$OverallId <- paste0(sprintf("%06d",Beam$BeamId),
                             sprintf("%06d",Beam$SegmentId))

    if (i == 1) { # if the first beam, leave as is
      Star <- Beam
    } else{   #if not the first beam, append a new beam to previous beams
      Star <- rbind(Star,Beam)
    } # end else
  }
  return(Star)

}
