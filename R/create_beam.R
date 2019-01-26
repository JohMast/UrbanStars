#' Create a beam starting from a central point
#'
#' @details This function creates a beam object starting from a given \code{UrbanCenter}.
#' The returned Beam is a SpatialPolygonsDataFrame and is unprojected in WGS84 (EPSG:4326).
#' It consists of \code{NSegments} equally sized polygons of length \code{BeamLength/NSegments} and width \code{BeamWidth}. The \code{UrbanCenter} lies on the midpoint of the innermost edge of the innermost segment. From there, the beam extends for \code{BeamLength} meters at a true-north-azimuth of \code{OffAngle}, the edges of the beam forming a geodesic between its corners.

#'
#' @param UrbanCenter a SpatialPoint object
#' @param BeamWidth width of the beam in meters
#' @param BeamLength length of the beam in meters
#' @param NSegments number of segments of the beam
#' @param OffAngle azimuth of the beam
#'
#' @return SpatialPolygonsDataframe
#'
#' @examples
#' create_beam(UrbanCenter)
#'
#'@seealso
#'\code{\link{create_beam}},
#'\code{\link{create_star_click}},
#'\code{\link[geosphere]{dest_point}}
#'
#' @importFrom geosphere destPoint
#' @importFrom geosphere gcIntermediate
#' @importFrom gdata interleave
#' @import raster
#' @import sp
#' @export
#' @author Johannes Mast

#library(geosphere)
create_beam <- function(UrbanCenter, BeamWidth = 10000, BeamLength =70000 ,NSegments = 10 ,OffAngle= 0) {


  #To calculate the beam accurately, the coordinates are transformed to WGS84 longlat
  UrbanCenter_reprojected <- spTransform(UrbanCenter,CRSobj = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

  #Calculate the Length of a single segment
  SegmentLength <- BeamLength/NSegments

  #Calculate the starting points P1A and P1B
  P1A <- destPoint(UrbanCenter_reprojected,b = OffAngle-90,d=BeamWidth/2)
  P1B <- destPoint(UrbanCenter_reprojected,b = OffAngle+90,d=BeamWidth/2)

  #calculate the ending points PNA and PNB
  PNA <- destPoint(P1A,b = OffAngle,d=BeamLength)
  PNB <- destPoint(P1B,b = OffAngle,d=BeamLength)

  #Create Intermediate Points between them
  InterPointsA <- gcIntermediate(P1A,PNA,addStartEnd = T,n = NSegments-1)
  InterPointsB <- gcIntermediate(P1B,PNB,addStartEnd = T,n = NSegments-1)

  #Create a dataframe of coordinates, grouped by Id
  Id=rep(1:NSegments, each = 4)

  #Use the InterPoints to create a dataframe of coordinates that then gets cbound to the ID
  APoints <- InterPointsA[-(NSegments+1),] ##All A points except the last
  BPoints <- InterPointsA[-1,]             ##All A points except the first
  DPoints <- InterPointsB[-(NSegments+1),] ##All B points except the last
  CPoints <- InterPointsB[-1,]            ##All B points except the first

  #Create a List of coordinates, each list object containing the points for one polygon
  PolyCoords <- (gdata::interleave(APoints,BPoints,CPoints,DPoints))
  PolyCoordsList <- split(PolyCoords, Id)

  #Creat Spatial Polygons Dataframe from the list object
  ID=1:NSegments
  BeamSpatialPolygons <- SpatialPolygons(mapply(
    function(poly, id) {
      xy <- matrix(poly, ncol=2, byrow=F)
      Polygons(list(Polygon(xy)), ID=id)
    },#end function
    PolyCoordsList, ID
  )#End mapply
  )
  BeamPolygonsDataFrame <- SpatialPolygonsDataFrame(BeamSpatialPolygons, data.frame(id=ID, row.names=ID))
  BeamPolygonsDataFrame@proj4string <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  return(BeamPolygonsDataFrame)



}
