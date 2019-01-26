#' Find the limits of a Stars density distributions
#'
#'
#' @details
#' Splits the dataframe of a \code{Star} into its constituent beams (identified by their "BeamId").
#' Each beams \code{UrbanLayerName} column is then evaluated to find a plausible segmentation point, which can be considered the urban limit.
#' The criterium for the point being: First point at which
#' \enumerate{
#' \item the value is lower than \code{"Threshold"} and
#' \item the mean slope over the next \code{SlopeWindow} data points is lower than \code{SlopeThreshold}
#' }
#' The limits are then used to create a logical vector of \code{length(Star)}, indicating for each segment of the Star whether it is urban.
#' If for at least one of the beams, no point satisfies both conditions, NA is returned.
#'
#'
#' @param Star a Star object that contains columns "BeamId" and "SegmentId", as created by create_star()
#' @param UrbanLayerName the name of the column which holds the density values
#' @param Threshold a threshold under which the density is considered nonurban
#' @param SlopeWindow a forward looking search window in which the mean slope is evaluated
#' @param SlopeThreshold the threshold under which the in SlopeWindow calculated mean of the slope is considered to be small
#'
#' @return Logical vector
#'
#' @examples
#' find_limit_star(Star,UrbanLayerName,Threshold=50,SlopeThreshold=0.01,SlopeWindow=3)
#'
#'
#' @seealso
#' \code{\link{find_limit}},
#' \code{\link{split_star_into_beams}}
#'
#' @export
#' @import sp
#' @author Johannes Mast



#This function provides the added convenience of directly finding the limits of a star object
# By splitting it first and then applying the find_limit function on its components
find_limit_star <- function(Star,UrbanLayerName,Threshold=50,SlopeThreshold=0.01,SlopeWindow=3){
  StarDataByBeams <- split_star_into_beams(Star)
  Limits <- unlist(lapply(StarDataByBeams,function(x) (find_limit(x[[UrbanLayerName]],Threshold = Threshold,SlopeThreshold = SlopeThreshold,SlopeWindow = SlopeWindow))))
  if(anyNA(Limits)){ ##If For one of the beams no limit was found, return NA as an error indicator

    return(NA)
  }else{ ##Else continue as usual
    # Create a logical vector for the star elements that determines if they are urban or not (Error prone for unknown reasons)
    IsUrban <- apply(Star@data,MARGIN = 1, FUN = function(x){x["SegmentId"]<Limits[as.integer(x["BeamId"])]})

    # Create a logical vector for the star elements that determines if they are urban or not
    # Should do the same thing as the apply function above, but is not bugged
    IsUrban <- vector(length=length(Star))

    for (i in 1:length(Star)){
      IsUrban[i] <- Star$SegmentId[i]<=Limits[Star$BeamId[i]]
    }
    return(IsUrban)
  }
}
