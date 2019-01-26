#' Find the limit of a unilateral density distribution
#'
#' @details
#' Take a \code{BeamSeries} of density values of a beam and finds a plausible segmentation point, which can be considered the urban limit.
#' The criterium for the point being: First point at which
#' \enumerate{
#' \item the value is lower than \code{"Threshold"} and
#' \item the mean slope over the next \code{SlopeWindow} data points is lower than \code{SlopeThreshold}
#' }
#' If no point satisfies both conditions, NA is returned.
#'
#' @param BeamSeries a unilateral density distribution
#' @param Threshold a threshold under which the density is considered nonurban
#' @param SlopeWindow a forward looking search window in which the mean slope is evaluated
#' @param SlopeThreshold the threshold under which the in SlopeWindow calculated mean of the slope is considered to be small
#'
#' @return Integer
#'
#' @seealso
#' \code{\link{find_limit_star}}
#'
#'
#' @importFrom zoo rollmean
#' @export
#' @author Johannes Mast

find_limit <- function(BeamSeries,Threshold=0.2,SlopeThreshold=0.0001,SlopeWindow=3){
  BeamSlope <- diff(BeamSeries) #Caculate the slope
  AvgBeamSlope <- zoo::rollmean(BeamSlope,SlopeWindow) #Calculate the moving average over SlopeWindow points
  AvgBeamSlope <- c(AvgBeamSlope,rep(0,SlopeWindow)) # Zero padding to ensure the vector length adds up
  Limit<-min(which(BeamSeries<=Threshold & AvgBeamSlope<=SlopeThreshold)) # FInd the limit as decribed above
  if(!is.finite(Limit)){
    Limit <- NA
    warning("Double Criterium Failed, No Limit found")
  }
  return(Limit)
}






