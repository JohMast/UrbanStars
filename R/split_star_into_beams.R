#' Split the dataframe of a Star object into its constituent beams
#'
#'@details  Splits the dataframe of a \code{Star} into its constituent beams (identified by their "BeamId"). Each resulting beam dataframe is as long as the number of segments of the beam.
#'
#' @param Star a Star object that contains columns "BeamId" and "SegmentId", as created by create_star()
#'
#' @return List of dataframes
#'
#' @examples
#' split_star_into_beams(Star)
#' @seealso
#' \code{\link{create_star}}
#'
#'
#' @export
#' @import sp
#' @author Johannes Mast

split_star_into_beams <- function(Star) {
  StarDataByBeams <- split(Star@data,
                           f = rep(1:length(unique(Star$BeamId)), #Split into the number of beams
                                   each = length(unique(Star$SegmentId)))) # Each beam as long as the number of segments
  return(StarDataByBeams)
}
