#' Create crosssections from a Star object
#'
#'
#'
#' @param Star a Star Object that contains columns "BeamId" and "SegmentId", as created by \code{create_star()}
#'
#' @return List of dataframes
#'
#' @examples
#' create_star_crosssections(Star)
#'
#' @details  Splits the dataframe ofa \code{Star} into its constituent beams (identified by their "BeamId"). Binds the data of each beam to the reverse of its opposite beam. This creates crosssections with the original geographic center of the \code{Star} in the center of the crosssections.
#'
#'
#'   The number of beams must be an even number.
#' @seealso
#' \code{\link{create_star}},
#' \code{\link{split_star_into_beams}}
#' @export
#' @import sp
#' @author Johannes Mast




##Helpful function for reversing a dataframe
#myfun3<-function(x){x[dim(x)[1]:1,]} ##Credit to user 5th @ stackoverflow

create_star_crosssections <- function(Star){

  # Calculate the number of crosssections
  NCrossSections <- length(unique(Star$BeamId))/2
  ##Assert that the number of beams is even
  stopifnot(length(unique(Star$BeamId)) %%2==0)

  ## Split the star into its component beams
  StarDataByBeams <- split_star_into_beams(Star)
  #Create a reversed version of the second half of the beams
  SecondHalfOfBeamsReversed <- lapply(StarDataByBeams[(NCrossSections+1):length(StarDataByBeams)],function(x){x[dim(x)[1]:1,]})
  #Create an nonreversed version of the first half of the beams
  FirstHalfOfBeams <- StarDataByBeams[1:NCrossSections]
  #Bind the counterparts to create NCrossSections Crosssections
  Map(rbind,SecondHalfOfBeamsReversed,FirstHalfOfBeams)

}

