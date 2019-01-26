
#' Extract raster layers under Star object
#'
#' @details This Function extracts the values of a \code{LayerList} of raster layers under a \code{Star}
#'  and returns a data frame in which the rows correspond to the Stars segments and the columns to the extracted raster layers.
#'  Optionally applies a function \code{funs} during the extraction.
#'
#' @param Star a star object
#' @param LayerList a list object of raster layers
#' @param funs a function which is passed to raster::extract()
#'
#' @return Dataframe
#'
#'
#' @examples
#' find_limit_star(Star,UrbanLayerName,Threshold=50,SlopeThreshold=0.01,SlopeWindow=3)
#'
#' @import raster
#' @seealso
#' \code{\link[raster]{extract}}
#'
#' @export
#' @author Johannes Mast

extract_under_star <- function(Star, LayerList, funs=mean){


  #extract each layer under the star elements
  StarStatsLayer <- lapply(LayerList, function(x){
    extract(x, Star, funs)}
    )
  # get the names of the layers
  LayerNames <- unlist(lapply(LayerList, function(x)
    names(x)
  ))
  # unlist into a dataframe
  Df <- data.frame((sapply(StarStatsLayer,c)))
  # add the respective names to the columns
  names(Df) <- LayerNames
  # return the df
  return(Df)
}

