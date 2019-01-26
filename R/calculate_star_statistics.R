#' Calculates the statistics for the crosssections of a Star
#'
#' @details The function calculates descriptive statistics for the distribution of the raster values extracted under a star.
#'
#' If \code{SubsetToUrban = T} the statistics will be calculated only for the urban segments if the input \code{StarCrosssections} includes a logical column "IsUrban", as created by \code{find_limit_star}.
#'
#' An \code{UrbanLayerName} is required and returns statistics for the building density distribution.
#' Optionally, passing \code{NDVILayerName}, \code{SlopeLayerName} and \code{AspectLayerName} will return additional statistics.
#' All Statistics are calculated for each of the crosssections individually and then averaged over all crosssections, weighted by their length. The exception is MeanUrbanCentralOffset, for which all crosssections are equally weighted.
#'
#'
#'   Calculated statistics include:
#'  \enumerate{
#'   \item MeanUrbanMean, the weighted mean of the means of the building density
#'   \item MeanUrbanSkew, the weighted mean of the skews of the building density
#'   \item MeanUrbanKurtosis, the weighted mean of the kurtosis of the building density
#'   \item MeanUrbanVariance, the weighted mean of the variances of the building density
#'   \item MeanUrbanCentralOff, the mean number of segments by which the maximum of the curves differ from the centers of the building density distributions.
#'   \item MeanGreenMean, the weighted mean of the means of the NDVI distributions
#'   \item MeanUrbanGreenness, the weighted mean of the scalar products of the building density and the NDVI distributions.
#'   \item MeanUrbanGreennessCor, the weighted mean of the correlations of the building density and the NDVI distributions
#'   \item MeanUrbanSlope, the weighted mean of the scalar products of the building density and the slope distributions.
#'   \item MeanSlopeMean, the weighted mean of the means of the slope
#'   \item MeanUrbanAspect, the weighted mean of the means of the aspect
#' }
#'
#' @param StarCrosssections a list of crosssections, as created by \code{create_star_crosssections}
#' @param SubsetToUrban a switch indicating if the statistics should only be calculated for urban segments. Requires \code{UrbanLayerName} to be defined.
#' @param UrbanLayerName name of the column which holds the urban density values
#' @param NDVILayerName name of the column which holds the NDVI values
#' @param SlopeLayerName name of the column which holds the slope values
#' @param AspectLayerName name of the column which holds the aspect values
#'
#' @return Dataframe
#'
#' @examples
#'   calculate_star_statistics(
#'   StarCrosssections = StarCross,
#'   SubsetToUrban = F,
#'   UrbanLayerName = "GUFNorm",
#'   NDVILayerName = "NDVI",
#'   SlopeLayerName = "slope",
#'   AspectLayerName = "aspect")
#'
#' @seealso
#' \code{\link{create_star_crosssections}},
#' \code{\link{find_limit}}
#'
#' @importFrom stats weighted.mean
#' @importFrom moments skewness
#' @importFrom moments kurtosis
#' @export
#' @author Johannes Mast

##Calculates the statistics for the crosssections of this star:

# CropToUrban requires one column in the DF to be a logical vector called IsUrban


calculate_star_statistics <-
  function(StarCrosssections,
           SubsetToUrban = F,
           UrbanLayerName,
           NDVILayerName,
           SlopeLayerName,
           AspectLayerName) {
    if (missing(UrbanLayerName)) {
      print("UrbanLayerName required")
    }

    ##Optional: Subset the Crosssections to those Segments that are identified as urban
    if (SubsetToUrban) {
      if (is.null(StarCrosssections[[1]]$IsUrban)) {
        print("IsUrban column required for subsetting")
      }

      #
      StarCrosssections <- lapply(StarCrosssections, function(x) {
        x[x[["IsUrban"]],]
      })
    }

    #Calculate the weights for the crosssections (equal to the length of the crosssection, all equal if SubsetToUrban=F)
    WeightsCrosssections <- unlist(lapply(StarCrosssections, function(x) {
      nrow(x)
    }))


    #============Calculate the Statistics================#
    ##These Statistics will be calculated, if the necessary columns are specified:
    MeanUrbanMean <- NA
    MeanUrbanSkew <- NA
    MeanUrbanKurtosis <- NA
    MeanUrbanVariance <- NA
    MeanUrbanCentralOffset <- NA
    MeanGreenMean <- NA
    MeanUrbanGreenness <- NA
    MeanUrbanGreennessCor <- NA
    MeanUrbanSlope <- NA
    MeanSlopeMean <- NA
    MeanUrbanAspect <- NA


    ####Urban Density Statistics
    ##SImple Mean over the mean of the Building Density of all the Crosssections
    MeanUrbanMean = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
      colMeans(x[UrbanLayerName])
    })),WeightsCrosssections)


    weighted.mean(unlist(lapply(StarCrosssections, function(x) {
      colMeans(x[UrbanLayerName])
    })),WeightsCrosssections)

    ##Simple Mean over the Skewness of the Building Density of all the Crossections
    MeanUrbanSkew = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
      skewness(x[UrbanLayerName])
    })),WeightsCrosssections)
    ##Simple Mean over the Kurtosis of the Building Density of all the Crossections
    MeanUrbanKurtosis = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
      kurtosis(x[UrbanLayerName])
    })),WeightsCrosssections)

    ##Simple Mean over the Variance of the Building Density ofall the Crossections
    MeanUrbanVariance = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
      var(x[UrbanLayerName])
    })),WeightsCrosssections)

    ## Simple Mean of the CenterOffset
    ## Which is the distance between the center of the star
    ## and the maximum of the GUF (measured in segments)
    ## !The weights for all Crosssections are equal, even if SubsetToUrban=T!
    MeanUrbanCentralOffset = mean(unlist(lapply(StarCrosssections, function(x) {
      min(x["SegmentId"][x[UrbanLayerName] == max(x[UrbanLayerName])]) - 1
    })))


    ##If a NDVI Layer Name was given:
    if (!missing(NDVILayerName)) {
      ####Urban Greenness Statistics####
      ## Simple Mean of the GreenMean
      ## Which is simply the mean NDVI
      MeanGreenMean = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
        colMeans(x[NDVILayerName])
      })),WeightsCrosssections)

      ## Simple Mean of the Urban Greenness
      ## Which is the Mean of
      ## The NDVI of each segment multiplied by its GUF
      ## As a simple indicator of the presence of green space in urban areas
      MeanUrbanGreenness = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
        colMeans(x[UrbanLayerName] * x[NDVILayerName])
      })),WeightsCrosssections)

      ## Simple Mean of the UrbanGreennessCorrelation
      ## Which is the Correlation of the GuF and the NDVI
      ## As a simple indicator of the presence of green space in urban areas
      MeanUrbanGreennessCor = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
        cor(x[UrbanLayerName], x[NDVILayerName])
      })),WeightsCrosssections)

    }



    ##If a slope Layer Name was given
    if (!missing(SlopeLayerName)) {
      ####Urban Relief Statistics####
      ## Simple Mean of the Urban Slope
      ## Which is the Mean of
      ## The Slope of each segment multiplied by its GUF
      ## As a simple indicator of the relief energy in urban areas
      MeanUrbanSlope = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
        colMeans(x[UrbanLayerName] * x[SlopeLayerName])
      })),WeightsCrosssections)

      ## SImple Mean of the SlopeMean
      ## Which is simply the mean Slope
      MeanSlopeMean = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
        colMeans(x[SlopeLayerName])
      })),WeightsCrosssections)
    }


    ## If an Aspect Layer Name was given
    if (!missing(AspectLayerName)) {
      ## Simple Mean of the Urban Aspect
      ## Which is the Mean of
      ## The Slope of each segment multiplied by its GUF
      ## As a simple indicator of the preferred aspect of urban areas
      MeanUrbanAspect = weighted.mean(unlist(lapply(StarCrosssections, function(x) {
        colMeans(x[UrbanLayerName] * x[AspectLayerName])
      })),WeightsCrosssections)
    }
    #============Return the Statistics================#
    #return the results as a dataframe
    return(data.frame(
      MeanUrbanMean,
      MeanUrbanSkew,
      MeanUrbanKurtosis,
      MeanUrbanVariance,
      MeanUrbanCentralOffset,
      MeanGreenMean,
      MeanUrbanGreenness,
      MeanUrbanGreennessCor,
      MeanUrbanSlope,
      MeanSlopeMean,
      MeanUrbanAspect))
  }


