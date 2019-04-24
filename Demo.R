library(rgdal)
library(raster)
library(moments)
library(UrbanStars)



####SetBasicParameters####
BeamWidth = 350
BeamLength = 1000
NSegments = 25
NBeams = 8
SlopeWindow = 3
SlopeThreshold = 0.001
Threshold = 0.2



#Acquire Data
download.file(url = 'https://github.com/JohMast/UrbanStars/raw/master/DemoData/GUF_Demo.tif',destfile = "GUF_Demo.tif")
download.file(url = 'https://github.com/JohMast/UrbanStars/raw/master/DemoData/SRTM_Demo.tif',destfile = "SRTM_Demo.tif")
download.file(url = 'https://github.com/JohMast/UrbanStars/raw/master/DemoData/GUF_Demo.tif',destfile = "TS_Sel_Demo.tif")
download.file(url = 'https://github.com/JohMast/UrbanStars/raw/master/DemoData/UrbanCenters.csv',destfile = "UrbanCenters.csv")





####====================Step 1: Load and prepare Data=======================####
####(One of the datasets should contain a urban mask or building density layer)

##1.1: Create a normalized GUFNorm Layer
GUF <- raster("GUF_Demo.tif")
GUFNorm <- GUF / 255
names(GUFNorm) <- "GUFNorm"

##1.2: Select mean NDBI, NDWI, NDVI layers from the timescan stack
TS <- stack("TS_Demo.tif")
names(TS) <- c("NDBI", "NDWI", "NDVI")

##1.3: Create Aspect and Slope from the SRTM DEM
SRTM <- raster("SRTM_Demo.tif")
Aspect <- terrain(SRTM, opt = "aspect")
Slope <- terrain(SRTM, opt = "slope")

####==========Step 2: Get a list of cities and start looping over it========####
##2.1: Read a list of Urban Centers
UrbanCentersList <- read.csv2("UrbanCenters.csv")[, ]
##For the purpose of this Demo, we just work on one city
UrbanCentersList <- UrbanCentersList[1,]
##2.2: Create a Point Shapefile from the List of Urban Centers
coordinates(UrbanCentersList) <- ~ x + y
UrbanCentersList@proj4string <-
  crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
##2.3: Create an empty Dataframe which
##will later collect the data over all cities
CityStats <- data.frame()
##2.4: Create Output directory
dir.create("Out")


##2.4Grab a city
CurrentCity <- UrbanCentersList[1,]
CityName <- CurrentCity$UrbanCenter


####================Step 3: Create a Star  for the City===================####
##3.1: Create an Output directory
dir.create(paste0("Out/", CityName), showWarnings = FALSE)


##3.2: prepare the while loop
#3.2.1: UsUrban indicates if valid city limits have been found
IsUrban <- NA
#3.2.2: Iteration Counter
Iteration <- 0
#3.2.3: Loop until valid limits are found
#~~~~~~~~~~~~~Begin while loop until limits are found~~~~~~~~~~~~~#
while (is.na(IsUrban)) {
  Iteration = Iteration + 1
  print(Iteration)
  ##3.3: Create a Star which length increases with successive iterations
  Star <-
    create_star(
      UrbanCenter = CurrentCity,
      BeamWidth = BeamWidth,
      BeamLength = BeamLength * Iteration,
      NSegments = NSegments * Iteration,
      NBeams = NBeams
    )

  ####==============Step 4: Extract Points within Corridors===============####
  ####and append them to the Star
  ##4.1: Create a List of layers to be extracted
  ##(At this point, doing just the GUF makes sense)
  LayerList <- list(GUFNorm)
  ##4.2: Extract the given layers under the star polygon
  StarRasterValues <- extract_under_star(Star, LayerList, mean)
  ##4.3: Append the column to the stars dataframe
  Star@data <- cbind(Star@data, StarRasterValues)

  ####===================Step 5: Find the Limits for this City============####
  ##5.1: Find the limits for each beam
  ##and append them to the star as an attribute
  IsUrban <-
    find_limit_star(
      Star = Star,
      UrbanLayerName = "GUFNorm",
      Threshold = Threshold,
      SlopeThreshold = SlopeThreshold,
      SlopeWindow = SlopeWindow
    )
}
#~~~~~~~~~~~~~~End while loop if limits are found~~~~~~~~~~~~~#
#Once Limits are found, add them to the Star
Star@data$IsUrban <- IsUrban
windows(width = 10,height = 10)
plot(Star,main=paste("Star over ", CityName))
plot(GUFNorm,add=T)
plot(Star,add=T,col=Star$IsUrban,border=Star$IsUrban)

####===================Step 6: Extract and add further layers=============####
##6.1: Create a list of layers to be extracted
##(The GUF was already extracted in Step 4.1)
LayerList <- list(TS$NDBI, TS$NDWI, TS$NDVI, SRTM, Aspect, Slope)
##6.2: Extract the given layers under the star polygon
StarRasterValues <- extract_under_star(Star, LayerList, mean)
##6.3: Append the columns to the stars dataframe
Star@data <- cbind(Star@data, StarRasterValues)

####============Step 7: Create the crosssections and write outputs========####
##7.1: Create Crossections by combining opposing beams
StarCross <- create_star_crosssections(Star)
##7.2: Write each as its own csv
for (i in 1:length(StarCross)) {
  write.csv2(StarCross[[i]],
             file = paste0("Out/",
                           CityName,
                           "/",
                           CityName,
                           "_Crosssection_",
                           i,
                           ".csv"))
}
##7.3: Write the star as a shapefile
writeOGR(
  Star,
  dsn = paste0("Out/", CityName, "/", CityName, "_Star_", i, ".shp"),
  layer = "Star",
  driver = "ESRI Shapefile"
)

##7.4: Display one of the profiles
windows(width = 10,height = 20)
par(mfrow=c(4,1))
plot(Star,main="Star over Wuerzburg")
plot(GUFNorm,add=T)
plot(Star,add=T,col=Star$IsUrban,border=Star$IsUrban)
plot(StarCross[[4]]$SRTM,xlab="Position",ylab="SRTM",main="SRTM Profile");lines(StarCross[[4]]$SRTM,col="yellow");abline(v=which(diff(StarCross[[4]]$IsUrban)==1));abline(v=which(diff(StarCross[[4]]$IsUrban)==-1))
plot(StarCross[[4]]$NDVI,xlab="Position",ylab="NDVI",main="NDVI Profile");lines(StarCross[[4]]$NDVI,col="green");abline(v=which(diff(StarCross[[4]]$IsUrban)==1));abline(v=which(diff(StarCross[[4]]$IsUrban)==-1))
plot(StarCross[[4]]$NDBI,xlab="Position",ylab="NDBI",main="NDBI Profile");lines(StarCross[[4]]$NDBI,col="red");abline(v=which(diff(StarCross[[4]]$IsUrban)==1));abline(v=which(diff(StarCross[[4]]$IsUrban)==-1))



####==============Step 8: Calculate Statistics for the city===============####
####and add them to a global dataframe####
##8.1. Calculate some statistics describing the shape,
##greenness and roughness of the city
StarStats <-
  calculate_star_statistics(
    StarCrosssections = StarCross,
    SubsetToUrban = T,
    UrbanLayerName = "GUFNorm",
    NDVILayerName = "NDVI",
    SlopeLayerName = "slope",
    AspectLayerName = "aspect"
  )
##8.2: Append the stats to the global CityStats df
#8.2.1: Bind the statistics to the city name
StatsCurrentCity <- cbind(CurrentCity$UrbanCenter, StarStats)
#8.2.2: Bind the reuslting vector to the global df
CityStats <- rbind(CityStats, StatsCurrentCity)



write.table(
  StatsCurrentCity,
  file = "Out/CityTable.csv",
  sep = ",",
  append = TRUE,
  quote = FALSE,
  col.names = FALSE,
  row.names = FALSE
)



#~~~~~~~~~~~~~~~~~~~~End Loop over Cities~~~~~~~~~~~~~~~~~~~~~~~~#

####==============Step 9: Write Output csv =================================####
write.csv2(CityStats, "Out/GlobalCityStats.csv")
