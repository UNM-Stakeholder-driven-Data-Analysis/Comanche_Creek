#### Read Me ####

#### Libraries ####

# install.packages('devtools')
# devtools::install_github("dkahle/ggmap")
# install.packages('maps')
install.packages('rgdal')
install.packages('rgeos')
install.packages('raster')

library(rgdal)
library(rgeos)
library(raster)
library(ggmap)

#### Load Data ####

#### Mapping ####

#creating basemap of Valle Vidal unit on Carson Nat'l Forest
ValleVidalMap <- get_stamenmap(bbox = c(left = -105.3393,
                                bottom = 36.7202,
                                right = -105.2284,
                                top = 36.8401),
                       maptype = "terrain", 
                       crop = FALSE,
                       zoom = 12)
ggmap(ValleVidalMap)

#Loading shapefiles of Grassy Creek and No Name Creek watershed boundaries
GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                               "Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                               "Watersheds_no name creek")
#View metadata to get info about object class, coordinate reference system, and spatial extent
GrassyCreekWatershed
NoNameCreekWatershed
#these shapefiles are polygons using the transverse mercator  map projection 
#we need them to be in UTM so they have the same crs as the Landsat 8 data

#Add Landsat data; we need red and infrared bands for NDVI

red <- raster("data/raw/Landsat8_082013/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")
near.infrared <- raster("data/raw/Landsat8_082013/LC08_L1TP_033034_20130809_20200912_02_T1_B5.TIF")

#reproject the watershed shapefiles using the crs for the Landsat data, in this case we using the 'red' band which uses the UTM projection

GrassyUTM <- spTransform(GrassyCreekWatershed,
                              crs(red))
crs(GrassyUTM)
#now the Grassy Creek Watershed shapfile is in UTM instead of tmerc

NoNameUTM <- spTransform(NoNameCreekWatershed,
                         crs(red))
crs(NoNameUTM)


# #plot shapefiles
# plot(ValleVidalMap)
# plot(NoNameCreekWatershed, col = "green", add = TRUE)
# plot(GrassyCreekWatershed, col = "blue", add = TRUE)
#   #I can't get both shapefiles to plot on the same map, so I'm   going to move on for now and come back to this later


#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot Grassy Creek shapefile
plot(GrassyUTM,
     main = "Grassy Creek Watershed",
     axes = TRUE,
     border = "blue")

# crop the lidar raster using the vector extent
red_crop_grassy <- crop(red, GrassyUTM)

plot(red_crop_grassy)

# add shapefile on top of the existing raster
plot(GrassyUTM, add = TRUE)


