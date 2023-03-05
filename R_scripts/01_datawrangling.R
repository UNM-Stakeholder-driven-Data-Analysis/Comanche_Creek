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

#### Basemap ####

#creating map of Valle Vidal unit on Carson Nat'l Forest
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
  #these shapefiles are polygons using the transverse mercator   map projection 
#checking out the attributes of the shapefiles
GrassyCreekWatershed@data
NoNameCreekWatershed@data
  #nothing super useful in the attributes
