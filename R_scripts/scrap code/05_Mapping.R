#### Libraries ####
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(stars)
#### Load Data ####
RewettingSites <- readOGR(dsn = "data/raw/Potential_RewettingSites","Potential Rewetting Sites")
GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries","Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries","Watersheds_no name creek")
#### Mapping ####

api_secret <- ''
register_google(key = api_secret)

RewettingNewCRS <- spTransform(RewettingSites,
                         crs("+init=epsg:3857"))

VV_basemap <- get_map(location=c(lon = -105.3393, lat = 36.7202), zoom=11, maptype = 'terrain-background', source = 'stamen')

######################################################

Rewetting_sub <- RewettingSites[RewettingSites$xy %in% c("-105.299502", "36.804658"), ]

plot(Rewetting_sub)

####################
plot(RewettingSites)
axis(1)
axis(2)
crs(RewettingSites)

Rewetting_sub <- RewettingSites[RewettingSites$x %in% c("475000", "480000"), ]

plot(Rewetting_sub)

RewettingSites$Tributary

plot(RewetingSites$Triubtary = "Comanche Creek")

plot(RewettingSites['Tributary')
