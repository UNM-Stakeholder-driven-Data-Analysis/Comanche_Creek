#### Read Me ####

# The purpose of this code is to create a shapefile that buffers the streams. This buffer represents potential wetland expansion areas in each watershed
#code created by Alex Webster

#### libraries ####
library(rgeos)
library(sp)
library(stars)
library(sf)
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)
library(parallel)
library(reproducible)
library(dplyr)
library(readr)

#### import shapefiles of watersheds ####

GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries","Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries","Watersheds_no name creek")

summary(NoNameCreekWatershed)

#### import stream line features ####

fgdb <- "data/raw/NHDPLUS_H_1302_HU4_GDB.gdb/"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
HUC1302 <- readOGR(dsn = "data/raw/NHDPLUS_H_1302_HU4_GDB.gdb/", 
                   "NHDFlowline")

#plot(HUC1302)
# this is large so takes a while to plot

# Determine the FC extent, projection, and attribute information
summary(HUC1302)

#### get features in same projection ####

#watershed shapefiles in the correct NAD83 crs
GrassyNAD83 <- spTransform(GrassyCreekWatershed,
                         crs(HUC1302))
NoNameNAD83 <- spTransform(NoNameCreekWatershed,
                         crs(HUC1302))

# check
st_crs(NoNameNAD83)==st_crs(GrassyNAD83)
st_crs(GrassyNAD83)==st_crs(HUC1302)

#### clip line features with watershed polygon ####

# Original data as sf objects
HUC1302_sf <- st_as_sf(HUC1302)
GrassyNAD83_sf <- st_as_sf(GrassyNAD83)
NoNameNAD83_sf <- st_as_sf(NoNameNAD83)

GrassyHUC1302_cropped<-st_intersection(HUC1302_sf,GrassyNAD83_sf)
plot(GrassyHUC1302_cropped$geometry)

NoNameHUC1302_cropped<-st_intersection(HUC1302_sf,NoNameNAD83_sf)
plot(NoNameHUC1302_cropped$geometry)

#### join flowlines ####

Grassyflow = st_union(GrassyHUC1302_cropped)
summary(Grassyflow)
plot(Grassyflow)

NoNameflow = st_union(NoNameHUC1302_cropped)
summary(NoNameflow)
plot(NoNameflow)

#### create buffer line feature ####

# convert to UTM so that geometry is in meters
Grassyflow_UTM <- st_transform(Grassyflow, CRS("+proj=utm +zone=13 ellps=WGS84"))
class(Grassyflow_UTM)
NoNameflow_UTM <- st_transform(NoNameflow, CRS("+proj=utm +zone=13 ellps=WGS84"))
class(NoNameflow_UTM)

# MAKE BUFFER
GrassyBuf <- st_buffer(Grassyflow_UTM,dist=60,endCapStyle="ROUND")
plot(GrassyBuf)
NoNameBuf <- st_buffer(NoNameflow_UTM,dist=60,endCapStyle="ROUND")
plot(NoNameBuf)

#plot buffers
plot(GrassyBuf, col = "lightblue", add = TRUE) 
plot(Grassyflow_UTM, col = "blue", add = TRUE) 
 
plot(NoNameBuf, col = "pink", add = TRUE) 
plot(NoNameflow_UTM, col = "blue", add = TRUE) 


#### save buffers ####

st_write(GrassyBuf, "data/processed/GrassyBuf.shp")
st_write(NoNameBuf, "data/processed/NoNameBuf.shp")
