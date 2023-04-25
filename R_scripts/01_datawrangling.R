#### Read Me ####

#### Libraries ####

# install.packages('devtools')
# devtools::install_github("dkahle/ggmap")
# install.packages('maps')
# install.packages('rgdal')
# install.packages('rgeos')
# install.packages('raster')
# install.packages('stars')
# install.packages('sf')
#install.packages('parallel')
#install.packages('reproducible')
#install.packages('readr')
#install.packages('dplyr')

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
library(stringr)
library(terra)

#### Mapping ####

#creating basemap of Valle Vidal unit on Carson Nat'l Forest
ValleVidalMap <- get_stamenmap(bbox = c(left = -105.3393,
                                bottom = 36.7202,
                                right = -105.2284,
                                top = 36.8401),
                       maptype = "terrain",
                       crop = FALSE,
                       zoom = 12)
# plot shapefiles
plot(ValleVidalMap)
plot(NoNameCreekWatershed, col = "green", add = TRUE)
plot(GrassyCreekWatershed, col = "blue", add = TRUE)
#I can't get both shapefiles to plot on the same map, so I'm   going to move on for now and come back to this later

##trying to plot on basemap##

# ggmap(ValleVidalMap)
# colors = colorRampPalette(c("red3", "white", "darkcyan"))(255)
# 
# plot(ValleVidalMap,
#      main = "ValleVidalMap",
#      col = grey(1:100/100))
# plot(Rewettingndvi, 
#      main = "NDVI Rewetting",
#      add = TRUE, alpha = 1.0,
#      zlim=c(0, 0.6), col=colors)



#### Load Data ####

#Loading shapefiles of Grassy Creek and No Name Creek watershed boundaries
GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                               "Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                               "Watersheds_no name creek")
RewettingSites <- readOGR(dsn = "data/raw/Potential_RewettingSites","Potential Rewetting Sites")

WetlandAreas <- readOGR(dsn = "data/raw/WetlandAreas", "RevisedWetlandAreas")

#View metadata to get info about object class, coordinate reference system, and spatial extent
GrassyCreekWatershed
NoNameCreekWatershed
#these shapefiles are polygons using the transverse mercator  map projection 
#we need them to be in UTM so they have the same crs as the Landsat 8 data

#Add Landsat data; we need red (band 4) and infrared (band 5) for NDVI

red <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")
near.infrared <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B5.TIF")

#reproject the watershed shapefiles using the crs for the Landsat data, in this case we using the 'red' band which uses the UTM projection

GrassyUTM <- spTransform(GrassyCreekWatershed,
                              crs(red))
crs(GrassyUTM)
#now the Grassy Creek Watershed shapefile is in UTM instead of tmerc

#change No Name Creek to URM too
NoNameUTM <- spTransform(NoNameCreekWatershed,
                         crs(red))
crs(NoNameUTM)

#### Grassy Creek ####

#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot Grassy Creek shapefile
plot(GrassyUTM,
     main = "Grassy Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)


# crop the lidar raster using the vector extent
red_crop_grassy <- crop(x = red, y= GrassyUTM)
plot(red_crop_grassy)


# add shapefile on top of the existing raster
plot(GrassyUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells transparent, showing just the cropped portion
GrassyMasked <- mask(x = red_crop_grassy, mask = GrassyUTM)
plot(GrassyMasked)

##Do the same for the infrared band##
#plot infrared band
plot(near.infrared, col = rev(terrain.colors(50)))

#plot Grassy Creek shapefile
plot(GrassyUTM,
     main = "Grassy Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the infrared raster using the vector extent
near.infrared_crop_grassy <- crop(x = near.infrared, y= GrassyUTM)
plot(near.infrared_crop_grassy)

# add shapefile on top of the existing raster
plot(GrassyUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells
Grassyinfra.Masked <- mask(x = near.infrared_crop_grassy, mask = GrassyUTM)
plot(Grassyinfra.Masked)

#Do math for NDVI and plot

Grassyndvi = (Grassyinfra.Masked - GrassyMasked) / (Grassyinfra.Masked + GrassyMasked)

colors = colorRampPalette(c("red3", "white", "darkcyan"))(255)

plot(Grassyndvi, zlim=c(0, 0.6), col=colors)

#Let's see what the NDVI values look like
hist(Grassyndvi)

#extract NDVI values from raster
Grassyndvi_vals = values(Grassyndvi)
Grassyndvi_vals

#remove NAs
Grassyndvi_noNAs <- Grassyndvi_vals[!is.na(Grassyndvi_vals)]

#check out the data
summary(Grassyndvi_noNAs)
hist(Grassyndvi_noNAs)

#make dataframe with Grassy Creek NDVI, No Name Creek NDVI, month, and year

NDVIGrassy082005 = data.frame (Site = "Grassy_Creek",
                               Year = 2005,
                               Month = 08,
                               NDVI = Grassyndvi_noNAs)
#make a CSV of the data
write.csv(NDVIGrassy082005, "data/processed/NDVIGrassy082005.csv")

#### No Name ####

#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot No Name Creek shapefile
plot(NoNameUTM,
     main = "No Name Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the lidar raster using the vector extent
red_crop_noname <- crop(x = red, y= NoNameUTM)
plot(red_crop_noname)

# add shapefile on top of the existing raster
plot(NoNameUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells transparent, showing just the cropped portion
NoNameMasked <- mask(x = red_crop_noname, mask = NoNameUTM)
plot(NoNameMasked)

##Do the same for the infrared band##
#plot infrared band
plot(near.infrared, col = rev(terrain.colors(50)))

#plot NoName Creek shapefile
plot(NoNameUTM,
     main = "No Name Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the lidar raster using the vector extent
near.infrared_crop_noname <- crop(x = near.infrared, y= NoNameUTM)
plot(near.infrared_crop_noname)

# add shapefile on top of the existing raster
plot(NoNameUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells
NoNameinfra.Masked <- mask(x = near.infrared_crop_noname, mask = NoNameUTM)
plot(NoNameinfra.Masked)

#Do math for NDVI and plot

NoNamendvi = (NoNameinfra.Masked - NoNameMasked) / (NoNameinfra.Masked + NoNameMasked)

colors = colorRampPalette(c("red3", "white", "darkcyan"))(255)

plot(NoNamendvi, zlim=c(0, 0.6), col=colors)

#Let's see what the NDVI values look like
hist(NoNamendvi)

#extract NDVI values from raster
NoNamendvi_vals = values(NoNamendvi)
NoNamendvi_vals

#remove NAs
NoNamendvi_noNAs <- NoNamendvi_vals[!is.na(NoNamendvi_vals)]

#check out the data
summary(NoNamendvi_noNAs)
hist(NoNamendvi_noNAs)

#make dataframe with Grassy Creek NDVI, No Name Creek NDVI, month, and year

NDVINoName082005 = data.frame (Site = "NoName_Creek",
                               Year = 2005,
                               Month = 08,
                               NDVI = NoNamendvi_noNAs)

#make a CSV of the data                               
write.csv(NDVINoName082005, "data/processed/NDVINoName082005.csv")
  

#### Data Frame ####
#now I have a bunch of csv's for each month and year with NDVI data and I want to put them all into one data frame

df <- list.files(path="data/processed/NDVI_Filled", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

summary(df)
View(df)

write.csv(df, "data/processed/NDVI_Filled_GrassyNoName_Full.csv")

#### Extract XY Coordinates ####

xyFromCell(Grassyndvi, 1:5419)

class(xyFromCell(Grassyndvi, 1:5419))

df_coordNDVI_082013 <- data.frame(xyFromCell(Grassyndvi, 1:5419), NDVI = Grassyndvi_noNAs)

# df$ID = paste (df$x, df$y, sep = "_")
#creates a unique number for each plot

#create a dataframe with x, y, NDVI, x_y, month, year, site for each raster
#then use rbind to join the dataframes together

#### Potential Rewetting Sites ####

RewettingSites <- readOGR(dsn = "data/raw/Potential_RewettingSites","Potential Rewetting Sites")

crs(RewettingSites)

RewettingUTM <- spTransform(RewettingSites, crs(red))

#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot Rewetting shapefile
plot(RewettingUTM,
     main = "Grassy Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the lidar raster using the vector extent
red_crop_rewetting <- crop(x = red, y= RewettingUTM)
plot(red_crop_rewetting)


# add shapefile on top of the existing raster
plot(RewettingUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells transparent, showing just the cropped portion
RewettingMasked <- mask(x = red_crop_rewetting, mask = RewettingUTM)
plot(RewettingMasked)

##Do the same for the infrared band##
#plot infrared band
plot(near.infrared, col = rev(terrain.colors(50)))

#plot rewetting shapefile
plot(RewettingUTM,
     main = "Rewetting Sites",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the lidar raster using the vector extent
near.infrared_crop_rewetting <- crop(x = near.infrared, y= RewettingUTM)
plot(near.infrared_crop_rewetting)

# add shapefile on top of the existing raster
plot(RewettingUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells
Rewettinginfra.Masked <- mask(x = near.infrared_crop_rewetting, mask = RewettingUTM)
plot(Rewettinginfra.Masked)

#Do math for NDVI and plot

Rewettingndvi = (Rewettinginfra.Masked - RewettingMasked) / (Rewettinginfra.Masked + RewettingMasked)

colors = colorRampPalette(c("red3", "white", "darkcyan"))(255)

plot(Rewettingndvi, zlim=c(0, 0.6), col=colors)

#Let's see what the NDVI values look like
hist(Rewettingndvi)

#### Wetland Areas ####

WetlandUTM <- spTransform(WetlandAreas, crs(red))

#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot Rewetting shapefile
plot(WetlandUTM,
     main = "Wetland Areas",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the lidar raster using the vector extent
red_crop_wetland <- crop(x = red, y= WetlandUTM)
plot(red_crop_wetland)


# add shapefile on top of the existing raster
plot(WetlandUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells transparent, showing just the cropped portion
WetlandMasked <- mask(x = red_crop_wetland, mask = WetlandUTM)
plot(WetlandMasked)

##Do the same for the infrared band##
#plot infrared band
plot(near.infrared, col = rev(terrain.colors(50)))

#plot rewetting shapefile
plot(WetlandUTM,
     main = "Wetland Areas",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the lidar raster using the vector extent
near.infrared_crop_wetland <- crop(x = near.infrared, y= WetlandUTM)
plot(near.infrared_crop_wetland)

# add shapefile on top of the existing raster
plot(WetlandUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells
Wetlandinfra.Masked <- mask(x = near.infrared_crop_wetland, mask = WetlandUTM)
plot(Wetlandinfra.Masked)

#Do math for NDVI and plot

Wetlandndvi = (Wetlandinfra.Masked - WetlandMasked) / (Wetlandinfra.Masked + WetlandMasked)

colors = colorRampPalette(c("red3", "white", "darkcyan"))(255)

plot(Wetlandndvi, zlim=c(0, 0.6), col=colors)

#Let's see what the NDVI values look like
hist(Wetlandndvi)


WetlandKeyAreas <- Wetlandndvi > 0.44992

WetlandKeyAreas[Wetlandndvi < 0.44992] <- NA

plot(WetlandKeyAreas)




