#### Read Me ####

#The purpose of this code is to understand and explore Landsat imagery data and the shapefiles provided by Quivira. This will be used to develop a method for extracting NDVI values from the Landsat data for the shapefiles. 

#### Libraries ####

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

#### Load Data ####

#Download raw files from Google Drive https://drive.google.com/drive/folders/1BUcrp3w0RnRCkpragy-PKkmmW2SjGXyy
#The folder in this drive is called "raw" and contains all of the raw data files 
#Put this "raw" folder in the "data" folder
#The "data" folder should now have two folders: "processed" and "raw"

#Load shapefiles of Grassy Creek and No Name Creek watershed boundaries
GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                               "Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                               "Watersheds_no name creek")
#Load Landsat data; we need red (band 4) and infrared (band 5) for NDVI. We will use August 2013 to establish a method for extracting NDVI
red <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")
near.infrared <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B5.TIF")

#### Prep data for analysis ####

#View metadata to get info about object class, coordinate reference system, and spatial extent
GrassyCreekWatershed
NoNameCreekWatershed
#these shapefiles are polygons using the transverse mercator  map projection 
#we need them to be in UTM so they have the same crs as the Landsat 8 data

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
#calculate NDVI for Grassy Creek

#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot Grassy Creek shapefile over red band
plot(GrassyUTM,
     main = "Grassy Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the raster using the spatial extent of the Grassy UTM shapefile
red_crop_grassy <- crop(x = red, y= GrassyUTM)
plot(red_crop_grassy)

# add shapefile on top of the existing raster to visualize 
plot(GrassyUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells by making them transparent, showing just the cropped portion
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

# crop the raster using the spatial extent of the shapefile
near.infrared_crop_grassy <- crop(x = near.infrared, y= GrassyUTM)
plot(near.infrared_crop_grassy)

# add shapefile on top of the existing raster
plot(GrassyUTM, add = TRUE) 

#view the cropped raster, R can only plot raster data as a square, so this masks all the null cells
Grassyinfra.Masked <- mask(x = near.infrared_crop_grassy, mask = GrassyUTM)
plot(Grassyinfra.Masked)

#Use each raster layer to caclulate NDVI and plot
#Grassyinfra.Masked= infrared layer and GrassyMasked= red layer

Grassyndvi = (Grassyinfra.Masked - GrassyMasked) / (Grassyinfra.Masked + GrassyMasked)

colors = colorRampPalette(c("red3", "white", "darkcyan"))(255)

plot(Grassyndvi, zlim=c(0, 0.6), col=colors)
#cyan = higher NDVI, red= lower NDVI

#Look at NDVI values
hist(Grassyndvi)

#extract NDVI values from raster
Grassyndvi_vals = values(Grassyndvi)
Grassyndvi_vals

#remove NAs, which are pixels outside the spaital extent of the shapefile
Grassyndvi_noNAs <- Grassyndvi_vals[!is.na(Grassyndvi_vals)]

#check out the data
summary(Grassyndvi_noNAs)
hist(Grassyndvi_noNAs)


#### No Name ####
#calculate NDVI for Grassy Creek

#plot red band
plot(red, col = rev(terrain.colors(50)))

#plot No Name Creek shapefile
plot(NoNameUTM,
     main = "No Name Creek Watershed",
     axes = TRUE,
     border = "blue",
     add = TRUE)

# crop the raster using the spatial extent of the No Name Creek shapefile
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

#Calculate NDVI and plot
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


#### Extract XY Coordinates ####

xyFromCell(Grassyndvi, 1:5419)

class(xyFromCell(Grassyndvi, 1:5419))

df_coordNDVI_082013 <- data.frame(xyFromCell(Grassyndvi, 1:5419), NDVI = Grassyndvi_noNAs)

df_coordNDVI_082013$ID = paste (df_coordNDVI_082013$x, df_coordNDVI_082013$y, sep = "_")
#creates a unique number for each plot

#Now that I have this method established, I will make a for loop to repeat this process for all the layers for each month and year (see Automation code)
#The final output will be a dataframe with x, y, NDVI, x_y, month, year, site for each raster
