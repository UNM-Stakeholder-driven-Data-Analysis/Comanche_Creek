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

library(stars)
library(sf)
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)

#### Mapping ####

#creating basemap of Valle Vidal unit on Carson Nat'l Forest
# ValleVidalMap <- get_stamenmap(bbox = c(left = -105.3393,
#                                 bottom = 36.7202,
#                                 right = -105.2284,
#                                 top = 36.8401),
#                        maptype = "terrain", 
#                        crop = FALSE,
#                        zoom = 12)
# ggmap(ValleVidalMap)

#### Load Data ####

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

#Add Landsat data; we need red (band 4) and infrared (band 5) for NDVI

red <- raster("data/raw/Landsat8_082013/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")
near.infrared <- raster("data/raw/Landsat8_082013/LC08_L1TP_033034_20130809_20200912_02_T1_B5.TIF")

#reproject the watershed shapefiles using the crs for the Landsat data, in this case we using the 'red' band which uses the UTM projection

GrassyUTM <- spTransform(GrassyCreekWatershed,
                              crs(red))
crs(GrassyUTM)
#now the Grassy Creek Watershed shapefile is in UTM instead of tmerc

#change No Name Creek to URM too
NoNameUTM <- spTransform(NoNameCreekWatershed,
                         crs(red))
crs(NoNameUTM)


# plot shapefiles
# plot(ValleVidalMap)
# plot(NoNameCreekWatershed, col = "green", add = TRUE)
# plot(GrassyCreekWatershed, col = "blue", add = TRUE)
#   #I can't get both shapefiles to plot on the same map, so I'm   going to move on for now and come back to this later

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

# crop the lidar raster using the vector extent
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

NDVIGrassy082013 = data.frame (Site = "Grassy_Creek",
                               Year = 2013,
                               Month = 08,
                               NDVI = Grassyndvi_noNAs)

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

NDVINoName082013 = data.frame (Site = "NoName_Creek",
                               Year = 2013,
                               Month = 08,
                               NDVI = NoNamendvi_noNAs)
                               
                               