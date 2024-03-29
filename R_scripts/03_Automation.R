#### Read Me ####

#The purpose of this code is to create a for loop which applies the method established in the 01_datawrangling code to all the downloaded raster files for each month and year. This will result in a dataframe that has NDVI data for each pixel at each site during each month and year of the study period. Repeat the process again to create a dataframe for the buffered areas.

#### Libraries ####
library(sf)
library(rgdal)
library(raster)
library(ggmap)
library(dplyr)
library(readr)
library(stringr)
library(terra)
library(tidyverse)

#### Subset Red and Infrared Bands ####

#Download raw files from Google Drive https://drive.google.com/drive/folders/1BUcrp3w0RnRCkpragy-PKkmmW2SjGXyy
#The folder in this drive is called "raw" and contains all of the raw data files 
#Put this "raw" folder in the "data" folder
#The "data" folder should now have two folders: "processed" and "raw"

#create files list of Landsat files
files.list <- list.files("data/raw/", pattern = ".TIF")

#we have to subset the Landsat 7 and 8 files because Landsat 7 uses bands 3 and 4 for red and infrared and Landsat 8 uses bands 4 and 5 for red and infrared 
#The file names have month and year, so I used the information in the file names to subset them

#subset Landsat 7 files (2010-2012)
Landsat7 <- which(str_sub(files.list, 18,21) == 2010 |str_sub(files.list, 18,21) == 2011 |str_sub(files.list, 18,21) == 2012)

#subset Landsat 8 files (2013-2022)
Landsat8 <- which(str_sub(files.list, 18,21) == 2013 |str_sub(files.list, 18,21) == 2014 |str_sub(files.list, 18,21) == 2015 |str_sub(files.list, 18,21) == 2016 |str_sub(files.list, 18,21) == 2017 |str_sub(files.list, 18,21) == 2018 |str_sub(files.list, 18,21) == 2019 |str_sub(files.list, 18,21) == 2020 | str_sub(files.list, 18,21) == 2021 | str_sub(files.list, 18,21) == 2022)

#create a list of files for Landsat 7 and 8
Landsat7.files <- files.list[Landsat7]
Landsat8.files <- files.list[Landsat8]

#subset the red and infrared bands from the list of files using the name of the band at the end of the file name
#for Landsat7 red is band 3 and infrared is band 4
#for Landsat 8 red is band 4 and infrared is band 5
Landsat7_red <- which(str_sub(Landsat7.files, -6,-5) == "B3")
Landsat7_infrared <- which(str_sub(Landsat7.files, -6,-5) == "B4")

Landsat8_red <- which(str_sub(Landsat8.files, -6,-5) == "B4")
Landsat8_infrared <- which(str_sub(Landsat8.files, -6,-5) == "B5")

#create a list of red and infrared files for Landsat 7 and 8
Landsat7_red.files <- Landsat7.files[Landsat7_red]
Landsat7_infrared.files <- Landsat7.files[Landsat7_infrared]

Landsat8_red.files <- Landsat8.files[Landsat8_red]
Landsat8_infrared.files <- Landsat8.files[Landsat8_infrared]

#### Inputs ####

#red and infrared bands across all dates
RedBands <- append(Landsat7_red.files, Landsat8_red.files)
InfraredBands <- append(Landsat7_infrared.files, Landsat8_infrared.files)

#watershed shapefiles
GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                                "Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                                "Watersheds_no name creek")
#load a reference raster to put shapefiles into correct crs
red <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")

#watershed shapefiles in the correct UTM crs
GrassyUTM <- spTransform(GrassyCreekWatershed,
                         crs(red))
NoNameUTM <- spTransform(NoNameCreekWatershed,
                         crs(red))
#### Output ####
#this is the structure of the final dataframe
output <- tibble(x=NA, y=NA, NDVI=NA, month=NA, year=NA, site=NA, cell=NA)

#### For Loop Grassy Creek ####
#note: the loop takes about 40 min to run on my computer

for (i in 1:18){
  #get the red and infrared rasters
  temp.red <- raster(str_c("data/raw/", RedBands[i]))
  temp.infrared <-raster(str_c("data/raw/", InfraredBands[i]))
  #crop the rasters to the shapefile
  Red_GrassyMasked <- mask(x = temp.red, mask = GrassyUTM)
  Infra_GrassyMasked <- mask(x = temp.infrared, mask = GrassyUTM)
  #calculate NDVI
  GrassyNDVI = (Infra_GrassyMasked - Red_GrassyMasked) / (Infra_GrassyMasked + Red_GrassyMasked)
 
#extract xy coordinates from raster#
  #masking might be redundant, fix if it takes a long time
  test <- raster::extract(GrassyNDVI, GrassyUTM, df = TRUE, cellnumbers = TRUE)
  # Order (for checking purposes)
  test <- test[order(test$cell),]
    # Extract coordinates
  xy <- xyFromCell(GrassyNDVI, cell = test$cell, spatial = FALSE)
  
  # Convert to df and add cellnumber
  xy <- as.data.frame(xy)
  xy$cell <- test$cell
#make into a dataframe with same columns as output
  df.grassy <- tibble (x=xy$x, y=xy$y, NDVI=test$layer, month=str_sub(RedBands[i],22,23), year=str_sub(RedBands[i],18,21), site="Grassy_Creek", cell = xy$cell)
  #make the output by combining iterations of dataframes
  output <- rbind(output, df.grassy) 
  i
}

output$ID = paste(output$x,output$y,sep="_")

write.csv(output, "data/processed/GrassyCreek_Dataframe.csv")
  

#### For Loop No Name Creek ####
#create new blank output
output <- tibble(x=NA, y=NA, NDVI=NA, month=NA, year=NA, site=NA, cell=NA)

for (i in 1:18){
  #get the red and infrared rasters
  temp.red <- raster(str_c("data/raw/", RedBands[i]))
  temp.infrared <-raster(str_c("data/raw/", InfraredBands[i]))
  #crop the rasters to the shapefile
  Red_NoNameMasked <- mask(x = temp.red, mask = NoNameUTM)
  Infra_NoNameMasked <- mask(x = temp.infrared, mask = NoNameUTM)
  #calculate NDVI
  NoNameNDVI = (Infra_NoNameMasked - Red_NoNameMasked) / (Infra_NoNameMasked + Red_NoNameMasked)
  
  #extract xy coordinates from raster#
  #masking might be redundant, fix if it takes a long time
  test <- raster::extract(NoNameNDVI, NoNameUTM, df = TRUE, cellnumbers = TRUE)
  # Order (for checking purposes)
  test <- test[order(test$cell),]
  # Extract coordinates
  xy <- xyFromCell(NoNameNDVI, cell = test$cell, spatial = FALSE)
  
  # Convert to df and add cellnumber
  xy <- as.data.frame(xy)
  xy$cell <- test$cell
  #make into a dataframe with same columns as output
  df.noname <- tibble (x=xy$x, y=xy$y, NDVI=test$layer, month=str_sub(RedBands[i],22,23), year=str_sub(RedBands[i],18,21), site="NoName_Creek", cell = xy$cell)
  #make the output by combining iterations of dataframes
  output <- rbind(output, df.noname) 
}
output$ID = paste(output$x,output$y,sep="_")

write.csv(output, "data/processed/NoNameCreek_Dataframe.csv")

#### Create Full DataFrame ####

Full_Dataframe <- list.files(path="data/processed/", pattern = "_Dataframe", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  na.omit(Full_Dataframe)

write.csv(Full_Dataframe, "data/processed/Full_Dataframe.csv")



#### Repeat for Buffers ####

# Load buffer shapefiles

GrassyBuf <- readOGR(dsn = "data/processed/GrassyBuf.shp")
NoNameBuf <- readOGR(dsn = "data/processed/NoNameBuf.shp")

#load raster to use a reference for crs
red <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")

#watershed shapefiles in the correct UTM crs
GrassyBufUTM <- spTransform(GrassyBuf,
                            crs(red))
NoNameBufUTM <- spTransform(NoNameBuf,
                            crs(red))

#output
#this is the final dataframe
output <- tibble(x=NA, y=NA, NDVI=NA, month=NA, year=NA, site=NA, cell=NA)

## run loop for Grassy Creek Buffer ##
#for loop
for (i in 1:18){
  #get the red and infrared rasters
  temp.red <- raster(str_c("data/raw/", RedBands[i]))
  temp.infrared <-raster(str_c("data/raw/", InfraredBands[i]))
  #crop the rasters to the shapefile
  Red_GrassyMasked <- mask(x = temp.red, mask = GrassyBufUTM)
  Infra_GrassyMasked <- mask(x = temp.infrared, mask = GrassyBufUTM)
  #calculate NDVI
  GrassyNDVI = (Infra_GrassyMasked - Red_GrassyMasked) / (Infra_GrassyMasked + Red_GrassyMasked)
  
  #extract xy coordinates from raster#
  #masking might be redundant, fix if it takes a long time
  test <- raster::extract(GrassyNDVI, GrassyBufUTM, df = TRUE, cellnumbers = TRUE)
  # Order (for checking purposes)
  test <- test[order(test$cell),]
  # Extract coordinates
  xy <- xyFromCell(GrassyNDVI, cell = test$cell, spatial = FALSE)
  
  # Convert to df and add cellnumber
  xy <- as.data.frame(xy)
  xy$cell <- test$cell
  #make into a dataframe with same columns as output
  df.grassy <- tibble (x=xy$x, y=xy$y, NDVI=test$layer, month=str_sub(RedBands[i],22,23), year=str_sub(RedBands[i],18,21), site="Grassy_Creek", cell = xy$cell)
  #make the output by combining iterations of dataframes
  output <- rbind(output, df.grassy) 
}

output$ID = paste(output$x,output$y,sep="_")

write.csv(output, "data/processed/GrassyCreekBuf_Dataframe.csv")


## repeat loop for No Name Creek buffer ##
#output
#this is the final dataframe
output <- tibble(x=NA, y=NA, NDVI=NA, month=NA, year=NA, site=NA, cell=NA)

for (i in 1:18){
  #get the red and infrared rasters
  temp.red <- raster(str_c("data/raw/", RedBands[i]))
  temp.infrared <-raster(str_c("data/raw/", InfraredBands[i]))
  #crop the rasters to the shapefile
  Red_NoNameMasked <- mask(x = temp.red, mask = NoNameBufUTM)
  Infra_NoNameMasked <- mask(x = temp.infrared, mask = NoNameBufUTM)
  #calculate NDVI
  NoNameNDVI = (Infra_NoNameMasked - Red_NoNameMasked) / (Infra_NoNameMasked + Red_NoNameMasked)
  
  #extract xy coordinates from raster#
  #masking might be redundant, fix if it takes a long time
  test <- raster::extract(NoNameNDVI, NoNameBufUTM, df = TRUE, cellnumbers = TRUE)
  # Order (for checking purposes)
  test <- test[order(test$cell),]
  # Extract coordinates
  xy <- xyFromCell(NoNameNDVI, cell = test$cell, spatial = FALSE)
  
  # Convert to df and add cellnumber
  xy <- as.data.frame(xy)
  xy$cell <- test$cell
  #make into a dataframe with same columns as output
  df.noname <- tibble (x=xy$x, y=xy$y, NDVI=test$layer, month=str_sub(RedBands[i],22,23), year=str_sub(RedBands[i],18,21), site="NoName_Creek", cell = xy$cell)
  #make the output by combining iterations of dataframes
  output <- rbind(output, df.noname) 
}
output$ID = paste(output$x,output$y,sep="_")

write.csv(output, "data/processed/NoNameCreekBuf_Dataframe.csv")

## Create Full Dataframe for Buffers ## 

Full_Dataframe <- list.files(path="data/processed/", pattern = "Buf_Dataframe", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows %>%
  na.omit(Full_Dataframe)

write.csv(Full_Dataframe, "data/processed/FullBuf_Dataframe.csv")

