##Automation for NDVI data##

#create files list
files.list <- list.files("data/raw/", pattern = ".TIF")

#subset Landsat 7 files (2010-2012)
Landsat7 <- which(str_sub(files.list, 18,21) == 2010 |str_sub(files.list, 18,21) == 2011 |str_sub(files.list, 18,21) == 2012)

#subset Landsat 8 files (2013-2022)
Landsat8 <- which(str_sub(files.list, 18,21) == 2013 |str_sub(files.list, 18,21) == 2014 |str_sub(files.list, 18,21) == 2015 |str_sub(files.list, 18,21) == 2016 |str_sub(files.list, 18,21) == 2017 |str_sub(files.list, 18,21) == 2018 |str_sub(files.list, 18,21) == 2019 |str_sub(files.list, 18,21) == 2020 | str_sub(files.list, 18,21) == 2021 | str_sub(files.list, 18,21) == 2022)

#create a list of files for Landsat 7 and 8
Landsat7.files <- files.list[Landsat7]
Landsat8.files <- files.list[Landsat8]

#subset the red and infrared bands from the list of files
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

##inputs##
#red and infrared bands across all dates
RedBands <- append(Landsat7_red.files, Landsat8_red.files)
InfraredBands <- append(Landsat7_infrared.files, Landsat8_infrared.files)

#watershed shapefiles
GrassyCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                                "Watersheds_grassy creek")
NoNameCreekWatershed <- readOGR(dsn = "data/raw/Watershed_Boundaries",
                                "Watersheds_no name creek")

red <- raster("data/raw/LC08_L1TP_033034_20130809_20200912_02_T1_B4.TIF")

#watershed shapefiles in the correct UTM crs
GrassyUTM <- spTransform(GrassyCreekWatershed,
                         crs(red))
NoNameUTM <- spTransform(NoNameCreekWatershed,
                         crs(red))

#output
#this is the final dataframe
output <- tibble(x=NA, y=NA, NDVI=NA, month=NA, year=NA, site=NA, cell=NA)

#for loop
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
  

## repeat loop for No Name Creek
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
  output_NoName <- rbind(output, df.noname) 
  i
}
