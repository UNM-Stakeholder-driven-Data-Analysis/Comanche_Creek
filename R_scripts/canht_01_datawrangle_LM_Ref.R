#### read me ####
# the purpose of this script is to comapre LANDSAT tree cover data and PRISM spatially distributed climate data

# goal is to replicate the global analysis described here, but for NM: Xu C, Staal A, Hantson S, Holmgren M, van Nes EH, Scheffer M. 2018. Remotely sensed canopy height reveals three pantropical ecosystem states. Ecology 99:235–7.

## PRISM data notes:
# I am using PRISM long term (30 y) annual normals
# See the following links:
# https://prism.oregonstate.edu/normals/
# https://prism.oregonstate.edu/documents/PRISM_formats.pdf
# https://prism.oregonstate.edu/documents/PRISM_datasets.pdf
# PRISM ppt long term normals are also provided for each month of the year, which is specified by a 2 digit numeral in the file name: "800mM2_01" = January; "800mM2_02" = Feb; etc.

## Reitz ET notes:
# # See Reitz M, Sanford WE, Senay GB, Cazenas J. 2017. Annual Estimates of Recharge, Quick-Flow Runoff, and Evapotranspiration for the Contiguous U.S. Using Empirical Regression Equations. J Am Water Resour Assoc 53:961–83.
# I downloaded .tiff file for annual average evapotranspiration rates across the contiguous US, 2000-2013 from here: https://www.sciencebase.gov/catalog/item/56c49126e4b0946c65219231
# ET is avaiable by year from 2000 to 2013. I downloaded the average: “2000-2013 average ET, m/yr”



#### libraries ####

library(rgdal)
library(gdalUtils)
library(ggmap)
library(rgdal)
library(broom)
library(rgeos)
library(raster)
library(sf)
library(maps)
library(ggsn)
library(leaflet)
library(ggspatial)
library(geojsonio)
library(usmap)
library(terra)
library(rasterVis)
library(sp)
library(scattermore)
library(RColorBrewer)
library(beepr)
library(tidyverse)
library(Hmisc)
library(earlywarnings)
library(quantmod)


# turn off scientific notation
options(scipen=999)

#### fxns ####

# https://csaybar.github.io/blog/2018/12/05/resample/
#' Resampling a Raster* object via GDAL
#' 
#' @param r Raster* object to be resampled
#' @param r_base Raster* object with parameters that r
#' should be resampled to.
#' @param method Character. GDAL resampling_method
#' ("near"|"bilinear"|"cubic"|"cubicspline"|
#'  "lanczos"|"average"|"mode"|"max"|"min"|
#'  "med"|"q1"|"q3")
#'
#' @export
gdal_resample <- function(r, r_base, method = 'bilinear') {
  
  #Geometry attributes
  t1 <- c(xmin(r_base), ymin(r_base), 
          xmax(r_base), ymax(r_base))
  res <- res(r_base)
  
  #Temporal files
  tmp_outname <- sprintf('%s.tif', tempfile())
  tmp_inname <- sprintf('%s.tif', tempfile())
  writeRaster(r, tmp_inname)
  
  #GDAL time!
  gdalwarp(tmp_inname, tmp_outname, 
           tr = res, te = t1, r = method)
  resample_raster = raster(tmp_outname)
  
  return(resample_raster)
}
#### create df with treeht_no0s, swgap, percTC, PRISM, ET, and BP ####

# load rasters
treeht_no0s <- raster("Mapping/outside_data/tree_ht_urban_ag_barren_water_wetland_nodata_no0s.tif")
percTC <- raster("Mapping/outside_data/NM_MODIS_percTC.tif")
swgap <- raster("Mapping/outside_data/NM_swgap.tif")
ppt <- raster("Mapping/outside_data/PRISM_ppt.tif")
temp <- raster("Mapping/outside_data/PRISM_tmean.tif")
tmax <- raster("Mapping/outside_data/PRISM_tmax.tif")
vdpmax <- raster("Mapping/outside_data/PRISM_vdpmax.tif")
ET <- raster("Mapping/outside_data/NM_ET_0013.tif")
BP <- raster("Mapping/outside_data/NM_iBP.tif")

# crop all rasters to treeht_no0s raster
percTC_crop = crop(percTC, treeht_no0s); beep(2)
swgap_crop = crop(swgap, treeht_no0s); beep(2)
ppt_crop = crop(ppt, treeht_no0s); beep(2)
temp_crop = crop(temp, treeht_no0s); beep(2)
tmax_crop = crop(tmax, treeht_no0s); beep(2)
vdpmax_crop = crop(vdpmax, treeht_no0s); beep(2)
ET_crop = crop(ET, treeht_no0s); beep(2)
BP_crop = crop(BP, treeht_no0s); beep(2)

# resample rasters to match treeht_no0s
percTC_highres <- gdal_resample(percTC_crop, treeht_no0s)
swgap_highres <- gdal_resample(swgap_crop, treeht_no0s)
ppt_highres <- gdal_resample(ppt_crop, treeht_no0s)
temp_highres <- gdal_resample(temp_crop, treeht_no0s)
tmax_highres <- gdal_resample(tmax_crop, treeht_no0s)
vdpmax_highres <- gdal_resample(vdpmax_crop, treeht_no0s)
ET_highres <- gdal_resample(ET_crop, treeht_no0s)
BP_highres <- gdal_resample(BP_crop, treeht_no0s)
beep(2)

# check resolutions
res(percTC_highres)
res(swgap_highres)
res(ppt_highres)
res(temp_highres)
res(tmax_highres)
res(vdpmax_highres)
res(ET_highres)
res(BP_highres)

# extract values from rasters
treeht_no0s_vals = values(treeht_no0s)
percTC_highres_vals = values(percTC_highres)
swgap_highres_vals = values(swgap_highres)
ppt_highres_vals = values(ppt_highres)
temp_highres_vals = values(temp_highres)
tmax_highres_vals = values(tmax_highres)
vdpmax_highres_vals = values(vdpmax_highres)
ET_highres_vals = values(ET_highres)
BP_highres_vals = values(BP_highres)
beep(2)

# make df
tree_climate = data.frame(tree_ht_m = treeht_no0s_vals,
                          percTC = percTC_highres_vals,
                          swgap = swgap_highres_vals,
                          ppt = ppt_highres_vals,
                          temp = temp_highres_vals,
                          tmax = tmax_highres_vals,
                          vdpmax = vdpmax_highres_vals,
                          ET = ET_highres_vals,
                          BP = BP_highres_vals)
# remove NAs
tree_climate_noNAs = tree_climate[!is.na(tree_climate$tree_ht_m),]
# make swgap integers
tree_climate_noNAs$swgap = as.integer(tree_climate_noNAs$swgap)
# remove water from the percTC col
tree_climate_noNAs$percTC[tree_climate_noNAs$percTC>100] = NA
# add swgap categories
ref = read.csv("Data/raw/swgap_attributes.csv", header=T)
ref = ref[,c(1:2)]
tree_climate_noNAs = left_join(tree_climate_noNAs, ref, by=c("swgap" = "Value"))

# examine data
hist(tree_climate_noNAs$tree_ht_m, breaks=35)
hist(tree_climate_noNAs$percTC, breaks=100)
table(tree_climate_noNAs$Class)
hist(tree_climate_noNAs$ppt, breaks=100)
hist(tree_climate_noNAs$temp, breaks=100)
hist(tree_climate_noNAs$tmax, breaks=100)
hist(tree_climate_noNAs$vdpmax, breaks=100)
hist(tree_climate_noNAs$ET, breaks=100)
hist(tree_climate_noNAs$BP, breaks=100)

# save df
saveRDS(tree_climate_noNAs, "Data/processed/tree_climate.rds"); beep(2)

#### add PET and AI ####

# Global 30-arc res (~1km at equator, so similar to the 800m res PRISM data) PET and AI are from: https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v2/7504448/3
# global was clipped to NM in QGIS

# PET is available as the long term monthly or annual avg - here I am using annual
# AIR is availabl only as the long term annual avg
# long term = 1970-2000

## load saved raster as df data ##
tree_climate = readRDS("Data/processed/tree_climate.rds"); beep(2)

# load new rasters (plot tree ht ref raster)
treeht_no0s <- raster("Mapping/outside_data/tree_ht_urban_ag_barren_water_wetland_nodata_no0s.tif")
PET = raster("Mapping/outside_data/NM_PET_1970_2000_avg_annual.tif")
AI = raster("Mapping/outside_data/NM_AI_1970_2000_avg_annual.tif")

# crop all rasters to treeht_no0s raster
PET_crop = crop(PET, treeht_no0s); beep(2)
AI_crop = crop(AI, treeht_no0s); beep(2)

# resample rasters to match treeht_no0s
PET_highres <- gdal_resample(PET_crop, treeht_no0s)
AI_highres <- gdal_resample(AI_crop, treeht_no0s)
beep(2)

# check resolutions
res(treeht_no0s)
res(PET_highres)
res(AI_highres)

# extract values from rasters
treeht_no0s_vals = values(treeht_no0s)
PET_vals = values(PET_highres)
AI_vals = values(AI_highres)
beep(2)

# make df
tree_PET_AI = data.frame(tree_ht_m = treeht_no0s_vals,
                          PET = PET_vals,
                          AI = AI_vals)
# remove NAs
tree_PET_AI_noNAs = tree_PET_AI[!is.na(tree_PET_AI$tree_ht_m),]

# check that tree_ht_m matches
# ?all = are all values true?
all(tree_PET_AI_noNAs$tree_ht_m == tree_climate$tree_ht_m)

# join
tree_climate_2 = cbind(tree_climate, tree_PET_AI_noNAs[,2:3])

# examine data
hist(tree_climate_2$PET, breaks=100)
hist(tree_climate_2$AI, breaks=100)

# save df
saveRDS(tree_climate_2, "Data/processed/tree_climate.rds"); beep(2)



