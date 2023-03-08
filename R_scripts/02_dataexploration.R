#### Read Me ####

#### Libraries ####

library(reshape2)
library(tidyverse)

#### Load Data ####

NDVI_Full <- read.csv("data/processed/NDVIGrassyNoName_Full.csv", header = T, stringsAsFactors = T)

#### Looking at the data ####
dim(NDVI_Full)

#6 variables and 250,384 observations

summary(NDVI_Full) #gives mean, median, min, and max

#data makes sense, nothing looks too out of place

str(NDVI_Full) #shows different data types

#let's look at the distribution
hist(NDVI_Full$NDVI)

#we can make it look a little nicer
NDVI_Full %>% ggplot(aes(x=NDVI)) + geom_histogram() +
  labs(title="NDVI Histogram All Sites",x="NDVI", y = "Number of observations")
