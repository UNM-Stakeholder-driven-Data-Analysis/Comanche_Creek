#### Read Me ####

#### Libraries ####
install.packages('nortest')
library(reshape2)
library(tidyverse)
library(nortest)

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

#now let's look at the data by site
NDVI_Full %>% ggplot(aes(x=NDVI)) + 
              geom_histogram() +
              facet_wrap(~Site) +
  labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")

#let's also see what it's like in a "before" (2005-2013) and "after" period (2013-2022)
#first we have to make the before and after periods in the dataframe
NDVI_Full_BA <- NDVI_Full %>% 
  mutate("Period" = case_when(`Year` == "2005" ~ "Before",
                              `Year` == "2006" ~ "Before",
                              `Year` == "2008" ~ "Before",
                              `Year` == "2010" ~ "Before",
                              `Year` == "2011" ~ "Before",
                              `Year` == "2012" ~ "Before",
                              `Year` == "2013" ~ "Before",
                              `Year` == "2014" ~ "After",
                              `Year` == "2015" ~ "After",
                              `Year` == "2016" ~ "After",
                              `Year` == "2017" ~ "After",
                              `Year` == "2018" ~ "After",
                              `Year` == "2019" ~ "After",
                              `Year` == "2020" ~ "After",
                              `Year` == "2021" ~ "After",
                              `Year` == "2022" ~ "After"))
                              
summary(NDVI_Full_BA)

#next step- convert Period from character to factor
