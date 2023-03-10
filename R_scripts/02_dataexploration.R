#### Read Me ####

#### Libraries ####
install.packages('nortest')
install.packages("spdep")
install.packages("raster")
install.packages("rgdal")
install.packages("tmap")
install.packages("shinyjs")
install.packages('lubridate')
install.packages('psych') # to plot pair-wise correlations
install.packages('car') # I like their qq plot fxn
install.packages('tsibble') # useful for creating time series objects
install.packages('forecast') # I like their Acf fxn
install.packages('ape') # for spatial autocorrelation
install.packages('ade4')# for spatial autocorrelation
install.packages('rgdal')

#Load libraries
library(tmap)
library(spdep)
library(raster)
library(shinyjs) 
library(reshape2)
library(tidyverse)
library(nortest)
library(tidyverse)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(rgdal) # for mapping

#### Load Data ####

NDVI_Full <- read.csv("data/processed/NDVIGrassyNoName_Full.csv", header = T, stringsAsFactors = T)

#tidy data- make the before and after periods in the dataframe
#"before" (2005-2013) and "after" (2013-2022)

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
#convert Period from character to factor
NDVI_Full_BA$Period <- as.factor(NDVI_Full_BA$Period)
class(NDVI_Full_BA$Period)

str(NDVI_Full_BA)

#### Data Types ####
dim(NDVI_Full)

#6 variables and 250,384 observations

summary(NDVI_Full) #gives mean, median, min, and max

#data makes sense, nothing looks too out of place

str(NDVI_Full) #shows different data types

#### Data Distribution ####
#let's look at the distribution
hist(NDVI_Full$NDVI)
qqPlot(NDVI_Full_BA$NDVI)

#we can make it look a little nicer
NDVI_Full %>% ggplot(aes(x=NDVI)) + geom_histogram() +
  labs(title="NDVI Histogram All Sites",x="NDVI", y = "Number of observations")

#now let's look at the data by site
NDVI_Full %>% ggplot(aes(x=NDVI)) + 
              geom_histogram() +
              facet_wrap(~Site) +
  labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")
#QQ Plot
temp = NDVI_Full_BA[NDVI_Full_BA$Site == "Grassy_Creek",]
qqPlot(temp$NDVI, main = "QQ Plot Grassy Creek")

temp =NDVI_Full_BA[NDVI_Full_BA$Site == "NoName_Creek",]
qqPlot(temp$NDVI, main = "QQ Plot NoName Creek")

#Histograms of data Before vs After
NDVI_Full_BA %>% mutate(Period = fct_relevel(Period, 
                                           "Before", "After"))%>% ggplot(aes(x=NDVI)) + 
                geom_histogram() +
                facet_wrap(~Period) +
                labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")


#Histograms for Before and After per site
NDVI_Full_BA %>% mutate(Period = fct_relevel(Period, 
                                             "Before", "After")) %>% ggplot(aes(x=NDVI)) + 
  geom_histogram() +
  facet_wrap(~Period + Site) +
  labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")

#Hisogram for one year
NDVI_2013 <- NDVI_Full_BA %>% filter(Year=="2013", Month == "8", Site == "Grassy_Creek")
  
hist(NDVI_2013$NDVI)

str(NDVI_2013)

shapiro.test(NDVI_2013$NDVI)


#### Data Structure and Relationships ####
#testing for autocorrelation

