#### Read Me ####

#The purpose of this code is to explore the NDVI data extracted from the Landsat 7 and 8 raster files. The results will be used to complete the data exploration assignment. 

#### Libraries ####

library(tmap)
library(raster)
library(reshape2)
library(tidyverse)
library(nortest)
library(tidyverse)
library(lubridate)
library(psych) 
library(car) 
library(tsibble) 
library(forecast) 
library(ape) 
library(ade4)
library(rgdal)
library(car)
library(TSstudio)

#### Load Data ####

NDVI_Full <- read.csv("data/processed/Full_Dataframe.csv", header = T, stringsAsFactors = T)

#### Organize Data ####

#tidy data- make the before and after periods in the dataframe
#"before" (2005-2013) and "after" (2013-2022)
NDVI_Full <- NDVI_Full %>% 
  mutate("period" = case_when(`year` == "2010" ~ "Before",
                              `year` == "2011" ~ "Before",
                              `year` == "2012" ~ "Before",
                              `year` == "2013" ~ "Before",
                              `year` == "2014" ~ "After",
                              `year` == "2015" ~ "After",
                              `year` == "2016" ~ "After",
                              `year` == "2017" ~ "After",
                              `year` == "2018" ~ "After",
                              `year` == "2019" ~ "After",
                              `year` == "2020" ~ "After",
                              `year` == "2021" ~ "After",
                              `year` == "2022" ~ "After"))

NDVI_Full <- NDVI_Full %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                                 `site` == "NoName_Creek" ~ "Control"))
#convert Period from character to factor
NDVI_Full$period <- as.factor(NDVI_Full$period)
class(NDVI_Full$period)

str(NDVI_Full)

#make dataframe with full date
NDVI_Full$day <- c(1)

NDVI_Full$new_date <- as.Date(paste(NDVI_Full$year, NDVI_Full$month, NDVI_Full$day, sep = "-" ), format = "%Y-%m-%d")

str(NDVI_Full$new_date,na.rm=T)


#### Data Types ####
dim(NDVI_Full)

#15 variables and 196398 observations

summary(NDVI_Full) #gives mean, median, min, and max

#data makes sense, nothing looks too out of place

str(NDVI_Full) #shows different data types


#### Data Distribution ####
#let's look at the distribution
hist(NDVI_Full$NDVI)
qqPlot(NDVI_Full$NDVI)

#we can make it look a little nicer
NDVI_Full %>% ggplot(aes(x=NDVI)) + geom_histogram() +
  labs(title="NDVI Histogram All Sites",x="NDVI", y = "Number of observations")

#now let's look at the data by site
NDVI_Full %>% ggplot(aes(x=NDVI)) + 
              geom_histogram() +
              facet_wrap(~site) +
  labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")

#QQ Plot- shows data distributions
temp = NDVI_Full[NDVI_Full$site == "Grassy_Creek",]
qqPlot(temp$NDVI, main = "QQ Plot Grassy Creek")

temp =NDVI_Full[NDVI_Full$site == "NoName_Creek",]
qqPlot(temp$NDVI, main = "QQ Plot NoName Creek")

#Histograms of data Before vs After
NDVI_Full %>% mutate(period = fct_relevel(period, 
                                           "Before", "After"))%>% ggplot(aes(x=NDVI)) + 
                geom_histogram() +
                facet_wrap(~period) +
                labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")

#Histograms for Before and After per site
NDVI_Full %>% mutate(period = fct_relevel(period, 
                                             "Before", "After")) %>% ggplot(aes(x=NDVI)) + 
  geom_histogram() +
  facet_wrap(~period + site) +
  labs(title="NDVI Histogram",x="NDVI", y = "Number of observations")

#Histogram for one year
NDVI_2013 <- NDVI_Full %>% filter(year=="2013", month == "8", site == "Grassy_Creek")
  
hist(NDVI_2013$NDVI)

str(NDVI_2013)


#### Data Structure and Relationships ####
#test for autocorrelation

##Check for temporal autocorrelation##

#going to check one site at a time

#Average observations within the same year:
dat_yearly = 
  NDVI_Full %>%
  mutate(yr = lubridate::year(new_date)) %>%
  mutate(mo = lubridate::month(new_date)) %>%
  dplyr::select(site, yr, NDVI) %>%
  group_by(site, yr) %>%
  summarise(Value.mn = mean(NDVI, aQna.rm = T)) %>%
  mutate(date = paste(yr))

#subset data into one site
temp = dat_yearly[dat_yearly$site == "NoName_Creek",]

### make this a time series object
#make sure data are ordered by date
temp = temp %>% arrange(date) 
#make the spacing of dates consistent and fill in missing obs with NA. I exported temp into excel and added 2021 to the file with an NA, then re-imported 

write.csv(temp, "data/processed/temp_dates_NoName.csv")
temp_dates <- read.csv("data/processed/temp_dates_NoName.csv", header = T, stringsAsFactors = T)
temp_ts <- temp_dates

#convert temp to a time series object 
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=1, start=c(2010))

# check that the time series is in order with all years
print(temp_ts, calendar = T) 

#check for temporal autocorrelation in the time series
# use forecast act function
forecast::Acf(temp_ts, na.action = na.pass) # na.pass fills in NAs with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) # na.contiguous does longest contigous section
forecast::Acf(temp_ts, na.action = na.interp) # na.interp fills in NAs using linear interpolation

#no temporal autocorrelation

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

#create a plot of the time series to show inter-annual variation
ts_plot(temp_ts)


### repeat for Grassy Creek

#subset data into one site
temp = dat_yearly[dat_yearly$site == "Grassy_Creek",]

### make this a time series object
#make sure data are ordered by date
temp = temp %>% arrange(date) 
#make the spacing of dates consistent and fill in missing obs with NA. I exported temp into excel and added 2021 to the file with an NA, then re-imported 

write.csv(temp, "data/processed/temp_dates_Grassy.csv")

#add the correct time series, then re-upload
temp_dates <- read.csv("data/processed/temp_dates_Grassy.csv", header = T, stringsAsFactors = T)
temp_ts <- temp_dates

#convert temp to a time series object 
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=1, start=c(2010))

# check that the time series is in order with all years
print(temp_ts, calendar = T) 

#check for temporal autocorrelation in the time series
# use forecast act function
forecast::Acf(temp_ts, na.action = na.pass) # na.pass fills in NAs with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) # na.contiguous does longest contigous section
forecast::Acf(temp_ts, na.action = na.interp) # na.interp fills in NAs using linear interpolation

#no temporal autocorrelation

forecast::Pacf(temp_ts, na.action = na.pass)
#shows spatial autocorrelation at the 10th lag
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

ts_plot(temp_ts)
