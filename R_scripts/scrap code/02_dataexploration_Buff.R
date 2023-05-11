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
library(raster)
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
library(car)

#### Load Data ####

NDVI_Full <- read.csv("data/processed/FullBuff_Dataframe.csv", header = T, stringsAsFactors = T)

#tidy data- make the before and after periods in the dataframe
#"before" (2005-2013) and "after" (2013-2022)

NDVI_Full_BA <- NDVI_Full %>% 
  mutate("Period" = case_when(`Year` == "2010" ~ "Before",
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

#make dataframe with full date
NDVI_Full_BA$Day <- c(15)

NDVI_Full_BA$new_date <- as.Date(paste(NDVI_Full_BA$Year,NDVI_Full_BA$Month, NDVI_Full_BA$Day,
                               sep = "-" ), format = "%Y-%m-%d")

str(NDVI_Full_BA$new_date,na.rm=T)


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



##Check for temporal autocorrelation##

#going to check one site at a time
#Average observations within the same month:
dat_monthly = 
  NDVI_Full_BA %>%
  mutate(yr = lubridate::year(new_date)) %>%
  mutate(mo = lubridate::month(new_date)) %>%
  dplyr::select(Site, yr, mo, NDVI) %>%
  group_by(Site, yr, mo) %>%
  summarise(Value.mn = mean(NDVI, aQna.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))

#Average observations within the same year:

dat_yearly = 
  NDVI_Full_BA %>%
  mutate(yr = lubridate::year(new_date)) %>%
  mutate(mo = lubridate::month(new_date)) %>%
  dplyr::select(Site, yr, NDVI) %>%
  group_by(Site, yr) %>%
  summarise(Value.mn = mean(NDVI, aQna.rm = T)) %>%
  mutate(date = paste(yr)) 


#subset data into one site
temp = dat_yearly[dat_yearly$Site == "NoName_Creek",]

### make this a time series object
#make sure data are ordered by date
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(2010), max(2022), by = "1 year"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)

#this didn't work for me so I'm just going to fix it in Excel for now

write.csv(temp, "temp_dates_NoName.csv")
temp_dates <- read.csv("data/processed/temp_dates_NoName.csv", header = T, stringsAsFactors = T)
temp_ts <- temp_dates

head (temp_ts)

#convert to a ts object 
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=2, start=c(2005, 7))
temp_ts = ts(temp_ts$Value.mn, frequency=1, start=c(2010))

# check that you specified the ts correctly
print(temp_ts, calendar = T) 

### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) # filling in NAs with likely points
forecast::Acf(temp_ts, na.action = na.contiguous) #does longest contigous section
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

## modeling?? ##

model.NDVI<-lm(NDVI~new_date,data=NDVI_Full_BA)
summary(model.NDVI)
# test model assumptions
# homogeneity of variances 
plot(fitted(model.NDVI),resid(model.NDVI)) # should not show a pattern in the residuals vs. fitted plot
# normality of residuals
qqnorm(resid(model.NDVI)) # should be a straight line, or close to it

## ANOVA ##
###### ADVANCED - do a statistical Analysis of variance (ANOVA)
###### to test differences in RECO_dayint among seasons
# Question: 
# linear statistical model using a factor for x in y~x rather than continuous variable

model.Period<-lm(NDVI~Period,data=NDVI_Full_BA)
plot(fitted(model.Period), res)

length(NDVI_Full_BA$NDVI)
length(NDVI_Full_BA$Period)
# get results from model

# select Target attribute and 
# Predictor attribute
Y<- NDVI_Full_BA[,"NDVI"] 
X<- NDVI_Full_BA[,"new_date"]

# fit a regression model
model <- lm(Y~X)

# get list of residuals 
res <- resid(model)
res

# produce residual vs. fitted plot
plot(fitted(model), res)

# add a horizontal line at 0 
abline(0,0)

# create Q-Q plot for residuals
qqnorm(res)

# add a straight diagonal line 
# to the plot
qqline(res)

plot(density(res))
###ANOVA

model.Period2 <- lm( I(NDVI * 1e6) ~ Site * Period, data=NDVI_Full_BA)
library(car)
Anova( model.Period2, type=2)

### timeplot

timeplot_period<-ggplot(data=NDVI_Full_BA, aes(x=new_date, y=NDVI))+ 
  facet_wrap(vars(Period))+
  geom_point()+ 
  #the next line adds a LINEAR trendline (y~(read: as function of) x)
  geom_smooth(method="loess",se=TRUE,colour="black",fullrange=FALSE,size=0.5)+ 
  theme_bw() + 
  ylab("NDVI")+
  xlab("Time")
timeplot_period


