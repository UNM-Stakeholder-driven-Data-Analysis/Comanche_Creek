#### Read Me ####

#The purpose of this code is to create a plot to visualize how NDVI is changing over time in the control and impact reaches during the study period at both the full watershed and buffered wetland area levels. I will create a full time series plot and box plots. 

#### Libraries ####
library(ggplot2)
library(dplyr)
library(tidyverse)

#### Load Data ####
NDVI_byplot_Buff <- read.csv("data/processed/FullBuf_Dataframe.csv")

NDVI_byplot_Full <- read.csv("data/processed/Full_Dataframe.csv")

#### Data Wrangling ####

#create new factor for before after period and control and impact treatments
NDVI_byplot_Buff <- NDVI_byplot_Buff %>% 
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
NDVI_byplot_Buff <- NDVI_byplot_Buff %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                                 `site` == "NoName_Creek" ~ "Control"))

#to make a plot showing change over time, we need to add a date column
NDVI_byplot_Buff$date = as.Date(paste(NDVI_byplot_Buff$year, NDVI_byplot_Buff$month, "01", sep="-")) #data is monthly, so this assigns a "day", in this case the first of the month, so that it can be a date object

#summarize by cell by taking the means
NDVI_Buff_mns = NDVI_byplot_Buff %>%
  select(treatment, date, NDVI)%>%
  group_by(treatment, date)%>%
  summarise(NDVI_mn = mean(NDVI, na.rm = T))

#add a new column to identify that this dataframe is for the buffered area
NDVI_Buff_mns$scale = "buffer"

### repeat for watershed-level ###

#create new factor for before after period and control and impact treatments
NDVI_byplot_Full <- NDVI_byplot_Full %>% 
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
NDVI_byplot_Full <- NDVI_byplot_Full %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                                 `site` == "NoName_Creek" ~ "Control"))
#to make a plot showing change over time, we need to add a date column
NDVI_byplot_Full$date = as.Date(paste(NDVI_byplot_Full$year, NDVI_byplot_Full$month, "01", sep="-")) #data is monthly, so this assigns a "day", in this case the first of the month, so that it can be a date object

#summarize by cell
NDVI_Full_mns = NDVI_byplot_Full %>%
  select(treatment, date, NDVI)%>%
  group_by(treatment, date)%>%
  summarise(NDVI_mn = mean(NDVI, na.rm = T))

#add a new column to identify that this dataframe is for the buffered area
NDVI_Full_mns$scale = "watershed"

#make a dataframe with both buffer and watershed level
NDVI_mns_All <- rbind(NDVI_Buff_mns,NDVI_Full_mns)

NDVI_mns_All$scale <- factor(NDVI_mns_All$scale, levels = c("watershed", "buffer"),
                  labels = c("Whole Watershed", "Wetland Areas"))

summary(NDVI_mns_All)

#make the plot

NDVI_ts= 
  ggplot(data=NDVI_mns_All, aes(x=date, y=NDVI_mn, group = treatment, color = treatment))+
  geom_point() + geom_path()+
  facet_wrap(~scale, ncol =1) +
  ylab("NDVI") + xlab("Year") + theme(legend.title = element_blank()) +
  geom_vline(xintercept = as.Date("2014-07-01"), color="red", lty=2)+
  theme_classic()
plot(NDVI_ts)

#### Box Plots ####

## Buffer Area ##
NDVI_byplot_Buff$period<-factor(NDVI_byplot_Buff$period, levels = c("Before", "After"))

p1 <- ggplot(data=NDVI_byplot_Buff, aes(x=period, y=NDVI, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment,)

plot(p1)

## Full Watershed ##
NDVI_byplot_Full$period<-factor(NDVI_byplot_Full$period, levels = c("Before", "After"))

p2 <- ggplot(data=NDVI_byplot_Full, aes(x=period, y=NDVI, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment)

plot(p2)


