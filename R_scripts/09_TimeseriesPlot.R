#### Read Me ####
#The purpose of this code is to create a plot to visualize how NDVI is changing over time in the control and impact reaches during the study period at both the full watershed and buffered wetland area levels

#### Libraries ####
library(ggplot2)
library(dplyr)
library(tidyverse)

#### Load Data ####
NDVI_byplot_Buff <- read.csv("data/processed/FullBuf_Dataframe.csv")

NDVI_byplot_Full <- read.csv("data/processed/Full_Dataframe.csv")

#### Data Wrangling ####

#create new factor for control and impact
NDVI_byplot_Buff <- NDVI_byplot_Buff %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                                 `site` == "NoName_Creek" ~ "Control"))
#to make a plot showing change over time, we need to add a date column
NDVI_byplot_Buff$date = as.Date(paste(NDVI_byplot_Buff$year, NDVI_byplot_Buff$month, "01", sep="-")) #data is monthly, so this assigns a "day", in this case the first of the month, so that it can be a date object

#summarize by cell
NDVI_Buff_mns = NDVI_byplot_Buff %>%
  select(treatment, date, NDVI)%>%
  group_by(treatment, date)%>%
  summarise(NDVI_mn = mean(NDVI, na.rm = T))

#add a new column to identify that this dataframe is for the buffered area
NDVI_Buff_mns$scale = "buffer"

### repeat for watershed-level ###

#create new factor for control and impact
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
