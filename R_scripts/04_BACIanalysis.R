#### Read Me ####

#### Libraries ####

library(ggplot2)
library(dplyr)
library(writexl)
library(FD)
library(data.table)
library(tidyverse)
library(lme4)
library(readxl)
library(ggpubr)
library(rstatix)
library(lmerTest)

#### Load Data ####

NDVI_byplot <- read.csv("data/processed/Full_Dataframe.csv")

#### Arrange Data ####

#Create new variables for control and impact sites and before and after periods
NDVI_byplot <- NDVI_byplot %>% 
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
NDVI_byplot <- NDVI_byplot %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                              `site` == "NoName_Creek" ~ "Control"))

#filter data so you can take averages of NDVI in Impact After, Control After, Impact Before and Control Before

NDVI_ImpactAfter <- NDVI_byplot %>% filter(treatment=="Impact", period == "After")

NDVI_ControlAfter <- NDVI_byplot %>% filter(treatment=="Control", period == "After")

NDVI_ImpactBefore <- NDVI_byplot %>% filter(treatment=="Impact", period == "Before")

NDVI_ControlBefore <- NDVI_byplot %>% filter(treatment=="Control", period == "Before")

(mean(NDVI_ImpactAfter$NDVI) - mean(NDVI_ControlAfter$NDVI)) - (mean(NDVI_ImpactBefore$NDVI) - mean(NDVI_ControlBefore$NDVI))

#BACI interaction effect is -0.0206697

#calculate means and sd for NDVI
NDVI_byplot.means <- NDVI_byplot %>%
  group_by(site, period, treatment) %>%
  summarise_at(.vars = vars(NDVI),
               .funs = c(mean="mean", sd="sd"), na.rm = TRUE)

#make sure before comes first in plot
NDVI_byplot.means$period<-factor(NDVI_byplot.means$period, levels = c("Before", "After"))

#make the BACI plot

baci.plot.NDVI <- ggplot(NDVI_byplot.means, aes(x=period, y=mean, group=treatment, color = treatment))+
  geom_point(size = 4)+
  geom_smooth(method = lm, size = 1.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width = .1), position = position_dodge(.03))+
  theme_bw()+
  theme(text = element_text(size = 20))+
  ylab("NDVI")

baci.plot.NDVI

#run mutli-level model to determine if effect size is significant
lmer.TotalNDVI <-lmer(NDVI ~ period + treatment + period:treatment + (1|year), data = NDVI_byplot)
anova(lmer.TotalNDVI)

#p<2e-16

#### Box plot whole area ####

NDVI_byplot$period<-factor(NDVI_byplot$period, levels = c("Before", "After"))

boxplot(NDVI~period,data=NDVI_byplot)

p1 <- ggplot(data=NDVI_byplot, aes(x=period, y=NDVI, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment, scale="free")

plot(p1)
