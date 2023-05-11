#### Read Me ####

#The purpose of this code is to calculate the BACI effect size, run a linear model to determine the significance of the effect size, and create plots to visualize the BACI interaction

#### Libraries ####

library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)

#### Load Data ####

NDVI_byplot_Buff <- read.csv("data/processed/FullBuf_Dataframe.csv")

NDVI_byplot_Full <- read.csv("data/processed/Full_Dataframe.csv")

#### Arrange Buffer Data ####

#Create new variables for control and impact sites and before and after periods
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

#filter data so you can take averages of NDVI in Impact After, Control After, Impact Before and Control Before

NDVI_ImpactAfter <- NDVI_byplot_Buff %>% filter(treatment=="Impact", period == "After")

NDVI_ControlAfter <- NDVI_byplot_Buff %>% filter(treatment=="Control", period == "After")

NDVI_ImpactBefore <- NDVI_byplot_Buff %>% filter(treatment=="Impact", period == "Before")

NDVI_ControlBefore <- NDVI_byplot_Buff %>% filter(treatment=="Control", period == "Before")

(mean(NDVI_ImpactAfter$NDVI) - mean(NDVI_ControlAfter$NDVI)) - (mean(NDVI_ImpactBefore$NDVI) - mean(NDVI_ControlBefore$NDVI))

### BACI interaction effect is -0.0128677 ###

#calculate means and sd for NDVI
NDVI_byplot.means <- NDVI_byplot_Buff %>%
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

#### Change "After" Period  to 2019-2022 ####

NDVI_2 <- NDVI_byplot_Buff %>% 
  mutate("period" = case_when(`year` == "2010" ~ "Before",
                              `year` == "2011" ~ "Before",
                              `year` == "2012" ~ "Before",
                              `year` == "2013" ~ "Before",
                              `year` == "2019" ~ "After",
                              `year` == "2020" ~ "After",
                              `year` == "2021" ~ "After",
                              `year` == "2022" ~ "After"))
NDVI_2 <- NDVI_2 %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                                 `site` == "NoName_Creek" ~ "Control"))
NDVI_2 <- NDVI_2 %>% drop_na()

#filter data so you can take averages of NDVI in Impact After, Control After, Impact Before and Control Before

NDVI_ImpactAfter <- NDVI_2 %>% filter(treatment=="Impact", period == "After")

NDVI_ControlAfter <- NDVI_2 %>% filter(treatment=="Control", period == "After")

NDVI_ImpactBefore <- NDVI_2 %>% filter(treatment=="Impact", period == "Before")

NDVI_ControlBefore <- NDVI_2 %>% filter(treatment=="Control", period == "Before")

(mean(NDVI_ImpactAfter$NDVI) - mean(NDVI_ControlAfter$NDVI)) - (mean(NDVI_ImpactBefore$NDVI) - mean(NDVI_ControlBefore$NDVI))

#BACI interaction effect is -0.000245953

#calculate means and sd for NDVI
NDVI_2.means <- NDVI_2 %>%
  group_by(site, period, treatment) %>%
  summarise_at(.vars = vars(NDVI),
               .funs = c(mean="mean", sd="sd"), na.rm = TRUE)

#make sure before comes first in plot
NDVI_2.means$period<-factor(NDVI_2.means$period, levels = c("Before", "After"))

#make the BACI plot

baci.plot.NDVI <- ggplot(NDVI_2.means, aes(x=period, y=mean, group=treatment, color = treatment))+
  geom_point(size = 4)+
  geom_smooth(method = lm, size = 1.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width = .1), position = position_dodge(.03))+
  theme_bw()+
  theme(text = element_text(size = 20))+
  ylab("NDVI")

baci.plot.NDVI

#run mutli-level model to determine if effect size is significant
lmer.TotalNDVI <-lmer(NDVI ~ period + treatment + period:treatment + (1|year), data = NDVI_2)
anova(lmer.TotalNDVI)

#p=0.8871 <- not significant 
#treatment is sig, period is not, interaction is not
#still no significant interaction effect when using later "after" period

#### New Models for Buffer ####

#run multi-level model to determine if effect size is significant
lmer.TotalNDVI <-lmer(NDVI ~ period + treatment + period:treatment + (1|year), data = NDVI_byplot_Buff)
anova(lmer.TotalNDVI)
#the model above doesn't account for spatial autocorrelation and so gives a false significant p-value

#let's try a different model
#ran with the "ID" column which is the unique identifier for each cell
lmer.TotalNDVI <-lmer(NDVI ~ period + treatment + period:treatment  + (1|site/ID), data = NDVI_byplot_Buff)
anova(lmer.TotalNDVI)
summary(lmer.TotalNDVI)
acf(resid(lmer.TotalNDVI))
#did not converge (had one negative eigenvalue)

#new approach
# fit simple linear model on data with means of cells
#add date col
NDVI_byplot_Buff$date = as.Date(paste(NDVI_byplot_Buff$year, NDVI_byplot_Buff$month, "01", sep="-"))

NDVI_byplot_Buff$period<-factor(NDVI_byplot_Buff$period, levels = c("Before", "After"))

#summarize by cell
NDVI_byplot_mns = NDVI_byplot_Buff %>%
  select(period, treatment, date, NDVI)%>%
  group_by(period, treatment, date)%>%
  summarise(NDVI_mn = mean(NDVI, na.rm = T))
#fit linear model
lm.TotalNDVI <-lm(NDVI_mn ~ period + treatment + period:treatment, data = NDVI_byplot_mns)
#check for temporal autocorrelation
acf(resid(lm.TotalNDVI))
#view summary
anova(lm.TotalNDVI)
summary(lm.TotalNDVI)

### p=0.6668 ###

#### Repeat for Full Area ####

#Create new variables for control and impact sites and before and after periods
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

#filter data so you can take averages of NDVI in Impact After, Control After, Impact Before and Control Before

NDVI_ImpactAfter <- NDVI_byplot_Full %>% filter(treatment=="Impact", period == "After")

NDVI_ControlAfter <- NDVI_byplot_Full %>% filter(treatment=="Control", period == "After")

NDVI_ImpactBefore <- NDVI_byplot_Full %>% filter(treatment=="Impact", period == "Before")

NDVI_ControlBefore <- NDVI_byplot_Full %>% filter(treatment=="Control", period == "Before")

(mean(NDVI_ImpactAfter$NDVI) - mean(NDVI_ControlAfter$NDVI)) - (mean(NDVI_ImpactBefore$NDVI) - mean(NDVI_ControlBefore$NDVI))

### BACI interaction effect is -0.0206697 ###

#calculate means and sd for NDVI
NDVI_byplot.means <- NDVI_byplot_Full %>%
  group_by(site, period, treatment) %>%
  summarise_at(.vars = vars(NDVI),
               .funs = c(mean="mean", sd="sd"), na.rm = TRUE)

#make sure before comes first in plot
NDVI_byplot.means$period<-factor(NDVI_byplot.means$period, levels = c("Before", "After"))

### make the BACI plot ###

baci.plot.NDVI <- ggplot(NDVI_byplot.means, aes(x=period, y=mean, group=treatment, color = treatment))+
  geom_point(size = 4)+
  geom_smooth(method = lm, size = 1.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width = .1), position = position_dodge(.03))+
  theme_bw()+
  theme(text = element_text(size = 20))+
  ylab("NDVI")

baci.plot.NDVI

### new linear model using means for full area ### 

# fit simple linear model on data with means of cells
#add date col
NDVI_byplot_Full$date = as.Date(paste(NDVI_byplot_Full$year, NDVI_byplot_Full$month, "01", sep="-"))

NDVI_byplot_Full$period<-factor(NDVI_byplot_Full$period, levels = c("Before", "After"))

#summarize by cell
NDVI_byplot_Full_mns = NDVI_byplot_Full %>%
  select(period, treatment, date, NDVI)%>%
  group_by(period, treatment, date)%>%
  summarise(NDVI_mn = mean(NDVI, na.rm = T))
#fit linear model
lm.TotalNDVI <-lm(NDVI_mn ~ period + treatment + period:treatment, data = NDVI_byplot_Full_mns)
# check for temporal autocorrelation
acf(resid(lm.TotalNDVI))
# view summary
anova(lm.TotalNDVI)
summary(lm.TotalNDVI)


