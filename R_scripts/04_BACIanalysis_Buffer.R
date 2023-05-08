#### Read Me ####

#### Libraries ####

library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)

#### Load Data ####

NDVI_byplot <- read.csv("data/processed/FullBuf_Dataframe.csv")

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

#BACI interaction effect is -0.0128677

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

lmer.TotalNDVI <-lmer(NDVI ~ period + treatment + period:treatment + (1|treatment/period), data = NDVI_byplot)

#visreg::visreg(lmer.TotalNDVI, "treatment",by="period")
#p<2e-16

#is there spatial or temporal relatedness that isn't fully accounted for by those terms

#### Box plot whole area ####

NDVI_byplot$period<-factor(NDVI_byplot$period, levels = c("Before", "After"))

boxplot(NDVI~period,data=NDVI_byplot)

p1 <- ggplot(data=NDVI_byplot, aes(x=period, y=NDVI, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment, scale="free")

plot(p1)

hist(NDVI_byplot$NDVI)

#### Change "After" Period  to 2019-2022 ####

NDVI_2 <- NDVI_byplot %>% 
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

#### Box plot whole area ####

NDVI_byplot$period<-factor(NDVI_byplot$period, levels = c("Before", "After"))

boxplot(NDVI~period,data=NDVI_byplot)

p1 <- ggplot(data=NDVI_byplot, aes(x=period, y=NDVI, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment, scale="free")

plot(p1)

hist(NDVI_byplot$NDVI)

#### New Models ####

#run multi-level model to determine if effect size is significant
#ran with the "ID" column which is the unique identifier for each cell
lmer.TotalNDVI <-lmer(NDVI ~ period + treatment + period:treatment  + (1|site/ID), data = NDVI_byplot)
anova(lmer.TotalNDVI)
summary(lmer.TotalNDVI)
acf(resid(lmer.TotalNDVI))

#I ran it with the "cell" column too and got the same results. The extract cell from raster function didn't give unique cell numbers so I think the "ID" column is more accurate. 
lmer.TotalNDVI.cell <-lmer(NDVI ~ period + treatment + period:treatment  + (1|site/cell), data = NDVI_byplot)
anova(lmer.TotalNDVI)
summary(lmer.TotalNDVI)
acf(resid(lmer.TotalNDVI))

#did not converge
# fit simple linear model on data with means of cells
#add date col
NDVI_byplot$date = as.Date(paste(NDVI_byplot$year, NDVI_byplot$month, "01", sep="-"))

NDVI_byplot$period<-factor(NDVI_byplot$period, levels = c("Before", "After"))

#summarize by cell
NDVI_byplot_mns = NDVI_byplot %>%
  select(period, treatment, date, NDVI)%>%
  group_by(period, treatment, date)%>%
  summarise(NDVI_mn = mean(NDVI, na.rm = T))
#fit linear model
lm.TotalNDVI <-lm(NDVI_mn ~ period + treatment + period:treatment, data = NDVI_byplot_mns)
# check for temporal autocorrelation
acf(resid(lm.TotalNDVI))
# view summary
anova(lm.TotalNDVI)
summary(lm.TotalNDVI)

############

