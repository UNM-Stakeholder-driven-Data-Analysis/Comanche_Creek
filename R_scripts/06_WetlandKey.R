library(car)
library(ggplot2)

####Load Data####
NDVI_byplot <- read.csv("data/processed/Full_Dataframe.csv")

MaxNDVI <- NDVI_byplot %>% 
  group_by(year) %>% summarize(max(NDVI))

AvgNDVI <- NDVI_byplot %>%
  group_by(year) %>% summarize(mean(NDVI))

summary(MaxNDVI)

hist(NDVI_byplot, x= NDVI)

plot(MaxNDVI$year,MaxNDVI$"max(NDVI)")
plot(AvgNDVI$year, AvgNDVI$"mean(NDVI)")

str(AvgNDVI)

AvgNDVI$year <- as.numeric(AvgNDVI$year) 
AvgNDVI$NDVI <- AvgNDVI$`mean(NDVI)`

scatter_AvgNDVIAll<-ggplot(data=AvgNDVI, aes(x=year, y=NDVI)+ 
  #the next line adds the points
  geom_point()+ 
  #the next line adds a LINEAR trendline (y~(read: as function of) x)
  geom_smooth(method="lm",formula=y~x,se=TRUE,colour="black",fullrange=FALSE,linewidth=0.5)+ 
  # get rid of gray
  theme_bw() + 
  # label x and y axes
  xlab("Year")+ 
  ylab("NDVI"))
scatter_AvgNDVI # note you have to run the name of the plot to get it to show up in Plots tab on left

#average maximum NDVI is 0.5624

0.5624 * 0.80

#80% of maximum annual NDVI is 0.44992
#wetland areas = 0.44992-1.0

#sensitivity analysis= varying wetland key to be more conservative (higher) vs liberal (lower)
#report how that changed things

WetlandNDVI <- NDVI_byplot %>% 
  filter(NDVI > 0.44992)

str(WetlandNDVI)

#### BACI for Wetland Areas ####
#Create new variables for control and impact sites and before and after periods
WetlandNDVI <- WetlandNDVI %>% 
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
WetlandNDVI <- WetlandNDVI %>% 
  mutate("treatment" = case_when(`site` == "Grassy_Creek" ~ "Impact",
                                 `site` == "NoName_Creek" ~ "Control"))

#filter data so you can take averages of NDVI in Impact After, Control After, Impact Before and Control Before

NDVI_ImpactAfter <- WetlandNDVI %>% filter(treatment=="Impact", period == "After")

NDVI_ControlAfter <- WetlandNDVI %>% filter(treatment=="Control", period == "After")

NDVI_ImpactBefore <- WetlandNDVI %>% filter(treatment=="Impact", period == "Before")

NDVI_ControlBefore <- WetlandNDVI %>% filter(treatment=="Control", period == "Before")

(mean(NDVI_ImpactAfter$NDVI) - mean(NDVI_ControlAfter$NDVI)) - (mean(NDVI_ImpactBefore$NDVI) - mean(NDVI_ControlBefore$NDVI))

#BACI interaction effect is -0.01636


#calculate means and sd for NDVI
WetlandNDVI.means <- WetlandNDVI %>%
  group_by(site, period, treatment) %>%
  summarise_at(.vars = vars(NDVI),
               .funs = c(mean="mean", sd="sd"), na.rm = TRUE)

#make sure before comes first in plot
WetlandNDVI.means$period<-factor(WetlandNDVI.means$period, levels = c("Before", "After"))

#make the BACI plot

baci.plot.NDVI <- ggplot(WetlandNDVI.means, aes(x=period, y=mean, group=treatment, color = treatment))+
  geom_point(size = 4)+
  geom_smooth(method = lm, size = 1.5)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width = .1), position = position_dodge(.03))+
  theme_bw()+
  theme(text = element_text(size = 20))+
  ylab("NDVI") +
  xlab("Period")

baci.plot.NDVI

#### Model ####
lme4:: lmer(NDVI ~ period + treatment + period:treatment + (1|year), data = WetlandNDVI)
#gives us some info about the model

lmer.WetlandNDVI <-lmer(NDVI ~ period + treatment + period:treatment + (1|year), data = WetlandNDVI)

plot(lmer.WetlandNDVI)
#residuals do no have any shape, fits assumption

car::vif(lmer.WetlandNDVI)
#no evidence of multicolinearity (values are below 5), fits assumption

qqnorm(resid(lmer.WetlandNDVI))
qqline(resid(lmer.WetlandNDVI))
#residuals are normally distributed, fits assumption

car::Anova(lmer.WetlandNDVI, type = 3)

anova(lmer.WetlandNDVI)

#p=0.02698

#### Box Plots ####

WetlandNDVI$period<-factor(WetlandNDVI$period, levels = c("Before", "After"))

boxplot(NDVI~period,data=WetlandNDVI)

p1 <- ggplot(data=WetlandNDVI, aes(x=period, y=NDVI, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment, scale="free")

plot(p1)
