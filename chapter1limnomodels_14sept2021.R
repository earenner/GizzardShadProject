rm(list=ls())
#############################################
setwd("C:/Users/earenner/Dropbox/Shad/RcodeData")
### CODE FOR CHAPTER 1: LINEAR MIXED MODELS EVALUATING RELATIONSHIPS BETWEEN GIZZARD SHAD DENSITY AND LIMNOLOGICAL PARAMETERS
#WHAT's THE QUESTION? Our objective was to test for associations between gizzard shad densities and limnological parameters (zooplankton density, algal density, and TN & TP mg/L).
#install.packages("Rokemon")
#install.packages("devtools")
#devtools::install_github("schochastics/Rokemon")
#install.packages("palettetown") #to make the plots in Pokemon colors
#pokedex(cb = 1) *pokemon = 1 is for Bulbasaur colors #GOTTA PLOT 'EM ALL!!! LOL THIS IS HOW I MICRODOSE JOY THESE DAYS
#################################################################################################################
#Load libraries
library(devtools)
library(palettetown)
library(Rokemon)
library(lme4)
library(lmerTest)
library(lattice)
library(ggplot2)
library(reshape2)
library(patchwork)
library(cowplot)
library(pscl)
library(vegan)
library(BiodiversityR)
library(car)
library(Hmisc)
library(magick)
library(scales)
library(ggpubr)
library(gridExtra)
library(ggfortify)
library(dplyr)
library(raster)
library(tidyverse)
library(MuMIn)

library(help = "MuMIn")

### LOAD DATA, WRANGLE DATAFRAMES AND ASSIGN LEVELS TO MONTHS
####################################################################################
#ALGAE DATA VS SHAD - 
shadalgae<-read.csv(file="summary_algae.csv", header = TRUE, sep = ",")
shadalgae$month<-factor(shadalgae$month, levels = c("JUN", "AUG", "OCT"))

#Kitchen sink goliath site x factor df for literally everything - zoops, algae, nutrients, shad densities, morphometrics, you name it
everything_expanded<-read.csv(file="everything_expanded.csv", header = TRUE, sep = ",")
everything_expandednona<-na.omit(everything_expanded)
everything_expandednona$month<-factor(everything_expandednona$month, levels = c("JUN", "AUG", "OCT"))

#READ IN NUTRIENT DATA - NUTRIENT CONCENTRATIONS SITE x SHAD CPUE
nutrientclean<-read.csv(file="NutrientClean.csv", header = TRUE, sep = ",", row.names = NULL)
nutrientclean$month<-factor(nutrientclean$month, levels = c("JUNE", "AUG", "OCT"))
junenutrient <- subset(nutrientclean, month == "JUNE")
augnutrient <- subset(nutrientclean, month == "AUG")
octnutrient <- subset(nutrientclean, month == "OCT")

#attach things if needed, assign factors, subset stuff
attach(everything_expandednona)
attach(shadalgae)
shadalgae$month<-factor(shadalgae$month, levels = c("JUN", "AUG", "OCT"))
as.factor(lakecode)
as.factor(month)
as.factor(year)

##########################  NUTRIENTS VS SHAD   ################################################
attach(nutrientclean)
as.factor(nutrientclean$lakecode)
as.factor(nutrientclean$month)
as.factor(nutrientclean$year)

#R2 correlations
cor(everything_expanded$kdwpt, everything_expanded$shadpermin,  method = "pearson", use = "complete.obs")
shadcatchcomparison<-ggplot(everything_expanded, aes(x=kdwpt, y=shadpermin))+
  geom_point(size=1)+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  labs(y="KSU shad per minute", x="Gizzard Shad Density CPE KDWPT")+
  geom_smooth(method="lm", se=FALSE, show.legend = TRUE)
shadcatchcomparison

df <- data.frame(everything_expanded$TN, everything_expanded$wshedpercentag)
cor(everything_expanded$TN, everything_expanded$wshedpercentag,  method = "pearson", use = "complete.obs")
cor(everything_expanded$TN, everything_expanded$shadpermin,  method = "pearson", use = "complete.obs")
cor(everything_expanded$TP, everything_expanded$wshedpercentag,  method = "pearson", use = "complete.obs")
cor(everything_expanded$TP, everything_expanded$shadpermin,  method = "pearson", use = "complete.obs")
cor(everything_expanded$TotAlgae, everything_expanded$wshedpercentag,  method = "pearson", use = "complete.obs")
cor(everything_expanded$TotAlgae, everything_expanded$shadpermin,  method = "pearson", use = "complete.obs")
cor(everything_expanded$BG, everything_expanded$wshedpercentag,  method = "pearson", use = "complete.obs")
cor(everything_expanded$BG, everything_expanded$shadpermin,  method = "pearson", use = "complete.obs")

########### TN to TP ratio x SHAD PER MINUTE ELECTROFISHING WITH REGRESSION LINES FOR EACH MONTH ####################################################
mod0<-lmer(sqrt(NPratio) ~ log(shadpermin + 0.05) * month * (1|lakecode), data=everything_expandednona) 
anova(mod0)
summary(mod0)
confint(mod0)
plot(mod0)
r.squaredGLMM(mod0)
NPratioplot <- ggplot(everything_expandednona, aes(x = log(shadpermin + 0.05), y = sqrt(NPratio), colour = month)) +
  theme_cowplot(12)+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  scale_colour_poke(pokemon = 1)+
  labs(y="N:P ratio", x="Log(Gizzard Shad Per Minute + 0.05)")
NPratioplot
########### BLUE GREENS x SHAD PER MINUTE ELECTROFISHING WITH REGRESSION LINES FOR EACH MONTH ####################################################
mod1<-lmer(log(BG + 0.01) ~ log(shadpermin + 0.05) + month + (1|lakecode), data=shadalgae) #interactive model
anova(mod1) #interaction is not significant so we don't include it
summary(mod1)
confint(mod1)
plot(mod1)
r.squaredGLMM(mod1)

mod1A<-lmer(log(BG + 0.01) ~ log(shadpermin + 0.05) + month + (1|lakecode), data=shadalgae) #additive model
anova(mod1A) #again, no significant interaction but a marginally significant fixed effect for the month of August
summary(mod1A)
confint(mod1A)
plot(mod1A)
r.squaredGLMM(mod1A)

BGPlot <- ggplot(shadalgae, aes(x = log(shadpermin + 0.01), y = log(BG), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  theme(legend.position = "none")+
  labs(y="Log(Blue Green Algae Grid Count)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  theme(legend.position = "none")
BGPlot

BGbox <- ggplot(shadalgae, aes(month, log(BG + 0.05))) +
  geom_boxplot(aes(group = month)) +
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  geom_point(size = 2,  aes(colour = factor(month))) 
BGbox
#################### DIATOMS x SHAD CPUE ######################################################################
mod2<-lmer(log(D + 0.01) ~ log(shadpermin + 0.01) + month + (1|shadalgae$lakecode), data=shadalgae) 
anova(mod2)
summary(mod2)
confint(mod2)
plot(mod2)
r.squaredGLMM(mod2)
DiatomPlot <- ggplot(shadalgae, aes(x = log(shadpermin + 0.05), y = log(D), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  theme(legend.position = "none")+
  labs(y="Log(Diatom Grid Count)", x="Log(Gizzard Shad Per Minute + 0.05)")
DiatomPlot

Diatombox <- ggplot(shadalgae, aes(month, log(D))) +
  geom_boxplot(aes(group = month)) +
  theme_cowplot(12)+
  geom_jitter()+
  scale_colour_poke(pokemon = 1)+
  geom_point(size = 2,  aes(colour = factor(month))) 
Diatombox
################################ GREEN ALGAE x SHAD CPUE ###################################################################
mod3<-lmer(log(G + 1) ~ log(shadpermin + 0.01) + month + (1|lakecode), data=shadalgae) 
anova(mod3)
summary(mod3)
plot(mod3)
confint(mod3)
r.squaredGLMM(mod3)
GreenPlot <- ggplot(shadalgae, aes(x = log(shadpermin + 0.01), y = log(G), colour = month)) +
  theme_cowplot(12)+
  geom_point()+
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 1)+
  labs(y="Log(Green Algae Grid Count)", x="Log(Gizzard Shad Per Minute + 0.05)")
GreenPlot

Greenbox <- ggplot(shadalgae, aes(month, log(D))) +
  geom_boxplot(aes(group = month)) +
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  geom_point(size = 2,  aes(colour = factor(month))) 
Greenbox

################################ DINOFLAGELLATE ALGAE x SHAD CPUE ###################################################################
mod3A<-lmer(log(DINO + 1) ~ log(shadpermin + 0.01) + month + (1|lakecode), data=shadalgae) 
anova(mod3A)
summary(mod3A)
plot(mod3A)
confint(mod3)
r.squaredGLMM(mod3)
DinoPlot <- ggplot(shadalgae, aes(x = log(shadpermin + 0.01), y = log(DINO + 1), colour = month)) +
  theme_cowplot(12)+
  geom_point()+
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 1)+
  labs(y="Log(Green Algae Grid Count)", x="Log(Gizzard Shad Per Minute + 0.05)")
DinoPlot

Dinobox <- ggplot(shadalgae, aes(month, log(DINO))) +
  geom_boxplot(aes(group = month)) +
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  geom_point(size = 2,  aes(colour = factor(month))) 
Dinobox

############################# TOTAL ALGAL COUNT x SHAD CPUE #########################################################################
mod4<-lmer(log(TOTAL + 1) ~ log(shadpermin + 0.05) + month + (1|lakecode), data=shadalgae) #interaction was not significant
anova(mod4)
summary(mod4)
plot(mod4)
confint(mod4)
r.squaredGLMM(mod4)
TotAlgaePlot <- ggplot(shadalgae, aes(x = log(shadpermin + 0.05), y = log(TOTAL + 1), colour = month)) +
  geom_point()+
  theme(legend.position = "none")+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  labs(y="Log(Total Algal Grid Count)", x="Log(Gizzard Shad Per Minute + 0.05)")
TotAlgaePlot

TotAlgaebox <- ggplot(shadalgae, aes(month, log(TOTAL + 1))) +
  geom_boxplot(aes(group = month)) +
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  geom_point(size = 2,  aes(colour = factor(month))) 
TotAlgaebox

############ TN x SHAD CPUE ####################################
mod6<-lmer(log(TN + 0.01) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(mod6)
summary(mod6)
plot(mod6)
confint(mod6)
r.squaredGLMM(mod6)

mod6<-lmer(log(everything_expanded$TN + 0.01) ~ log(everything_expanded$shadpermin + 0.05) * everything$month + (1|everything$lakecode)) 
anova(mod6)
summary(mod6)
plot(mod6)
r.squaredGLMM(mod6)

TNplotmod <- ggplot(nutrientclean, aes(x = log(shadpermin + 0.05), y = log(TN + 0.01), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+
  geom_jitter(width = 0.25)+
  theme(legend.position = "none")+
  labs(y="Log(Total Nitrogen ppm)", x="Log(Gizzard Shad Per Minute + 0.05)")+ #need to create new variable called shad jitter
  geom_smooth(method="lm",se=FALSE)
TNplotmod

mod7<-glmer(sqrt(TN) ~ log(shadpermin + 0.01) * month + (1|lakecode), family = poisson())
anova(mod7)
summary(mod7)
plot(mod7)

#TP x SHAD CPUE
mod7<-lmer(log(TP + 0.01) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(mod7)
summary(mod7)
confint(mod7)
r.squaredGLMM(mod7)
plot(mod7)

#this jittered nicely now###############################
TPplotmod <- ggplot(nutrientclean, aes(x = log(shadpermin + 0.05), y = log(TP + 0.01), colour = month)) +
  geom_jitter(width = 0.5, height = 0)+
  labs(y="Log(Total Phosphorus ppm)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)
TPplotmod
##################  SECCHI DEPTH VS SHAD CPUE  ########################
#Site by density matrix for shad and algae
secchi<-read.csv(file="secchiclean.csv", header = TRUE, sep = ",")
# create new dataset without missing data
secchinozero <- na.omit(secchi)
secchinozero$month<-factor(secchinozero$month, levels = c("JUNE", "AUG", "OCT"))
attach(secchinozero)
as.factor(lakecode)
as.factor(month)
as.factor(year)

mod8<-lmer(log(secchinozero$secchi) ~ log(shadpermin + 0.01) + month + (1|lakecode)) 
anova(mod8)
summary(mod8)
plot(mod8)
r.squaredGLMM(mod8)
confint(mod8)

TPplotmod <- ggplot(nutrientclean, aes(x = log(shadpermin + 0.05), y = log(TP + 0.01), colour = month)) +
  geom_jitter(width = 0.5, height = 0)+
  labs(y="Log(Total Phosphorus ppm)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1) #bulbasaur color theme
TPplotmod

SecchiPlot <- ggplot(secchinozero, aes(x = log(shadpermin + 0.01), y = log(secchi), colour = month)) +
  geom_point()+
  labs(y="Log(Secchi Depth (m))", x="Log(Gizzard Shad Per Minute + 0.01)")+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  geom_smooth(method="lm",se=FALSE)
SecchiPlot

####NEXT UP: ZOOPLANKTON
#Site by density matrix for shad and algae
zoop<-read.csv(file="masterzoopcommun.csv", header = TRUE, sep = ",")
# create new dataset without missing data
zoopnozero <- na.omit(zoop)

zoopnozero<-read.csv(file="zoopnozero.csv", header = TRUE, sep = ",")

attach(zoopnozero)
as.factor(lakecode)
as.factor(month)
as.factor(year)

#CLADOCERANS x SHAD CPUE
mod9<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.01) + month + (1|lakecode)) 
anova(mod9)
summary(mod9)
plot(mod9)
xyplot(sqrt(Cladocera)~log(shadpermin + 0.01)|month)
r.squaredGLMM(mod9)
confint(mod9)

CladocPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(Cladocera), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Log(Cladocera Density)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CladocPlot
#CALANOIDS x SHAD CPUE
mod10<-lmer(sqrt(Calanoida) ~ log(shadpermin + 0.01) + month + (1|lakecode)) 
anova(mod10)
summary(mod10)
plot(mod10)
xyplot(sqrt(Calanoida)~log(shadpermin + 0.01)|month)
r.squaredGLMM(mod10)
confint(mod10)

CalanPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(Calanoida), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Log(Calanoid Density)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CalanPlot

#CYCLOPOIDS x SHAD CPUE
mod11<-lmer(sqrt(Cyclopoida) ~ log(shadpermin + 0.01) + month + (1|lakecode)) 
anova(mod11)
summary(mod11)
plot(mod11)
r.squaredGLMM(mod11)
confint(mod11)

CyclopoidPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(Cyclopoida), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Log(Cyclopoid Density)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CyclopoidPlot

#ROTIEFERS x SHAD CPUE
mod12<-lmer(sqrt(Rotifera) ~ log(shadpermin + 0.01) + month + (1|lakecode)) 
anova(mod12)
summary(mod12)
plot(mod12)
r.squaredGLMM(mod12)
confint(mod12)

RotiferPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(Rotifera), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Log(Rotifer Density)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
RotiferPlot

#NAUPLII x SHAD CPUE
mod13<-lmer(sqrt(Nauplii) ~ log(shadpermin + 0.01) * month + (1|lakecode)) 
anova(mod13)
summary(mod13)
plot(mod13)
r.squaredGLMM(mod13)
xyplot(sqrt(Nauplii)~log(shadpermin + 0.01)|month)

NaupliiPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(Nauplii), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Log(Nauplii Density)", x="Log(Gizzard Shad Per Minute + 0.05)")
NaupliiPlot

#Naupliibar<-ggplot(zoopnozero, aes(x = lakecode, y = sqrt(Nauplii)))+
  #geom_boxplot()
#Naupliibar

#TOTAL ZOOPLANKTON x SHAD CPUE
mod14<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) + month + (1|lakecode))
anova(mod14)
summary(mod14)
plot(mod14)
r.squaredGLMM(mod14)
confint(mod14)

TotZoopPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(TotZoop), colour = month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Log(Total Zooplankton Density)", x="Log(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
TotZoopPlot

#################### Combine plots using cowplot #########################################################
plot_grid(BGPlot,GreenPlot, DiatomPlot, TotAlgaePlot, nrow = 2, ncol = 2, align = "hv") #MULTIPANEL ALGAE
plot_grid(BGPlot,GreenPlot, DiatomPlot, TotAlgaePlot, labels = c('A', 'B', 'C', 'D'), label_size = 12) #MULTIPANEL ALGAE BOXPLOT
plot_grid(TNplotmod, TPplotmod, NPratioplot, nrow = 1, label_size = 12) #MULTIPANEL TN
plot_grid(Cladocplotmod, Rotifplotmod, TotZoopplotmod, labels = c('A', 'B', 'C'), label_size = 12) #MULTIPANEL ZOOPS
plot_grid(BGPlot, TotPlot)

plot_grid(TNplotmod, TPplotmod)

plot_grid(CladocPlot, CalanPlot, CyclopoidPlot, RotiferPlot, NaupliiPlot, TotZoopPlot)
