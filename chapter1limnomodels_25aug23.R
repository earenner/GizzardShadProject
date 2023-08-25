### LINEAR MIXED MODELS EVALUATING RELATIONSHIPS BETWEEN GIZZARD SHAD DENSITY AND LIMNOLOGICAL PARAMETERS
rm(list=ls())
setwd("C:/Users/gfpr27706/Dropbox/Shad/RcodeData")
getwd()

library(lme4)
library(lmerTest)
library(lattice)
library(ggplot2)
library(cowplot)
library(palettetown)
library(ggpubr)
library(patchwork)
library(MuMIn)
################# NUTRIENTS ##################################################################################
nutrients<-read.csv(file="NutrientCleanRatios_15dec2021.csv", header = TRUE, sep = ",")
attach(nutrients)
as.factor(Impoundment)
as.factor(month)

#SUBSET BY MONTH
nutrients_june <- subset(nutrients, month == "JUNE")
nutrients_aug <- subset(nutrients, month == "AUG")
nutrients_oct <- subset(nutrients, month == "OCT")

######## TOTAL NITROGEN VS SHAD RELATIVE ABUNDANCE #################
TNmod1<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data = nutrients) 
anova(TNmod1)
r.squaredGLMM(TNmod1)
confint(TNmod1)
summary(TNmod1)
plot(TNmod1)

#JUNE TN ONLY
TNmodjune<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_june) 
anova(TNmodjune)
summary(TNmodjune)
confint(TNmodjune)

TNplotjune <- ggplot(nutrients_june, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TN + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  labs(y="Ln(Total Nitrogen ppm + 0.01)", x="") +
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 6)+
  ggtitle('June')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE) 
TNplotjune

#AUG TN ONLY
TNmodaug<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_aug) 
anova(TNmodaug)
summary(TNmodaug)

TNplotaug <- ggplot(nutrients_aug, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TN + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  ggtitle('August')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE) 
TNplotaug

#oct TN only
TNmodoct<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_oct) 
anova(TNmodoct)
summary(TNmodoct)

TNplotoct <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TN + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  ggtitle('October')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE) 
TNplotoct

######## TOTAL PHOSPHORUS VS SHAD RELATIVE ABUNDANCE  #####################

#TP x SHAD CPUE
TPmod1<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data = nutrients) 
anova(TPmod1)
r.squaredGLMM(TPmod1)
confint(TPmod1)
summary(TPmod1)
plot(TPmod1)

#june TP only
modjune<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_june) 
anova(modjune)
summary(modjune)

TPplotjune <- ggplot(nutrients_june, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Total Phosphorus ppm + 0.01)", x="") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplotjune

#aug TP only
modaug<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_aug) 
anova(modaug)
summary(modaug)

TPplotaug <- ggplot(nutrients_aug, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplotaug

#oct TP only
modoct<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_oct) 
anova(modoct)
summary(modoct)

TPplotoct <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplotoct

# combined TP plot across months
TPplot <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Total Phosphorus ppm + 0.01)", x="Ln(Gizzard Shad Per Minute + 0.05)") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplot

########## N:P RATIOS VS SHAD RELATIVE ABUNDANCE ##############################
NPratiomod_ln<-lmer(log(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data=nutrients) 
anova(NPratiomod_ln)
summary(NPratiomod_ln)
confint(NPratiomod_ln)
plot(NPratiomod_ln)
r.squaredGLMM(NPratiomod_ln)

NPratioplot <- ggplot(aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = month), data = nutrients) +
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  labs(y="Sqrt(N:P ratio)", x="Ln(Gizzard Shad Per Minute + 0.05)")
NPratioplot


NPratiomod<-lmer(sqrt(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data=nutrients) 
anova(NPratiomod)
summary(NPratiomod)
confint(NPratiomod)
plot(NPratiomod)
r.squaredGLMM(NPratiomod)
## June N:P ratio # YES SIGNIFICANT
mod0june<-lmer(sqrt(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data=nutrients_june) 
anova(mod0june)
summary(mod0june)
confint(mod0june)
plot(mod0june)
r.squaredGLMM(mod0june)

NPplotjune <- ggplot(nutrients_june, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(N:P Ratio)", x=expression(paste("Ln(Gizzard Shad Min"^-1," + 0.05)"))) +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
NPplotjune

# August N:P ratio # NOT SIGNIFICANT
mod0aug<-lmer(sqrt(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data=nutrients_aug) 
anova(mod0aug)
summary(mod0aug)
confint(mod0aug)
r.squaredGLMM(mod0aug)
plot(mod0aug)

NPplotaug <- ggplot(nutrients_aug, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x=expression(paste("Ln(Gizzard Shad Min"^-1," + 0.05)"))) +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
NPplotaug

### October N:P ratio # YES SIGNIFICANT
mod0oct<-lmer(sqrt(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data=nutrients_oct) 
anova(mod0oct)
summary(mod0oct)
confint(mod0oct)
plot(mod0oct)
r.squaredGLMM(mod0oct)

NPplotoct <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x=expression(paste("Ln(Gizzard Shad Min"^-1," + 0.05)"))) +
         #labs(x = NULL, y = expression(paste("EC"[50]," ","(mmol/L)")))      
  geom_smooth(method="lm",se=FALSE) 
NPplotoct

######
#NPratioplot <- ggplot(aes(x = log(shad_cpe_electrofishing + 0.05), y = sqrt(NPratio), colour = month), data = nutrients) +
  #theme_cowplot(12)+
  #geom_point()+
  #geom_smooth(method="lm",se=FALSE)+
  labs(y="Sqrt(N:P ratio)", x="Ln(Gizzard Shad Per Minute + 0.05)")
#NPratioplot

##########################################################################################################
#################### Combine into multipanel plots for manuscript figures ################################
##########################################################################################################

(TNplotjune | TNplotaug | TNplotoct) / (TPplotjune | TPplotaug | TPplotoct) / (NPplotjune | NPplotaug | NPplotoct ) + plot_layout(guides = "collect")

ggsave(filename = "9panelplot_mod25aug23a.png", dpi = 600)
patchworkplot<-(TNplotjune | TNplotaug | TNplotoct) / (TPplotjune | TPplotaug | TPplotoct) / (NPplotjune | NPplotaug | NPplotoct ) + plot_layout(guides = "collect")
patchworkplot

######### SECCHI ######################
#Site by density matrix for shad and algae
secchi<-read.csv(file="secchiclean.csv", header = TRUE, sep = ",")
# create new dataset without missing data
secchinozero <- na.omit(secchi)
secchinozero$month<-factor(secchinozero$month, levels = c("JUNE", "AUG", "OCT"))

JuneSecchi <- subset(secchinozero, month == "JUNE")
attach(JuneSecchi)
AugSecchi <- subset(secchinozero, month == "AUG")
attach(AugSecchi)
OctSecchi <- subset(secchinozero, month == "OCT")
attach(OctSecchi)

attach(secchinozero)
as.factor(lakecode)
as.factor(month)
as.factor(year)

# OVERALL SECCHI
mod8<-lmer(log(secchinozero$secchi) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(mod8)
summary(mod8)
plot(mod8)
r.squaredGLMM(mod8)
confint(mod8)

#JUNE SECCHI
mod8june<-lmer(log(secchi) ~ log(shadpermin + 0.05) * (1|lakecode), data = JuneSecchi ) 
anova(mod8june)
summary(mod8june)
plot(mod8june)
r.squaredGLMM(mod8june)
confint(mod8june)
#AUG SECCHI
mod8aug<-lmer(log(secchi) ~ log(shadpermin + 0.05) * (1|lakecode), data = AugSecchi ) 
anova(mod8aug)
summary(mod8aug)
plot(mod8aug)
r.squaredGLMM(mod8aug)
confint(mod8aug)
#OCT SECCHI
mod8oct<-lmer(log(secchi) ~ log(shadpermin + 0.05) * (1|lakecode), data = OctSecchi ) 
anova(mod8oct)
summary(mod8oct)
plot(mod8oct)
r.squaredGLMM(mod8oct)
confint(mod8oct)

SecchiPlot <- ggplot(secchinozero, aes(x = log(shadpermin + 0.05), y = log(secchi), colour = month)) +
  geom_point()+
  labs(y="Ln(Secchi Depth (m))", x="Ln(Gizzard Shad Per Minute + 0.01)")+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  geom_smooth(method="lm",se=FALSE)
SecchiPlot

###########################################################################################################
######### ALGAE ######################################################################################
######################################################################################################
shadalgae<-read.csv(file="summary_algae.csv", header = TRUE, sep = ",")
attach(shadalgae)
as.factor(lakecode)
as.factor(Month, levels = c("JUNE", "AUG", "OCT"))

shadalgae$Month<-factor(shadalgae$Month, levels = c("JUNE", "AUG", "OCT"))


shadalgae_nozero <- subset(shadalgae, shad_cpe_electrofishing > 0)

############### CYANOBACTERIA VS SHAD CPUE ##############################################################################

cyanomod<-lmer(log(cyano + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(cyanomod) 
summary(cyanomod)
confint(cyanomod)
r.squaredGLMM(cyanomod)
plot(cyanomod)

mod1a<-lmer(log(cyano + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae_nozero) #interactive model
anova(mod1a)
summary(mod1a)
plot(mod1a)

BGPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.01), y = log(cyano), colour = Month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  theme(legend.position = "none")+
  labs(y="Ln(Cyanobacteria Grid Count)", x="")+
  theme(legend.position = "none")
BGPlot
#################### DIATOMS x SHAD CPUE ######################################################################
diatommod<-lmer(log(diatom + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(diatommod) 
summary(diatommod)
confint(diatommod)
r.squaredGLMM(diatommod)
plot(diatommod)

DiatomPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(diatom), colour = Month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  theme(legend.position = "none")+
  labs(y="Ln(Diatom Grid Count)", x="Ln(Gizzard Shad Per Minute + 0.05)")
DiatomPlot
################################ GREEN ALGAE x SHAD CPUE ###################################################################

greenmod<-lmer(log(green + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(greenmod) #interaction is not significant so we don't include it
summary(greenmod)
confint(greenmod)
r.squaredGLMM(greenmod)
plot(greenmod)

GreenPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.01), y = log(green), colour = Month)) +
  theme_cowplot(12)+
  geom_point()+
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Green Algae Grid Count)", x="")
GreenPlot
################################ DINOFLAGELLATE ALGAE x SHAD CPUE ###################################################################
mod4<-lmer(log(dinoflag + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(mod4) #interaction is not significant so we don't include it
summary(mod4)
plot(mod4)

mod4a<-lmer(log(dinoflag + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae_nozero) #interactive model
anova(mod4a) #interaction is not significant so we don't include it
summary(mod4a)
plot(mod4a)

DinoPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.01), y = log(dinoflag + 1), colour = Month)) +
  theme_cowplot(12)+
  geom_point()+
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Green Algae Grid Count)", x="Ln(Gizzard Shad Per Minute + 0.05)")
DinoPlot

############################# TOTAL ALGAL COUNT x SHAD CPUE #########################################################################
totphytomod<-lmer(log(total_phytoplankton + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(totphytomod) #interaction is not significant so we don't include it
summary(totphytomod)
confint(totphytomod)
r.squaredGLMM(totphytomod)
plot(totphytomod)

TotAlgaePlot <- ggplot(shadalgae, aes(x=log(shad_cpe_electrofishing + 0.05), y = log(total_phytoplankton + 1), colour = Month)) +
  geom_point()+
  theme(legend.position = "none")+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Total Phytoplankton Grid Count)", x="Ln(Gizzard Shad Per Minute + 0.05)")
TotAlgaePlot
######################## MUlTIPANEL PLOTS WITH PATCHWORK ##################
patchworkalgae<-plot_grid(BGPlot,GreenPlot, DiatomPlot, TotAlgaePlot, labels = c('A', 'B', 'C', 'D'), label_size = 12) + plot_layout(guides = "collect")    #MULTIPANEL ALGAE BOXPLOT

(BGPlot | GreenPlot) / (DiatomPlot | TotAlgaePlot ) + plot_layout(guides = "collect")
ggsave(filename = "4panelalgae_mod25aug23.png", dpi = 600)

##############################################################################################
###### ZOOPLANKTON ###########################################################################
###############################################################################################
zoopnozero<-read.csv(file="zoopnozero.csv", header = TRUE, sep = ",")
zoopnozero <- na.omit(zoopnozero)
attach(zoopnozero)
as.factor(lakecode)
as.factor(month)
as.factor(year)

JuneZoop <- subset(zoopnozero, month == "JUNE")
attach(JuneZoop)
AugZoop <- subset(zoopnozero, month == "AUG")
attach(AugZoop)
OctZoop <- subset(zoopnozero, month == "OCT")
attach(OctZoop)

####### #CLADOCERANS x SHAD CPUE # SIGNIFICANT AT 0.1 #######################################
cladocmod<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(cladocmod)
summary(cladocmod)
plot(cladocmod)
r.squaredGLMM(cladocmod)
confint(cladocmod)

## JUNE IS SIGNIFICANT
mod9june<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.05) * (1|lakecode), data = JuneZoop ) 
anova(mod9june)
summary(mod9june)
plot(mod9june)
r.squaredGLMM(mod9june)
confint(mod9june)
## AUG 
mod9aug<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.05) * (1|lakecode), data = AugZoop ) 
anova(mod9aug)
summary(mod9aug)
plot(mod9aug)
r.squaredGLMM(mod9aug)
confint(mod9aug)
# OCT 
mod9aug<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.01) * (1|lakecode), data = OctZoop ) 
anova(mod9aug)
summary(mod9aug)
plot(mod9aug)
r.squaredGLMM(mod9aug)
confint(mod9aug)
#########
CladocPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Cladocera))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Cladocera Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CladocPlot
################## #CALANOIDS x SHAD CPUE ################################################################
calanmod<-lmer(sqrt(Calanoida) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(calanmod)
summary(calanmod)
plot(calanmod)
r.squaredGLMM(calanmod)
confint(calanmod)

CalanPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Calanoida))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Calanoid Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CalanPlot

#CYCLOPOIDS x SHAD CPUE
cyclomod<-lmer(sqrt(Cyclopoida) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(cyclomod)
summary(cyclomod)
plot(cyclomod)
r.squaredGLMM(cyclomod)
confint(cyclomod)

CyclopoidPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Cyclopoida))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Cyclopoid Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CyclopoidPlot

#ROTIEFERS x SHAD CPUE
rotifermod<-lmer(sqrt(Rotifera) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(rotifermod)
summary(rotifermod)
plot(rotifermod)
r.squaredGLMM(rotifermod)
confint(rotifermod)

RotiferPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Rotifera))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Rotifer Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
RotiferPlot

#NAUPLII x SHAD CPUE
naupmod<-lmer(sqrt(Nauplii) ~ log(shadpermin + 0.05) * month + (1|lakecode)) 
anova(naupmod)
summary(naupmod)
confint(naupmod)
plot(naupmod)
r.squaredGLMM(naupmod)

NaupliiPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Nauplii))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+ #bulbasaur color theme
  labs(y="Sqrt(Nauplii Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")
NaupliiPlot

#TOTAL ZOOPLANKTON x SHAD CPUE
totzoopmod<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.05) * month * (1|lakecode))
anova(totzoopmod)
summary(totzoopmod)
plot(totzoopmod)
r.squaredGLMM(totzoopmod)
confint(totzoopmod)

## JUNE IS SIGNIFICANT
mod14june<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) * (1|lakecode), data = JuneEverything ) 
anova(mod14june)
summary(mod14june)
plot(mod14june)
r.squaredGLMM(mod14june)
confint(mod14june)
## AUG 
mod14aug<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) * (1|lakecode), data = AugEverything ) 
anova(mod14aug)
summary(mod14aug)
plot(mod14aug)
r.squaredGLMM(mod14aug)
confint(mod14aug)
# OCT 
mod14oct<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) * (1|lakecode), data = OctEverything ) 
anova(mod14oct)
summary(mod14oct)
plot(mod14oct)
r.squaredGLMM(mod14oct)
confint(mod14oct)

TotZoopPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(TotZoop))) +
  geom_point()+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Ln(Total Zooplankton Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
TotZoopPlot
##########################################################################################################################
####### REDUCED NO ZERO MODELS ############################################################################################
##########################################################################################################################
################# NUTRIENTS ##################################################################################
nutrients<-read.csv(file="NutrientCleanRatios_reduced.csv", header = TRUE, sep = ",")
attach(nutrients)
as.factor(Impoundment)
as.factor(month)

#SUBSET BY MONTH
nutrients_june <- subset(nutrients, month == "JUNE")
nutrients_aug <- subset(nutrients, month == "AUG")
nutrients_oct <- subset(nutrients, month == "OCT")

######## TOTAL NITROGEN VS SHAD RELATIVE ABUNDANCE #################
TNmod1<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data = nutrients) 
anova(TNmod1)
r.squaredGLMM(TNmod1)
confint(TNmod1)
summary(TNmod1)
plot(TNmod1)

#JUNE TN ONLY
TNmodjune<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_june) 
anova(TNmodjune)
summary(TNmodjune)
confint(TNmodjune)

TNplotjune <- ggplot(nutrients_june, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TN + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  labs(y="Ln(Total Nitrogen ppm + 0.01)", x="") +
  scale_colour_poke(pokemon = 6)+
  theme(legend.position = "none")+
  ggtitle('June')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE) 
TNplotjune

#AUG TN ONLY
TNmodaug<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_aug) 
anova(TNmodaug)
summary(TNmodaug)

TNplotaug <- ggplot(nutrients_aug, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TN + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 6)+
  ggtitle('August')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE) 
TNplotaug

#oct TN only
TNmodoct<-lmer(log(TN + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_oct) 
anova(TNmodoct)
summary(TNmodoct)

TNplotoct <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TN + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  labs(y="", x="") +
  scale_colour_poke(pokemon = 6)+
  theme(legend.position = "none")+
  ggtitle('October')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE) 
TNplotoct

######## TOTAL PHOSPHORUS VS SHAD RELATIVE ABUNDANCE  #####################

#TP x SHAD CPUE
TPmod1<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data = nutrients) 
anova(TPmod1)
r.squaredGLMM(TPmod1)
confint(TPmod1)
summary(TPmod1)
plot(TPmod1)

#june TP only
modjune<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_june) 
anova(modjune)
summary(modjune)

TPplotjune <- ggplot(nutrients_june, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Total Phosphorus ppm + 0.01)", x="") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplotjune

#aug TP only
modaug<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_aug) 
anova(modaug)
summary(modaug)

TPplotaug <- ggplot(nutrients_aug, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplotaug

#oct TP only
modoct<-lmer(log(TP + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data = nutrients_oct) 
anova(modoct)
summary(modoct)

TPplotoct <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x="") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplotoct

# combined TP plot across months
TPplot <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(TP + 0.01), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(Total Phosphorus ppm + 0.01)", x="Ln(Gizzard Shad Per Minute + 0.05)") +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
TPplot

########## N:P RATIOS VS SHAD RELATIVE ABUNDANCE ##############################
NPratiomod_ln<-lmer(log(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * month * (1|Impoundment), data=nutrients) 
anova(NPratiomod_ln)
summary(NPratiomod_ln)
confint(NPratiomod_ln)
plot(NPratiomod_ln)
r.squaredGLMM(NPratiomod_ln)

NPratioplot <- ggplot(aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = month), data = nutrients) +
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  labs(y="Ln(N:P ratio)", x="Ln(Gizzard Shad Per Minute + 0.05)")
NPratioplot



## June N:P ratio # YES SIGNIFICANT
mod0june<-lmer(log(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data=nutrients_june) 
anova(mod0june)
summary(mod0june)
confint(mod0june)
plot(mod0june)
r.squaredGLMM(mod0june)

NPplotjune <- ggplot(nutrients_june, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="Ln(N:P Ratio)", x=expression(paste("Ln(Gizzard Shad Min"^-1," + 0.05)"))) +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
NPplotjune

# August N:P ratio # NOT SIGNIFICANT
mod0aug<-lmer(log(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data=nutrients_aug) 
anova(mod0aug)
summary(mod0aug)
confint(mod0aug)
r.squaredGLMM(mod0aug)
plot(mod0aug)

NPplotaug <- ggplot(nutrients_aug, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x=expression(paste("Ln(Gizzard Shad Min"^-1," + 0.05)"))) +
  theme(legend.position = "none")+
  geom_smooth(method="lm",se=FALSE) 
NPplotaug

### October N:P ratio # YES SIGNIFICANT
mod0oct<-lmer(log(NPratio) ~ log(shad_cpe_electrofishing + 0.05) * (1|Impoundment), data=nutrients_oct) 
anova(mod0oct)
summary(mod0oct)
confint(mod0oct)
plot(mod0oct)
r.squaredGLMM(mod0oct)

NPplotoct <- ggplot(nutrients_oct, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(NPratio), colour = Impoundment)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 6)+
  labs(y="", x=expression(paste("Ln(Gizzard Shad Min"^-1," + 0.05)"))) +
  #labs(x = NULL, y = expression(paste("EC"[50]," ","(mmol/L)")))      
  geom_smooth(method="lm",se=FALSE) 
NPplotoct

pokedex()

######
#NPratioplot <- ggplot(aes(x = log(shad_cpe_electrofishing + 0.05), y = sqrt(NPratio), colour = month), data = nutrients) +
#theme_cowplot(12)+
#geom_point()+
#geom_smooth(method="lm",se=FALSE)+
#labs(y="Sqrt(N:P ratio)", x="Ln(Gizzard Shad Per Minute + 0.05)")
#NPratioplot

##########################################################################################################
#################### Combine into multipanel plots for manuscript figures ################################
##########################################################################################################

(TNplotjune | TNplotaug | TNplotoct) / (TPplotjune | TPplotaug | TPplotoct) / (NPplotjune | NPplotaug | NPplotoct ) + plot_layout(guides = "collect")

ggsave(filename = "9panelplot_Aug25mod.png", dpi = 600)
patchworkplot<-(TNplotjune | TNplotaug | TNplotoct) / (TPplotjune | TPplotaug | TPplotoct) / (NPplotjune | NPplotaug | NPplotoct ) + plot_layout(guides = "collect")
patchworkplot

######### SECCHI ######################
#Site by density matrix for shad and algae
secchi<-read.csv(file="secchi_reduced.csv", header = TRUE, sep = ",")
# create new dataset without missing data
secchinozero <- na.omit(secchi)
secchinozero$month<-factor(secchinozero$month, levels = c("JUNE", "AUG", "OCT"))

JuneSecchi <- subset(secchinozero, month == "JUNE")
attach(JuneSecchi)
AugSecchi <- subset(secchinozero, month == "AUG")
attach(AugSecchi)
OctSecchi <- subset(secchinozero, month == "OCT")
attach(OctSecchi)

attach(secchinozero)
as.factor(lakecode)
as.factor(month)
as.factor(year)

# OVERALL SECCHI
mod8<-lmer(log(secchinozero$secchi) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(mod8)
summary(mod8)
plot(mod8)
r.squaredGLMM(mod8)
confint(mod8)

#JUNE SECCHI
mod8june<-lmer(log(secchi) ~ log(shadpermin + 0.05) * (1|lakecode), data = JuneSecchi ) 
anova(mod8june)
summary(mod8june)
plot(mod8june)
r.squaredGLMM(mod8june)
confint(mod8june)
#AUG SECCHI
mod8aug<-lmer(log(secchi) ~ log(shadpermin + 0.05) * (1|lakecode), data = AugSecchi ) 
anova(mod8aug)
summary(mod8aug)
plot(mod8aug)
r.squaredGLMM(mod8aug)
confint(mod8aug)
#OCT SECCHI
mod8oct<-lmer(log(secchi) ~ log(shadpermin + 0.05) * (1|lakecode), data = OctSecchi ) 
anova(mod8oct)
summary(mod8oct)
plot(mod8oct)
r.squaredGLMM(mod8oct)
confint(mod8oct)

SecchiPlot <- ggplot(secchinozero, aes(x = log(shadpermin + 0.05), y = log(secchi), colour = month)) +
  geom_point()+
  labs(y="Ln(Secchi Depth (m))", x="Ln(Gizzard Shad Per Minute + 0.01)")+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  geom_smooth(method="lm",se=FALSE)
SecchiPlot

###########################################################################################################
######### ALGAE ######################################################################################
######################################################################################################
shadalgae<-read.csv(file="summary_algae_reduced.csv", header = TRUE, sep = ",")
attach(shadalgae)
as.factor(lakecode)
as.factor(Month, levels = c("JUNE", "AUG", "OCT"))

shadalgae$Month<-factor(shadalgae$Month, levels = c("JUNE", "AUG", "OCT"))


############### CYANOBACTERIA VS SHAD CPUE ##############################################################################

cyanomod<-lmer(log(cyano + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(cyanomod) 
summary(cyanomod)
confint(cyanomod)
r.squaredGLMM(cyanomod)
plot(cyanomod)


BGPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.01), y = log(cyano), colour = Month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  theme(legend.position = "none")+
  labs(y="Ln(Cyanobacteria Grid Count)", x="")+
  theme(legend.position = "none")
BGPlot
#################### DIATOMS x SHAD CPUE ######################################################################
diatommod<-lmer(log(diatom + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(diatommod) 
summary(diatommod)
confint(diatommod)
r.squaredGLMM(diatommod)
plot(diatommod)

DiatomPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.05), y = log(diatom), colour = Month)) +
  geom_point()+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  theme(legend.position = "none")+
  labs(y="Ln(Diatom Grid Count)", x="Ln(Gizzard Shad Per Minute + 0.05)")
DiatomPlot
################################ GREEN ALGAE x SHAD CPUE ###################################################################

greenmod<-lmer(log(green + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(greenmod) #interaction is not significant so we don't include it
summary(greenmod)
confint(greenmod)
r.squaredGLMM(greenmod)
plot(greenmod)

GreenPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.01), y = log(green), colour = Month)) +
  theme_cowplot(12)+
  geom_point()+
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 1)+
  labs(y="Ln(Green Algae Grid Count)", x="")
GreenPlot
################################ DINOFLAGELLATE ALGAE x SHAD CPUE ###################################################################
mod4<-lmer(log(dinoflag + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae_nozero) #interactive model
anova(mod4) #interaction is not significant so we don't include it
summary(mod4)
plot(mod4)

mod4a<-lmer(log(dinoflag + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae_nozero) #interactive model
anova(mod4a) #interaction is not significant so we don't include it
summary(mod4a)
plot(mod4a)

DinoPlot <- ggplot(shadalgae, aes(x = log(shad_cpe_electrofishing + 0.01), y = log(dinoflag + 1), colour = Month)) +
  theme_cowplot(12)+
  geom_point()+
  theme(legend.position = "none")+
  scale_colour_poke(pokemon = 1)+
  labs(y="Ln(Green Algae Grid Count)", x="Ln(Gizzard Shad Per Minute + 0.05)")
DinoPlot

############################# TOTAL ALGAL COUNT x SHAD CPUE #########################################################################
totphytomod<-lmer(log(total_phytoplankton + 0.01) ~ log(shad_cpe_electrofishing + 0.05) * Month * (1|lakecode), data=shadalgae) #interactive model
anova(totphytomod) #interaction is not significant so we don't include it
summary(totphytomod)
confint(totphytomod)
r.squaredGLMM(totphytomod)
plot(totphytomod)

TotAlgaePlot <- ggplot(shadalgae, aes(x=log(shad_cpe_electrofishing + 0.05), y = log(total_phytoplankton + 1), colour = Month)) +
  geom_point()+
  theme(legend.position = "none")+
  theme_cowplot(12)+
  scale_colour_poke(pokemon = 1)+
  labs(y="Ln(Total Phytoplankton Grid Count)", x="Ln(Gizzard Shad Per Minute + 0.05)")
TotAlgaePlot
######################## MUlTIPANEL PLOTS WITH PATCHWORK ##################
patchworkalgae<-plot_grid(BGPlot,GreenPlot, DiatomPlot, TotAlgaePlot, labels = c('A', 'B', 'C', 'D'), label_size = 12) + plot_layout(guides = "collect")    #MULTIPANEL ALGAE BOXPLOT

(BGPlot | GreenPlot) / (DiatomPlot | TotAlgaePlot ) + plot_layout(guides = "collect")
ggsave(filename = "4panelalgae.png", dpi = 600)

##############################################################################################
###### ZOOPLANKTON ###########################################################################
###############################################################################################
zoopnozero<-read.csv(file="zoopnozero_reduced.csv", header = TRUE, sep = ",")
zoopnozero <- na.omit(zoopnozero)
attach(zoopnozero)
as.factor(lakecode)
as.factor(month)
as.factor(year)

JuneZoop <- subset(zoopnozero, month == "JUNE")
attach(JuneZoop)
AugZoop <- subset(zoopnozero, month == "AUG")
attach(AugZoop)
OctZoop <- subset(zoopnozero, month == "OCT")
attach(OctZoop)

####### #CLADOCERANS x SHAD CPUE # SIGNIFICANT AT 0.1 #######################################
cladocmod<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.05) * month * (1|lakecode), data = zoopnozero) 
anova(cladocmod)
summary(cladocmod)
plot(cladocmod)
r.squaredGLMM(cladocmod)
confint(cladocmod)

## JUNE IS SIGNIFICANT
mod9june<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.05) * (1|lakecode), data = JuneZoop ) 
anova(mod9june)
summary(mod9june)
plot(mod9june)
r.squaredGLMM(mod9june)
confint(mod9june)
## AUG 
mod9aug<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.05) * (1|lakecode), data = AugZoop ) 
anova(mod9aug)
summary(mod9aug)
plot(mod9aug)
r.squaredGLMM(mod9aug)
confint(mod9aug)
# OCT 
mod9aug<-lmer(sqrt(Cladocera) ~ log(shadpermin + 0.01) * (1|lakecode), data = OctZoop ) 
anova(mod9aug)
summary(mod9aug)
plot(mod9aug)
r.squaredGLMM(mod9aug)
confint(mod9aug)
#########
CladocPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Cladocera))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Cladocera Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CladocPlot
################## #CALANOIDS x SHAD CPUE ################################################################
calanmod<-lmer(log(Calanoida) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(calanmod)
summary(calanmod)
plot(calanmod)
r.squaredGLMM(calanmod)
confint(calanmod)

CalanPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Calanoida))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Calanoid Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CalanPlot

#CYCLOPOIDS x SHAD CPUE
cyclomod<-lmer(sqrt(Cyclopoida) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(cyclomod)
summary(cyclomod)
plot(cyclomod)
r.squaredGLMM(cyclomod)
confint(cyclomod)

CyclopoidPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Cyclopoida))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Cyclopoid Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
CyclopoidPlot

#ROTIEFERS x SHAD CPUE
rotifermod<-lmer(sqrt(Rotifera) ~ log(shadpermin + 0.05) * month * (1|lakecode)) 
anova(rotifermod)
summary(rotifermod)
plot(rotifermod)
r.squaredGLMM(rotifermod)
confint(rotifermod)

RotiferPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Rotifera))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Sqrt(Rotifer Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
RotiferPlot

#NAUPLII x SHAD CPUE
naupmod<-lmer(sqrt(Nauplii) ~ log(shadpermin + 0.05) * month + (1|lakecode)) 
anova(naupmod)
summary(naupmod)
confint(naupmod)
plot(naupmod)
r.squaredGLMM(naupmod)

NaupliiPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = sqrt(Nauplii))) +
  geom_point()+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  scale_color_poke(pokemon = 1)+ #bulbasaur color theme
  labs(y="Sqrt(Nauplii Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")
NaupliiPlot

#TOTAL ZOOPLANKTON x SHAD CPUE
totzoopmod<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.05) * month * (1|lakecode))
anova(totzoopmod)
summary(totzoopmod)
plot(totzoopmod)
r.squaredGLMM(totzoopmod)
confint(totzoopmod)

## JUNE IS SIGNIFICANT
mod14june<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) * (1|lakecode), data = JuneEverything ) 
anova(mod14june)
summary(mod14june)
plot(mod14june)
r.squaredGLMM(mod14june)
confint(mod14june)
## AUG 
mod14aug<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) * (1|lakecode), data = AugEverything ) 
anova(mod14aug)
summary(mod14aug)
plot(mod14aug)
r.squaredGLMM(mod14aug)
confint(mod14aug)
# OCT 
mod14oct<-lmer(log(TotZoop + 1) ~ log(shadpermin + 0.01) * (1|lakecode), data = OctEverything ) 
anova(mod14oct)
summary(mod14oct)
plot(mod14oct)
r.squaredGLMM(mod14oct)
confint(mod14oct)

TotZoopPlot <- ggplot(zoopnozero, aes(x = log(shadpermin + 0.05), y = log(TotZoop))) +
  geom_point()+
  theme_cowplot(12)+
  scale_color_poke(pokemon = 1)+  #bulbasaur color theme
  labs(y="Ln(Total Zooplankton Density)", x="Ln(Gizzard Shad Per Minute + 0.05)")+
  geom_smooth(method="lm",se=FALSE)
TotZoopPlot