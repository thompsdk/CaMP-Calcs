library(rpart)
library(rpart.plot)
### Analysis of Zoltai BD data for CaMPS
library(car)
library(lme4)
#library(cars)
library(dplyr)
library(tidyverse)

#boxplot(MaxNDVI ~ Class,MaxNDVI_MDC)
#summary(aov(MaxNDVI ~ Class,data=MaxNDVI_MDC))




##################################################
#### simple regression tree for bulk density
#### used in the Bauer et al 2024 national peat core composite
##################################################

####################################
#### Load Data
###########################

PROFILES <- read.csv("~/CaMP-Calcs/PROFILES_March11_2021.csv")
tree.for.model <- subset(PROFILES,DB_EST_TYPE == "BD_TREE_ORG" & MATERIAL_1 != "GAP.NSL")
BD.training <- subset(PROFILES,DB_MEAS_EST == "MEAS")

Zoltai <- read.csv("~/CaMP-Calcs/Zoltai.csv")


#BD.training$MIDDEPTH <- BD.training$UPPER_SAMP_DEPTH+BD.training$SAMP_THICK/2

BD.tree <- rpart(BULK_DENSITY~ASH + UPPER_SAMP_DEPTH + MATERIAL_1 + MIDDEPTH + CWCS_CLASS,data=BD.training)
printcp(BD.tree)
rpart.plot(BD.tree)
BD.predict <- predict(BD.tree,newdata=tree.for.model)

cor(x=tree.for.model$BULK_DENSITY,y=BD.predict,method="pearson")

MAE <- mean(abs(tree.for.model$BULK_DENSITY-BD.predict))

MAE/mean(tree.for.model$BULK_DENSITY)


################################################################
#### linear mixed-effects model for bulk density ##############
################################################################

### note this is using just the older Zoltai-only data, the newer Bauer et al 2024 national composite needs to be used now

###https://open.canada.ca/data/dataset/e287b0dd-8b07-435b-8a76-0c7ec84ac228

### the file PROFILES_2024.csv provided is from Bauer et al 2024

PROFILES_2024 <- read.csv("~/CaMP-Calcs/PROFILES_2024.csv")


### step 1: calculate the peat carbon density from bulk density and carbon content
PROFILES_2024$PeatC_dens <- PROFILES_2024$BULK_DENSITY * PROFILES_2024$C_TOT_PCT/100

### step 2: compute an ordinal depthclass at 15 cm intervals, representing the typical sample thickness in the Zoltai dataset

PROFILES_2024$DEPTHCLASS <- floor(PROFILES_2024$UPPER_SAMP_DEPTH/15)+1

### step 3: import ecozone, tree cover, and peat type data from the CORESITES 2024 tabular data
### using dplyr inner_join https://dplyr.tidyverse.org/reference/mutate-joins.html

CORESITES_2024 <- read.csv("~/CaMP-Calcs/CORESITES_2024_ecozones.csv")

### need to compute a CaMPNutrient factor of "bog", "poor fen", "rich fen"

CORESITES_2024$FEN_TYPE <- as.factor(CORESITES_2024$FEN_TYPE)
CORESITES_2024$CWCS_CLASS <- as.factor(CORESITES_2024$CWCS_CLASS)
CORESITES_2024$CWCS_FORM <- as.factor(CORESITES_2024$CWCS_FORM)
CORESITES_2024$ECOZONE_NAME_EN <- as.factor(CORESITES_2024$ECOZONE_NAME_EN)

levels(CORESITES_2024$CWCS_CLASS)
levels(CORESITES_2024$FEN_TYPE)
levels(CORESITES_2024$CWCS_FORM)

### split into poor and rich fens
CORESITES_2024$CaMPNutrient_2024 <- case_when(
  CORESITES_2024$CWCS_FORM == "palsa" | CORESITES_2024$CWCS_FORM == "peat plateau" | CORESITES_2024$CWCS_FORM == "polygonal peat plateau" | CORESITES_2024$CWCS_FORM == "plateau : northern plateau" ~ "permafrost",
  CORESITES_2024$CWCS_CLASS == "bog" ~ "bog",
  CORESITES_2024$CWCS_CLASS == "marsh" ~ "marsh",
  CORESITES_2024$CWCS_CLASS == "swamp" ~ "swamp",
  CORESITES_2024$CWCS_CLASS == "fen" & CORESITES_2024$FEN_TYPE == "poor" ~ "poor fen",
  CORESITES_2024$CWCS_CLASS == "fen" & CORESITES_2024$FEN_TYPE != "poor" ~ "rich fen",
  .default = as.character(NA)
)

CORESITES_2024$CaMPNutrient_2024 <- as.factor(CORESITES_2024$CaMPNutrient_2024)

levels(CORESITES_2024$CaMPNutrient_2024)
levels(CORESITES_2024$ECOZONE_NAME_EN)

### right now using the provided "TREED" Y/N field, but could also go back in time with SCANFI and get the tree canopy cover at the time of sampling where it is not otherwise provided

### extract the Nutrient class and TREED per row in PROFILES_2024

#### build a dataframe of just CORESITES COREID, PROV, ECOZONE, CaMPNutrient, and TREEED

CORESITES_extract <- CORESITES_2024 %>% select(CORE_ID, PROV_TERR, ECOZONE_NAME_EN,CaMPNutrient_2024,TREED)

### then inner_join

PROFILE_2024_w_site_info <- inner_join(PROFILES_2024,CORESITES_extract,by="CORE_ID")






###note the slightly different header names in the 2024 document


### some simple linear models just to baseline

Zoltai.lm1 = lm(BD ~ log(MIDDEPTH)+CaMPTree+CaMPNutrient,data=Zoltai)
summary.lm(Zoltai.lm1)

Zoltai.lm2 = aov(BD ~ log(DEPTHCLASS)+CaMPTree+CaMPNutrient,data=Zoltai)
Anova(Zoltai.lm2, type="III")
summary.lm(Zoltai.lm2)


Zoltai$PeatC_dens <- as.numeric(Zoltai$PeatC_dens)
### null model
BD.null = lmer(PeatC_dens ~ log(DEPTHCLASS)+(1|CONCAT),data=Zoltai,REML=FALSE)
summary(BD.null)

BD.model1 = lmer(PeatC_dens ~ log(DEPTHCLASS)+(1|CONCAT)+CaMPTree,data=Zoltai,REML=FALSE)
summary(BD.model1)
anova(BD.null,BD.model1)

### this is best model so far

BD.model2a = lmer(PeatC_dens ~ log(DEPTHCLASS)+(1|CONCAT)+CaMPTree+CaMPNutrient,data=Zoltai,REML=FALSE)

### where PeatC_dens is the sample-measured (no model or interpolation) carbon density in g/cm3
### DEPTHCLASS is a 15-cm depth class interval, corresponding to the mean sample thickness in the dataset
### CONCAT is the concatenation of YEAR, SITE, and SUBSITE into a unique string for each core
### CaMPClass 1 

### note that the lmer is used here for completeness 



#### #not run: try a version of the data with no swamps at all
#BD.model2c = lmer(PeatC_dens ~ log(DEPTHCLASS)+(1|CONCAT)+CaMPTree*CaMPNutrient,data=Zoltai_no_swamp,REML=FALSE)
#summary(BD.model2c)


BD.model2b = lmer(PeatC_dens ~ (1+log(DEPTHCLASS)|CaMPTree)+(1|CONCAT)+CaMPNutrient+(1|YEAR),data=Zoltai,REML=FALSE)
summary(BD.model2b)
coef(BD.model2b)

#### doesn't converge
BD.model2c = lmer(PeatC_dens ~ (1+log(DEPTHCLASS)|CaMPNutrient)+(1|CONCAT)+(1+log(DEPTHCLASS)|CaMPTree)+(1|YEAR),data=Zoltai,REML=FALSE)

#### Go with this one now....  Same AIC as 2B above
BD.model2 = lmer(PeatC_dens ~ (1+log(DEPTHCLASS)|CaMPNutrient)+(1|CONCAT)+CaMPTree+(1|YEAR),data=Zoltai,REML=FALSE)


BD.model2e = lmer(PeatC_dens ~ (1+log(DEPTHCLASS)|CaMPNutrient)+(1|CONCAT)+CaMPTree+(1|YEAR)+ECOREGION,data=Zoltai,REML=FALSE)

summary(BD.model2)
#anova(BD.model2,BD.model2e)
ranef(BD.model2)
fixef(BD.model2)
coef(BD.model2)

### too much noise for interaction
BD.model3 = lmer(BD ~ log(DEPTHCLASS)+(1|CONCAT)+CaMPClass1*CaMPClass2,data=Zoltai,REML=FALSE)
summary(BD.model3)
anova(BD.model2,BD.model3)
coef(BD.model3)

### add in ecozone and MAT, no improvement
BD.model4 = lmer(BD ~ log(DEPTHCLASS)+(1|CONCAT)+CaMPClass1+CaMPClass2+ECOPROV,data=Zoltai,REML=FALSE)
summary(BD.model4)
anova(BD.model2,BD.model4)


##############################
#### New 2024 peat core dataset including permafrost and potentially ecozone
############################

BD.model2b.2024 = lmer(PeatC_dens ~ log(DEPTHCLASS)+TREED+(1|CORE_ID)+CaMPNutrient_2024+ECOZONE_NAME_EN,data=PROFILE_2024_w_site_info,REML=FALSE)
summary(BD.model2b.2024)
coef(BD.model2b.2024)


#######################################
### end BD model selection/exploration
#######################################



##########################################################################################
### leave one out (LOO) cross-validation and scatterplot of those results.  Use predict().
### have not done for 2024 BD model yet
#########################################################################################


Zoltai[1,]

n <- nrow(Zoltai)
#i <- 10
#data<-rbind(Zoltai[1:(i-1),],Zoltai[(i+1):n,])
#nrow(Zoltai[(i+1):n,])
Z <- array(0, c(5017,2))

#Zoltai$PeatC_dens[i]

### Leave-one-out cross validation, but don't ever use any data from the same subsite/core at all:
for (i in 22:(n-22)){
  BD.model.temp <- lmer(PeatC_dens ~ (1+log(DEPTHCLASS)|CaMPNutrient)+(1|CONCAT)+CaMPTree,data=rbind(Zoltai[1:(i-20),],Zoltai[(i+20):(n-22),]),REML=FALSE)
  Z[i,1] <- Zoltai$PeatC_dens[i]
  Z[i,2] <- predict(BD.model.temp, newdata=Zoltai[i,])
}

for (i in 50:(n-20)){
  BD.model.temp <- lmer(PeatC_dens ~ (1+log(DEPTHCLASS)|CaMPNutrient)+(1|CONCAT)+CaMPTree,data=rbind(Zoltai[1:(i-20),],Zoltai[(i+20):(n-20),]),REML=FALSE)
  Z[i,1] <- Zoltai$PeatC_dens[i]
  Z[i,2] <- predict(BD.model.temp, newdata=Zoltai[i,], allow.new.levels=TRUE)
}


#levels(Zoltai$CaMPTree)
plot(Z[,1],Z[,2])
abline(0,1)

library(ggplot2)


###option to write out the outputs of the Linear Mixed Effect Model on a leave-one-out basis in order to assess the LME accuracy with a simple 1:1 biplot
###write.csv(Z,"LME_pred_Vs_obs.csv")

########################################################################
#create "standard" BD curves per 9 class + swamp by depth using predict.
########################################################################

### right now, only using the Zoltai data, need to update to the newer national composite:

### 221 represents the number of cores in the Zoltai data

### just filling in the full parameter space of depth and tree/nutrient class with the above BD.model2

##########################
### 2024-06-26 edit: use dplyr to fill in parameter space of national peat compilation
##########################



### use dplyr's expand to get the full parameter space of ecozone * treed (Y/N/U), nutrient class and depth class < 20 (using filter() after)
PeatParamSpace <- tibble(PROFILE_2024_w_site_info) %>% expand(ECOZONE_NAME_EN,TREED,CaMPNutrient_2024,DEPTHCLASS) %>% filter(DEPTHCLASS<=20) %>% drop_na(ECOZONE_NAME_EN) %>% drop_na(CaMPNutrient_2024)


### Fill in the std BD curves
std <- array(NA, c(nrow(PeatParamSpace)))
for (i in seq(1:nrow(PeatParamSpace))){
#for (i in seq(1:1120)){
  std[i] <- predict(BD.model2b.2024, newdata=PeatParamSpace[i,], re.form=~0)
}

Peat_std_curves_long <- cbind(PeatParamSpace,std)

colnames(Peat_std_curves_long)[5] <- c("PeatC_dens")

#### then do the running sum along each parameter space instance until depthclass == 1

Core_C_density <- array(NA, c(nrow(Peat_std_curves_long)))
for (i in seq(1:nrow(Peat_std_curves_long))){
  Core_C_density[i] <- ifelse(Peat_std_curves_long$DEPTHCLASS[i] == 1, Peat_std_curves_long$PeatC_dens[i]*1000*0.15,Core_C_density[i-1]+(Peat_std_curves_long$PeatC_dens[i]*1000*0.15))
  
}

MIDDEPTH <- Peat_std_curves_long$DEPTHCLASS*15-7.5
Peat_std_curves_long <- cbind(Peat_std_curves_long,Core_C_density,MIDDEPTH)



### write this if not already written
##write.csv(std,"std_curves_output.csv")

#### ! not shown: summing up the sample-level gC/cm3 BD predictions into a kgC/m2 sum of C as one goes downward through the depth classes

### the fields being "PeatC_dens" for the instant gap-fill and "Core_C_density" for the cumulative

### (this is what is shown in Zoltai_std_curves_long.csv though)



################################
### Graphing and curve fitting, as seen in Bona et al 2020
################################
library(ggplot2)
library(nls2)
Zoltai_std_curves_long <- read.delim("~/CaMP-Calcs/Zoltai_std_curves_long.csv")
Zoltai_std_curves_long$CaMPClass <- factor(Zoltai_std_curves_long$CaMPClass, levels = c('open bog','treed bog','forested bog','open poor fen','treed poor fen','forested poor fen','open rich fen','treed rich fen','forested rich fen','treed swamp','forested swamp'))
Palette1 <- c("#0a4b32","#6a917b","#aad7bf","#5c5430","#aba476","#fbfabc","#792652","#af7a8f","#ffb6db","black","grey")

### regular x-axis spacing
ggplot(Zoltai_std_curves_long, aes(Core_C_density,(MIDDEPTH-7.5), colour=CaMPClass))+geom_line(size=1.5) + ylim(120,0) + xlim(0,85) + scale_colour_manual(values=Palette1) + labs(x = "Core Carbon density kg m^-2", y = "Depth from surface (cm)")

#### down to 3m
ggplot(Zoltai_std_curves_long, aes(Core_C_density,(MIDDEPTH-7.5), colour=CaMPClass))+geom_line(size=1.5) + ylim(300,0) + xlim(0,225) + scale_colour_manual(values=Palette1) + labs(x = "Core Carbon density kg m^-2", y = "Depth from surface (cm)")

### plot the obs-pred total C density
ggplot(Zoltai_core_compare, aes(Obs_C_density_total,Pred_C_Density_total, colour=CaMPClass))+geom_point(size=2)+ scale_colour_manual(values=Palette1) + geom_abline(intercept = 0, slope = 1)

### plot the obs-pred 1m C density
ggplot(Zoltai_core_compare, aes(Obs_C_Density_1m,Pred_C_Density_1m, colour=CaMPClass))+geom_point(size=2)+ scale_colour_manual(values=Palette1) + geom_abline(intercept = 0, slope = 1)

## curve fitting as seen in the Bona et al 2020 paper: http://www.sciencedirect.com/science/article/pii/S0304380020302350
## instantaneous C density: c*ln(depth from surface cm)-d

## NB! need to change CaMPClass manually each time here, not looping throuht (but could for a more complete document)
peatdata <- subset(Zoltai_std_curves_long, CaMPClass=="open poor fen")
### !!

attach(peatdata)
peatfit_inst <- nls2(PeatC_dens~c*log(MIDDEPTH)-d,data=peatdata,start=list(c=0.012,d=0.0035))
peatfit_cumul <- nls2(Core_C_density~a*(MIDDEPTH)^b,data=peatdata,start=list(a=0.3,b=1.03))
detach(peatdata)
summary(peatfit_cumul)
summary(peatfit_inst)


##############################
#### end curve fitting
#############################


##############################################################################
#### Misc summary stats
#############################################################################

#### attach max depth data from each core to CampClass and lat/long/ecozone
Samples <- read.csv("~/CaMP-Calcs/Samples.csv")
Sites <- read.csv("~/CaMP-Calcs/Sites.csv")

### traverse samples, look for UNIQUE_SITE in samples, grab the fields CaMPClass1 and 2, lat, and long, from Sites, insert into Samples

for (i in 1:nrow(Samples)) {
  Samples[i,13] <- Sites[match(Samples[i,4],table=Sites[,4]),5]
  Samples[i,14] <- Sites[match(Samples[i,4],table=Sites[,4]),6]
  Samples[i,15] <- Sites[match(Samples[i,4],table=Sites[,4]),16]
  Samples[i,16] <- Sites[match(Samples[i,4],table=Sites[,4]),20]
}
write.csv(Samples,"Samples_w_loc2.csv")
Samples$long <- Samples$long*-1

###load up data with samples, ecozones, and CaMP classes
Samples_w_loc_csv_v2 <- read.delim("~/CaMP-Calcs/Samples_w_loc_csv_v2.csv")
ecozones.aov <- aov(MaxDepth ~ ECOZONE,data=Samples_w_loc_csv_v2)
TukeyHSD(ecozones.aov)

ecozones.aov2 <- aov(MaxDepth ~ ECOZONE*CaMPClass2,data=Samples_w_loc_csv_v2)
summary(ecozones.aov2)

### low arctic has differing mean than Boreal Plains via Wilcox test
wilcox.test(Samples_w_loc_csv_v2$MaxDepth[Samples_w_loc_csv_v2$ECOZONE=="Boreal Plains"], Samples_w_loc_csv_v2$MaxDepth[Samples_w_loc_csv_v2$ECOZONE=="Taiga Cordillera"] , paired = FALSE, exact = NULL, correct = TRUE,conf.int = TRUE, conf.level = 0.95)

### one visualization
ggplot(Samples_w_loc_csv_v2, aes(ECOZONE, MaxDepth)) + geom_boxplot() + geom_jitter(aes(colour = factor(CaMPClass2)),width = 0.2) + xlab("Peat Depth per core (cm)")

### ecdf
ggplot(Samples_w_ecozones_3978, aes(MaxDepth, colour = ECOZONE)) + stat_ecdf() + ylab("Percentile of Zoltai cores") + xlab("Peat Depth per core (cm)")


############################
### end misc stats
###########################



########################################################################################
### comprehensive regression tree models for density and CTOT Feb 2020
### Using updated dataset Feb 2021 from Ilka
#library("rpart.plot", lib.loc="~/R/win-library/3.4")
########################################################################################

PROFILESFeb3 <-  read.csv("~/CaMP-Calcs/PROFILES_Feb2021.csv", stringsAsFactors=TRUE)


attach(PROFILESFeb3)
bd.model.obs <- subset(PROFILESFeb3, BD_MEAS_EST == "MEAS" & ASH < 67)
bd.model.obs.min <- subset(PROFILESFeb3, BD_MEAS_EST == "MEAS" & ASH > 67)
CTOT.mode.obs.org <- subset(PROFILESFeb3, C_TOT_MEAS_EST == "MEAS" & ASH < 67)
#CTOT.mode.obs.min <- subset(PROFILESFeb3, C_TOT_MEAS_EST == "MEAS" & ASH > 67)
mean(CTOT.mode.obs.org$C_TOT_PCT,na.rm=TRUE)
#mean(CTOT.mode.obs.min$C_TOT_PCT_BLANK,na.rm=TRUE)
### mean C_TOT for ASH < 67 = 45.77%
### mean C_TOT for ASH > 67 = 7.8%

colnames(PROFILESFeb3)

MAT_CTOT_MEAN <- tapply(PROFILESFeb3$C_TOT_PCT_BLANK,  MATERIAL_1_NEW, mean, na.rm=TRUE)


plot(BULK_DENSITY,C_TOT_PCT_BLANK)
###with CWCS_CLASS### bd.predict <- rpart(BULK_DENSITY~C_TOT_PCT+CWCS_CLASS + PERMAFROST+ MIDDEPTH+ASH+VON_POST+MATERIAL_1_NEW+MATERIAL_2_NEW, data=bd.model.obs)
bd.predict <- rpart(BULK_DENSITY~ASH+C_TOT_PCT + CWCS_CLASS+ PERMAFROST+ MIDDEPTH+ASH+VON_POST+MATERIAL_1+MATERIAL_2, data=bd.model.obs)

rpart.plot(bd.predict)

bd.predict.min <- rpart(BULK_DENSITY~ASH+C_TOT_PCT + CWCS_CLASS+ PERMAFROST+ MIDDEPTH+ASH+VON_POST+MATERIAL_1+MATERIAL_2, data=bd.model.obs.min)
rpart.plot(bd.predict.min)

detach(PROFILESFeb3)
### archive of CTOT tree, not to be used####
CTOT.predict <- rpart(C_TOT_PCT_BLANK~BULK_DENSITY+CWCS_CLASS + PERMAFROST+ MIDDEPTH+ASH+VON_POST+MATERIAL_1_NEW+MATERIAL_2_NEW, data=CTOT.mode.obs)
rpart.plot(CTOT.predict)
#### end older CTOT tree model

### make lookup dataframe of CTOT by MATERIAL

CTOT_BY_MAT <- tapply(CTOT.mode.obs.org$C_TOT_PCT, CTOT.mode.obs.org$MATERIAL_1, median,na.rm=TRUE)
CTOT_BY_MAT[2] <- mean(CTOT_BY_MAT[3],CTOT_BY_MAT[4]) ### reassign value of "B" material to mean of BB and BS
CTOT_BY_MAT[2]

CTOT_BY_MAT

### use if you want to write to overwrite file:
#write.csv(CTOT_BY_MAT,"CTOT_BY_MAT.csv")

### sum of stock of C (per sample) by coreID:
CoreStock <- tapply(PROFILESFeb3$SAMP_C_STOCK_kg_C_m2,  CORE_ID..Jan.20., sum, na.rm=TRUE)

### use if you want to write to overwrite file:
#write.csv(CoreStock,"Core_stock_C_kg_m2.csv")



CTOT_BY_MAT[CTOT_BY_MAT=PROFILESFeb3$MATERIAL_1[1]]

### Ilka's logic model for CTOT, from bottom up

###1 IF ASH != NA, then CTOT = (100-ASH)*0.514 where ASH <= 67 ELSE
###2 IF MATERIAL != NA, then assign CTOT by Material ELSE
###3 IF BD != NA AND BD > 0.096, THEN CTOT = 7.9*BD^(-0.76)
###4 IF BD != NA AND BD <= 0.096, THEN CTOT = 0.468

CTOT_MOD <- array(NA,dim=nrow(PROFILESFeb3))
PROFILESFeb3 <- cbind(PROFILESFeb3,CTOT_MOD)

if (PROFILESFeb3$ASH<67) { 
  PROFILESFeb3$CTOT_MOD <- (100-PROFILESFeb3$ASH)*0.514 
} else {
  if (PROFILESFeb3$MATERIAL_1 =! NA) {
    CTOT_BY_MAT[CTOT_BY_MAT=PROFILESFeb3$MATERIAL_1]
  } else {
    if (PROFILESFeb3$BULK_DENSITY > 0.096) {PROFILESFeb3$CTOT_MOD <- 7.9*PROFILESFeb3$BULK_DENSITY^(-0.76) 
    } else {
      PROFILESFeb3$CTOT_MOD <- 0.468}
  }
}

bd.predict.out <- predict(bd.predict, PROFILESFeb3, type = c("vector"))
bd.predict.out.min <- predict(bd.predict.min, PROFILESFeb3, type = c("vector"))
CTOT.predict.out <- predict(CTOT.predict, PROFILESFeb3, type = c("vector"))
write.csv(MAT_CTOT_MEAN,"MAT_CTOT_MEAN.csv")
write.csv(bd.predict.out,"bd_predict_out_org_only.csv")
write.csv(bd.predict.out.min,"bd_predict_out_min_only.csv")
write.csv(CTOT.predict.out,"CTOT_predict_out.csv")

#list.files(path = "D:/AllDec20_3857")
#tilesinfo <- file.info(list.files("D:/AllDec20_3857", full.names=TRUE))

### update bulk density model
ProfilesSub <- subset(PROFILESFeb3,select = c(BULK_DENSITY,C_TOT_PCT))
plot(ProfilesSub$BULK_DENSITY,ProfilesSub$C_TOT_PCT)
predict.nls <- nls(C_TOT_PCT~a*(BULK_DENSITY^(-b)),data=ProfilesSub,start=list(a=7.9,b=0.76))
yfitted <- predict(predict.nls)
lines(ProfilesSub$BULK_DENSITY,yfitted)

median(as.matrix(subset(PROFILESFeb3,MATERIAL_1 == "B" | MATERIAL_1 == "B.B" | MATERIAL_1 == "B.S" | MATERIAL_1 == "H" | MATERIAL_1 == "L" | MATERIAL_1 == "HU" ,select = C_TOT_PCT)),na.rm = TRUE)

median(as.matrix(subset(PROFILESFeb3,MATERIAL_1 == "B" | MATERIAL_1 == "B.B" | MATERIAL_1 == "B.S",select = C_TOT_PCT)),na.rm = TRUE)
mean(as.matrix(subset(PROFILESFeb3,MATERIAL_1 == "B" | MATERIAL_1 == "B.B" | MATERIAL_1 == "B.S",select = C_TOT_PCT)),na.rm = TRUE)

### mineral types
median(as.matrix(subset(PROFILESFeb3,MATERIAL_1 == "CL" | MATERIAL_1 == "GR" | MATERIAL_1 == "LOA"| MATERIAL_1 == "MI"| MATERIAL_1 == "SA"| MATERIAL_1 == "SI"| MATERIAL_1 == "TI",select = C_TOT_PCT)),na.rm = TRUE)

C_OM <- subset(PROFILESFeb3,ASH < 67 & C_TOT_PCT>0,select=c(C_TOT_PCT,ASH) )

mean(C_OM$C_TOT_PCT/(100-C_OM$ASH))
#[1] 0.5187978
median(C_OM$C_TOT_PCT/(100-C_OM$ASH))
#[1] 0.5224408