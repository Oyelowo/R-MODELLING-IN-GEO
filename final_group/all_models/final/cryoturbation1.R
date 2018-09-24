# cryoturbation, slope processes, NDVI and
# Nivation
rm(list = ls())
#moisture and nutrient on plant growth
#incoming solar radiation and and air temperature across different elevations
# require(effects); require(viridis)
# m <-lm(richness_1~soil_temp*soil_moist, data=d)
# plot(effect(term="soil_temp:soil_moist", mod=m,
#               xlevels=10), multiline=T, colors = rev(viridis(10)), lwd=2)
.libPaths("C:/Users/oyeda/Desktop/ADV_REM_SENS/library")
setwd("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124")
# gis_files = list.files(pattern="*.tif")
# gis_files <- dir("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124") #where you have your files
# 
# myfiles = lapply(gis_files, read.delim)


#your_data_frame <- do.call(rbind,lapply(file_names,raster(stack(myfiles))))

require(raster)
library(rgdal)
library(gbm)
library(mgcv)
library(dismo)
library(ggplot2)
library(GGally)
#library(tidyr)
library(corrplot)
library(caTools)

fp<-"C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/RastiTrainingData.csv"
data <- read.csv(fp, sep = ";")
#data<-data[0:100,]    #testing data
colnames(data)
ras_stack<- stack(raster("Elevation.tif"),raster("NDVI.tif"), raster("Prec_Ann.tif"),
                  raster("Radiation_7.tif"), raster("Slope.tif"),raster("Tavg_7.tif"))
#change the names to make the calibration data match with the prediction raster data
names(ras_stack)<-c("Altitude", "NDVI", "RR_annual","Radiation", "Slope","Tavg_7")


#' NOTE: the radiation variable has 
#' different units in calibration (MJ/cm^2) and raster data (KJ/M^2). 
#' Therefore you need to multiply the radiation values by 1 000 000 in the 
#' calibration data or divide the Radiation raster by 1 000 000. It is no wonder,
#' that the predictions did not make any sense!

#rescale the radiation raster to make it match with that of the dataframe
ras_stack$Radiation<- (ras_stack$Radiation)/1000000



cryo_glm <- glm(Cryoturb~ RR_annual+ Slope +  I(Slope^2), data=data, family = "poisson") 

cryo_pred_glm_ras<- predict(model=cryo_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(cryo_pred_glm_ras, main="Predicted Cryoturbation Coverage(GLM)")

#########


#GAM

cryo_gam <- gam(Cryoturb~ s(RR_annual, k=2)+ s(Slope, k=2), data = data, family = "poisson")
plot(cryo_gam, pages=1)
summary(cryo_gam)

#use the calibration data to predict into the raster stack
cryo_pred_gam_ras<- predict(object=ras_stack, model=cryo_gam, fun=predict.gam,type="response")
plot(cryo_pred_gam_ras, main="Predicted Cryoturbation Coverage(GAM)")





########################
#GBM
cryo_gbm1<-gbm(formula = Cryoturb~ RR_annual + Slope, data=data,
               distribution = "poisson",n.trees = 1300, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)
summary(cryo_gbm1)
best.iter<-gbm.perf(cryo_gbm1, plot.it = T, method = "OOB")
cryo_gbm1_pred<- predict.gbm(object = cryo_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_cryo <- cor(cryo_gbm1_pred, data$Cryoturb, method = "spearman")
cryo_pred_gbm1_ras<- predict(object=ras_stack,model=cryo_gbm1, fun=predict,
                          n.trees=cryo_gbm1$n.trees, type="response")
 plot(cryo_pred_gbm1_ras, main="Predicted Cryoturbation Coverage(GBM1)")
par(mfrow=c(1,2))
plot.gbm(cryo_gbm1, 1, best.iter)
plot.gbm(cryo_gbm1, 2, best.iter)
par(mfrow=c(1,1))


plot(predict.gbm(cryo_gbm1, data, best.iter), data$Cryoturb)
lines(lowess(predict.gbm(cryo_gbm1, data, best.iter), data$Cryoturb), col="red", lwd=3)
r_Cryo <-cor.test(predict.gbm(cryo_gbm1, data, best.iter), data$Cryoturb)
r2Cryp <- r_Cryo$estimate^2
r2Cryp
legend("topleft", paste("cor=", round(r2Cryp,3)))



##############################
#GBM2 with dismo package
cryo_gbm2 <- gbm.step(data=data, gbm.x =
                        c("RR_annual","Slope"), gbm.y = "Cryoturb",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="poisson",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
#best.iter<-gbm.perf(cryo_gbm1, plot.it = T, method = "OOB")
# cryo_gbm2_pred<- predict.gbm(object = cryo_gbm2, newdata = eva, best.iter,
#                              type="response")
summary(cryo_gbm1)
#this immediately does not work as expected, so, i'm using the next
#cryo_gbm2_pred<- predict(object=data,model=cryo_gbm2, fun=predict,n.trees=cryo_gbm2$n.trees, type="response")

cryo_pred_gbm2_ras <- predict(object=ras_stack,model=cryo_gbm2, fun=predict,
                         n.trees=cryo_gbm2$n.trees, type="response")
plot(cryo_pred_gbm2_ras)
###########
###For dismo package
plot(predict.gbm(cryo_gbm2, data, best.iter), data$Cryoturb)
lines(lowess(predict.gbm(cryo_gbm2, data, best.iter), data$Cryoturb), col="red", lwd=3)
r_Cryo2 <-cor.test(predict.gbm(cryo_gbm2, data, best.iter), data$Cryoturb)
r2Cryp2 <- r_Cryo2$estimate^2
r2Cryp2
legend("topleft", paste("cor=", round(r2Cryp2,3)))




par(mfrow=c(2,2))
plot(cryo_pred_glm_ras, main="Predicted Cryoturbation Coverage(GLM)")
plot(cryo_pred_gam_ras, main="Predicted Cryoturbation Coverage(GAM)")
plot(cryo_pred_gbm1_ras, main="Predicted Cryoturbation Coverage(GBM1)")
plot(cryo_pred_gbm2_ras, main="Predicted Cryoturbation Coverage(GBM2)")

                        