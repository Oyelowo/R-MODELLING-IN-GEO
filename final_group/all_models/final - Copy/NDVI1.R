# ndviMturbation, slope processes, NDVI and
# NDVI
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



ndviM_glm <- glm(NDVI~  RR_annual + Slope+ Radiation, data=data, family = "gaussian") 

#testing the data
summary(ndviM_glm)

anova(ndviM_glm, test= "F")
ndviM_pred_glm_ras<- predict(model=ndviM_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(ndviM_pred_glm_ras, main="Predicted NDVI(GLM)")

#########


#GAM

ndviM_gam <- gam(NDVI~ s(RR_annual, k=2)+ s(Slope, k=2)+ s(Radiation, k=2), data = data, family = "gaussian")
plot(ndviM_gam, pages=1, main= "GAM response curve for NDVI")
summary(ndviM_gam)
anova(ndviM_gam,test = "chisq")
plot(ndviM_gam, pages=1, main="Response curves NDVI(GAM)")
#use the calibration data to predict into the raster stack
ndviM_pred_gam_ras<- predict(object=ras_stack, model=ndviM_gam, fun=predict.gam,type="response")
plot(ndviM_pred_gam_ras, main="Predicted NDVI(GAM)")



########################
#GBM
ndviM_gbm1<-gbm(formula = NDVI~ RR_annual + Radiation + Slope, data=data,
               distribution = "gaussian",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)
summary(ndviM_gbm1)
best.iter<-gbm.perf(ndviM_gbm1, plot.it = T, method = "OOB")
ndviM_gbm1_pred<- predict.gbm(object = ndviM_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_ndviM <- cor(ndviM_gbm1_pred, data$NDVI, method = "spearman")


ndviM_pred_gbm1_ras<- predict(object=ras_stack,model=ndviM_gbm1, fun=predict,
                             n.trees=ndviM_gbm1$n.trees, type="response")
plot(ndviM_pred_gbm1_ras, main="Predicted NDVI(GBM1)")
par(mfrow=c(1,3))
plot.gbm(ndviM_gbm1, 1, best.iter,main="Response Curve Tav_7")
plot.gbm(ndviM_gbm1, 2, best.iter, main="Response Curve NDVI")
plot.gbm(ndviM_gbm1, 3, best.iter, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(ndviM_gbm1, data, best.iter), data$NDVI)
lines(lowess(predict.gbm(ndviM_gbm1, data, best.iter), data$NDVI), col="red", lwd=3)
r_ndviM <-cor.test(predict.gbm(ndviM_gbm1, data, best.iter), data$NDVI)
r2ndviM <- r_ndviM$estimate^2
r2ndviM
legend("topleft", paste("cor=", round(r2ndviM,3)))



##############################
#GBM2 with dismo package
ndviM_gbm2 <- gbm.step(data=data, gbm.x =
                        c("RR_annual", "Radiation", "Slope"), gbm.y = "NDVI",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="gaussian",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(ndviM_gbm2, plot.it = T, method = "OOB")
ndviM_gbm2_pred<- predict.gbm(object = ndviM_gbm2, newdata = data, best.iter2,
                             type="response")
summary(ndviM_gbm2)
#this immediately does not work as expected, so, i'm using the next
#ndviM_gbm2_pred<- predict(object=data,model=ndviM_gbm2, fun=predict,n.trees=ndviM_gbm2$n.trees, type="response")

ndviM_pred_gbm2_ras <- predict(object=ras_stack,model=ndviM_gbm2, fun=predict,
                              n.trees=ndviM_gbm2$n.trees, type="response")
plot(ndviM_pred_gbm2_ras,main="Predicted NDVI(GBM2)")
###########
###For dismo package
plot(predict.gbm(ndviM_gbm2, data, best.iter2), data$NDVI, 
     ylab="Observed", xlab="Predicted", main="NDVI")
lines(lowess(predict.gbm(ndviM_gbm2, data, best.iter2), data$NDVI), col="red", lwd=3)
r_ndviM <-cor.test(predict.gbm(ndviM_gbm2, data, best.iter2), data$NDVI)
r2_ndviM <- r_ndviM$estimate^2
r2_ndviM
legend("topleft", paste("cor=", round(r2_ndviM,3)))




par(mfrow=c(2,2))
plot(ndviM_pred_glm_ras, main="Predicted NDVI(GLM)")
plot(ndviM_pred_gam_ras, main="Predicted NDVI(GAM)")
plot(ndviM_pred_gbm1_ras, main="Predicted NDVI(GBM1)")
plot(ndviM_pred_gbm2_ras, main="Predicted NDVI(GBM2)")

