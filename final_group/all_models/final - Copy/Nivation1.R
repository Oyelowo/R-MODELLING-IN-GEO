# nivaturbation, slope processes, NDVI and
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



niva_glm <- glm(Nivation~  Tavg_7+ I(Tavg_7^2)+ Slope
                +  I(Slope^2), data=data, family = "poisson") 

#testing the data
summary(niva_glm)

anova(niva_glm, test= "Chisq")

summary(niva_glm)
niva_pred_glm_ras<- predict(model=niva_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(niva_pred_glm_ras, main="Predicted Nivation Index(GLM)")

#########


#GAM

niva_gam <- gam(Nivation~ s(Tavg_7, k=2)+ s(Slope, k=2)+ s(Radiation, k=2), data = data, family = "poisson")
plot(niva_gam, pages=1, main= "GAM response curve for NI")
summary(niva_gam)
plot(niva_gam, pages=1, main="Response curves NI(GAM)")
#use the calibration data to predict into the raster stack
niva_pred_gam_ras<- predict(object=ras_stack, model=niva_gam, fun=predict.gam,type="response")
plot(niva_pred_gam_ras, main="Predicted Nivation Index(GAM)")



########################
#GBM
niva_gbm1<-gbm(formula = Nivation~ Tavg_7 + NDVI  + Slope, data=data,
               distribution = "poisson",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)
summary(niva_gbm1)
best.iter<-gbm.perf(niva_gbm1, plot.it = T, method = "OOB")
niva_gbm1_pred<- predict.gbm(object = niva_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_niva <- cor(niva_gbm1_pred, data$Nivation, method = "spearman")


niva_pred_gbm1_ras<- predict(object=ras_stack,model=niva_gbm1, fun=predict,
                             n.trees=niva_gbm1$n.trees, type="response")
plot(niva_pred_gbm1_ras, main="Predicted Nivation Index(GBM1)")
par(mfrow=c(1,3))
plot.gbm(niva_gbm1, 1, best.iter,main="Response Curve Tav_7")
plot.gbm(niva_gbm1, 2, best.iter, main="Response Curve NDVI")
plot.gbm(niva_gbm1, 3, best.iter, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(niva_gbm1, data, best.iter), data$Nivation)
lines(lowess(predict.gbm(niva_gbm1, data, best.iter), data$Nivation), col="red", lwd=3)
r_niva <-cor.test(predict.gbm(niva_gbm1, data, best.iter), data$Nivation)
r2niva <- r_niva$estimate^2
r2niva
legend("topleft", paste("cor=", round(r2niva,3)))



##############################
#GBM2 with dismo package
niva_gbm2 <- gbm.step(data=data, gbm.x =
                        c("Tavg_7", "NDVI", "Slope"), gbm.y = "Nivation",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="poisson",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(niva_gbm2, plot.it = T, method = "OOB")
niva_gbm2_pred<- predict.gbm(object = niva_gbm2, newdata = data, best.iter2,
                              type="response")
summary(niva_gbm2)
#this immediately does not work as expected, so, i'm using the next
#niva_gbm2_pred<- predict(object=data,model=niva_gbm2, fun=predict,n.trees=niva_gbm2$n.trees, type="response")

niva_pred_gbm2_ras <- predict(object=ras_stack,model=niva_gbm2, fun=predict,
                              n.trees=niva_gbm2$n.trees, type="response")
plot(niva_pred_gbm2_ras)
###########
###For dismo package
plot(predict.gbm(niva_gbm2, data, best.iter2), data$Nivation, 
     ylab="Observed", xlab="Predicted", main="Nivation")
lines(lowess(predict.gbm(niva_gbm2, data, best.iter2), data$Nivation), col="red", lwd=3)
r_niva <-cor.test(predict.gbm(niva_gbm2, data, best.iter2), data$Nivation)
r2_niva <- r_niva$estimate^2
r2_niva
legend("topleft", paste("cor=", round(r2_niva,3)))




par(mfrow=c(2,2))
plot(niva_pred_glm_ras, main="Predicted Nivation Index(GLM)")
plot(niva_pred_gam_ras, main="Predicted Nivation Index(GAM)")
plot(niva_pred_gbm1_ras, main="Predicted Nivation Index(GBM1)")
plot(niva_pred_gbm2_ras, main="Predicted Nivation Index(GBM2)")

