# slopePRturbation, slope processes, NDVI and
# Slopeproc
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



slopePR_glm <- glm(Slopeproc~  Slope+ I(Slope^2) +I(Slope^3) +RR_annual
                , data=data, family = "poisson") 

#testing the data
summary(slopePR_glm)

anova(slopePR_glm, test= "Chisq")
#plot(slopePR_glm)

slopePR_pred_glm_ras<- predict(model=slopePR_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(slopePR_pred_glm_ras, main="Predicted Slope Processes(GLM)")

#########


#GAM

slopePR_gam <- gam(Slopeproc~s(Slope, k=2) + s(RR_annual, k=2), data = data, family = "poisson")
plot(slopePR_gam, pages=1, main= "GAM response curve for SP")
summary(slopePR_gam)
anova(slopePR_gam, test="Chisq")

plot(slopePR_gam, pages=1, main="Response curves SP(GAM)")
#use the calibration data to predict into the raster stack
slopePR_pred_gam_ras<- predict(object=ras_stack, model=slopePR_gam, fun=predict.gam,type="response")
plot(slopePR_pred_gam_ras, main="Predicted Slope Processes(GAM)")



########################
#GBM
slopePR_gbm1<-gbm(formula = Slopeproc~ Slope + RR_annual , data=data,
               distribution = "poisson",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)
summary(slopePR_gbm1)
best.iter<-gbm.perf(slopePR_gbm1, plot.it = T, method = "OOB")
slopePR_gbm1_pred<- predict.gbm(object = slopePR_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_slopePR <- cor(slopePR_gbm1_pred, data$Slopeproc, method = "spearman")


slopePR_pred_gbm1_ras<- predict(object=ras_stack,model=slopePR_gbm1, fun=predict,
                             n.trees=slopePR_gbm1$n.trees, type="response")
plot(slopePR_pred_gbm1_ras, main="Predicted Slope Processes(GBM1)")
par(mfrow=c(1,2))
plot.gbm(slopePR_gbm1, 1, best.iter,main="Response Curve Slope")
plot.gbm(slopePR_gbm1, 2, best.iter, main="Response Curve RR_annual")

par(mfrow=c(1,1))


plot(predict.gbm(slopePR_gbm1, data, best.iter), data$Slopeproc)
lines(lowess(predict.gbm(slopePR_gbm1, data, best.iter), data$Slopeproc), col="red", lwd=3)
r_slopePR <-cor.test(predict.gbm(slopePR_gbm1, data, best.iter), data$Slopeproc)
r2slopePR <- r_slopePR$estimate^2
r2slopePR
legend("topleft", paste("cor=", round(r2slopePR,3)))



##############################
#GBM2 with dismo package
slopePR_gbm2 <- gbm.step(data=data, gbm.x =
                        c( "Slope", "RR_annual"), gbm.y = "Slopeproc",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="poisson",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(slopePR_gbm2, plot.it = T, method = "OOB")
slopePR_gbm2_pred<- predict.gbm(object = slopePR_gbm2, newdata = data, best.iter2,
                             type="response")
summary(slopePR_gbm2)
#this immediately does not work as expected, so, i'm using the next
#slopePR_gbm2_pred<- predict(object=data,model=slopePR_gbm2, fun=predict,n.trees=slopePR_gbm2$n.trees, type="response")

slopePR_pred_gbm2_ras <- predict(object=ras_stack,model=slopePR_gbm2, fun=predict,
                              n.trees=slopePR_gbm2$n.trees, type="response")
plot(slopePR_pred_gbm2_ras)
###########
###For dismo package
plot(predict.gbm(slopePR_gbm2, data, best.iter2), data$Slopeproc, 
     ylab="Observed", xlab="Predicted", main="Slope processes")
lines(lowess(predict.gbm(slopePR_gbm2, data, best.iter2), data$Slopeproc), col="red", lwd=3)
r_slopePR <-cor.test(predict.gbm(slopePR_gbm2, data, best.iter2), data$Slopeproc)
r2_slopePR <- r_slopePR$estimate^2
r2_slopePR
legend("topleft", paste("cor=", round(r2_slopePR,3)))




par(mfrow=c(2,2))
plot(slopePR_pred_glm_ras, main="Predicted Slope Processes(GLM)")
plot(slopePR_pred_gam_ras, main="Predicted Slope Processes(GAM)")
plot(slopePR_pred_gbm1_ras, main="Predicted Slope Processes(GBM1)")
plot(slopePR_pred_gbm2_ras, main="Predicted Slope Processes(GBM2)")
par(mfrow=c(1,1))
