# phycturbation, slope processes, Phycae and
# Phycae
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



phyc_glm <- glm(Phycae~  RR_annual + I(RR_annual^2), data=data, family = "binomial") 

#testing the data
summary(phyc_glm)
anova(phyc_glm, test= "Chisq")


# phyc_auc_glm<-colAUC(phyc_glm, data$Phycae, plotROC=T)
# phyc_auc_gam <- c(phyc_auc_gam_p[[1]])

phyc_pred_glm_ras<- predict(model=phyc_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(phyc_pred_glm_ras, main="Predicted Phycae(GLM)")

#########


#GAM

phyc_gam <- gam(Phycae~ s(RR_annual, k=2), data = data, family = "binomial")
plot(phyc_gam, pages=1, main= "GAM response curve for Phycae")
summary(phyc_gam)
anova(phyc_gam,test = "chisq")
plot(phyc_gam, pages=1, main="Response curves Phycae(GAM)")
#use the calibration data to predict into the raster stack
phyc_pred_gam_ras<- predict(object=ras_stack, model=phyc_gam, fun=predict.gam,type="response")
plot(phyc_pred_gam_ras, main="Predicted Phycae(GAM)")



########################
#GBM
phyc_gbm1<-gbm(formula = Phycae~ RR_annual + NDVI + Tavg_7 + Radiation + Slope, data=data,
                distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                bag.fraction = 0.75)

summary(phyc_gbm1)
best.iter<-gbm.perf(phyc_gbm1, plot.it = T, method = "OOB")
phyc_gbm1_pred<- predict.gbm(object = phyc_gbm1, newdata = data, best.iter,
                              type="response")
cor_gbm1_phyc <- cor(phyc_gbm1_pred, data$Phycae, method = "spearman")


phyc_pred_gbm1_ras<- predict(object=ras_stack,model=phyc_gbm1, fun=predict,
                              n.trees=phyc_gbm1$n.trees, type="response")
plot(phyc_pred_gbm1_ras, main="Predicted Phycae(GBM1)")
par(mfrow=c(1,3))
plot.gbm(phyc_gbm1, 1, best.iter,main="Response Curve Tav_7")
plot.gbm(phyc_gbm1, 2, best.iter, main="Response Curve Phycae")
plot.gbm(phyc_gbm1, 3, best.iter, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(phyc_gbm1, data, best.iter), data$Phycae)
lines(lowess(predict.gbm(phyc_gbm1, data, best.iter), data$Phycae), col="red", lwd=3)
r_phyc <-cor.test(predict.gbm(phyc_gbm1, data, best.iter), data$Phycae)
r2phyc <- r_phyc$estimate^2
r2phyc
legend("topleft", paste("cor=", round(r2phyc,3)))



##############################
#GBM2 with dismo package
phyc_gbm2 <- gbm.step(data=data, gbm.x =
                         c("RR_annual","NDVI", "Tavg_7","Radiation","Slope"), gbm.y = "Phycae",
                       bag.fraction=0.75, learning.rate = 0.001,
                       family="bernoulli",n.trees=50, n.folds=10,
                       max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(phyc_gbm2, plot.it = T, method = "OOB")
phyc_gbm2_pred<- predict.gbm(object = phyc_gbm2, newdata = data, best.iter2,
                              type="response")
summary(phyc_gbm2)
#this immediately does not work as expected, so, i'm using the next
#phyc_gbm2_pred<- predict(object=data,model=phyc_gbm2, fun=predict,n.trees=phyc_gbm2$n.trees, type="response")

phyc_pred_gbm2_ras <- predict(object=ras_stack,model=phyc_gbm2, fun=predict,
                               n.trees=phyc_gbm2$n.trees, type="response")
plot(phyc_pred_gbm2_ras,main="Predicted Phycae(GBM2)")
###########
###For dismo package
plot(predict.gbm(phyc_gbm2, data, best.iter2), data$Phycae, 
     ylab="Observed", xlab="Predicted", main="Phycae")
lines(lowess(predict.gbm(phyc_gbm2, data, best.iter2), data$Phycae), col="red", lwd=3)
r_phyc <-cor.test(predict.gbm(phyc_gbm2, data, best.iter2), data$Phycae)
r2_phyc <- r_phyc$estimate^2
r2_phyc
legend("topleft", paste("cor=", round(r2_phyc,3)))




par(mfrow=c(2,2))
plot(phyc_pred_glm_ras, main="Predicted Phycae(GLM)")
plot(phyc_pred_gam_ras, main="Predicted Phycae(GAM)")
plot(phyc_pred_gbm1_ras, main="Predicted Phycae(GBM1)")
plot(phyc_pred_gbm2_ras, main="Predicted Phycae(GBM2)")






##########################################################3
#FUTURE IS HERE!!!!!!!!!!!!
###############################################
# where July air temperatures are expected to
# increase by 5°C and precipitation increase by 10% (
dataFuture<- data
dataFuture$Tavg_7 <- (data$Tavg_7) + 5
dataFuture$RR_annual <- (dataFuture$RR_annual) * 1.1 #because it is increasing by 10%(new is 110%)

ras_stackFut <- ras_stack
ras_stackFut$Tavg_7 <- ras_stack$Tavg_7 + 5
ras_stackFut$RR_annual <- ras_stackFut$RR_annual*1.1 #increase by 10% i.e *110%


phyc_glm_fut <- glm(Phycae~  RR_annual + I(RR_annual^2), data=dataFuture, family = "binomial") 

#testing the dataFuture
summary(phyc_glm_fut)
anova(phyc_glm_fut, test= "Chisq")


# phyc_auc_glm<-colAUC(phyc_glm_fut, dataFuture$Phycae, plotROC=T)
# phyc_auc_gam <- c(phyc_auc_gam_p[[1]])

phyc_pred_glm_ras_fut<- predict(model=phyc_glm_fut, object = ras_stackFut, fun=predict.glm, type = "response")
plot(phyc_pred_glm_ras_fut, main="Predicted Phycae(GLM)")

#########


#GAM

phyc_gam_fut <- gam(Phycae~ s(RR_annual, k=2), data=dataFuture, family = "binomial")
plot(phyc_gam_fut, pages=1, main= "GAM response curve for Phycae")
summary(phyc_gam_fut)
anova(phyc_gam_fut,test = "chisq")

plot(phyc_gam_fut, pages=1, main="Response curves Phycae(GAM)")

#use the calibration dataFuture to predict into the raster stack
phyc_pred_gam_ras_fut<- predict(object=ras_stackFut, model=phyc_gam_fut, fun=predict.gam,type="response")

plot(phyc_pred_gam_ras_fut, main="Predicted Future Phycae(GAM)")



########################
#GBM
phyc_gbm1_fut<-gbm(formula = Phycae~ RR_annual + NDVI + Tavg_7 + Radiation + Slope, data=dataFuture,
               distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)

summary(phyc_gbm1_fut)
best.iter_fut<-gbm.perf(phyc_gbm1_fut, plot.it = T, method = "OOB")
phyc_gbm1_pred_fut<- predict.gbm(object = phyc_gbm1_fut, newdata = dataFuture, best.iter,
                             type="response")
cor_gbm1_phyc_fut <- cor(phyc_gbm1_pred, dataFuture$Phycae, method = "spearman")


phyc_pred_gbm1_ras_fut<- predict(object=ras_stackFut,model=phyc_gbm1_fut, fun=predict,
                             n.trees=phyc_gbm1_fut$n.trees, type="response")
plot(phyc_pred_gbm1_ras_fut, main="Predicted Future Phycae(GBM1)")
par(mfrow=c(1,3))
plot.gbm(phyc_gbm1_fut, 1, best.iter_fut,main="Response Curve Tav_7")
plot.gbm(phyc_gbm1_fut, 2, best.iter_fut, main="Response Curve Phycae")
plot.gbm(phyc_gbm1_fut, 3, best.iter_fut, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(phyc_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Phycae)
lines(lowess(predict.gbm(phyc_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Phycae), col="red", lwd=3)
r_phyc <-cor.test(predict.gbm(phyc_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Phycae)
r2phyc <- r_phyc$estimate^2
r2phyc
legend("topleft", paste("cor=", round(r2phyc,3)))



##############################
#GBM2 with dismo package
phyc_gbm2_fut <- gbm.step(data=dataFuture, gbm.x =
   c("RR_annual","NDVI", "Tavg_7","Radiation","Slope"), gbm.y = "Phycae",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="bernoulli",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2_fut<-gbm.perf(phyc_gbm2_fut, plot.it = T, method = "OOB")
phyc_gbm2_pred<- predict.gbm(object = phyc_gbm2_fut, newdata = dataFuture, best.iter2_fut,
                             type="response")
summary(phyc_gbm2_fut)
#this immediately does not work as expected, so, i'm using the next
#phyc_gbm2_pred<- predict(object=dataFuture,model=phyc_gbm2_fut, fun=predict,n.trees=phyc_gbm2_fut$n.trees, type="response")

phyc_pred_gbm2_ras_fut <- predict(object=ras_stackFut,model=phyc_gbm2_fut, fun=predict,
                              n.trees=phyc_gbm2_fut$n.trees, type="response")
plot(phyc_pred_gbm2_ras_fut,main="Predicted Phycae(GBM2)")
###########

###For dismo package
plot(predict.gbm(phyc_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Phycae, 
     ylab="Observed", xlab="Predicted", main="Phycae")
lines(lowess(predict.gbm(phyc_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Phycae), col="red", lwd=3)
r_phyc_fut <-cor.test(predict.gbm(phyc_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Phycae)
r2_phyc_fut <- r_phyc_fut$estimate^2
r2_phyc_fut
legend("topleft", paste("cor=", round(r2_phyc_fut,3)))




par(mfrow=c(2,4))
plot(phyc_pred_glm_ras, main="Present Phycae(GLM)")
plot(phyc_pred_gam_ras, main="Present Phycae(GAM)")
plot(phyc_pred_gbm1_ras, main="Present Phycae(GBM1)")
plot(phyc_pred_gbm2_ras, main="Present Phycae(GBM2)")

plot(phyc_pred_glm_ras_fut, main="Future Phycae(GLM)")
plot(phyc_pred_gam_ras_fut, main="Future Phycae(GAM)")
plot(phyc_pred_gbm1_ras_fut, main="Future (GBM1)")
plot(phyc_pred_gbm2_ras_fut, main="Future(GBM2)")
