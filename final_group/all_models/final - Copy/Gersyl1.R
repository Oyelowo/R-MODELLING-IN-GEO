###############################
##REMEMBER TO CHANGE THE PARAMETERS FOR FUTURE PREDICTIONS


# gersturbation, slope processes, Gersyl and
# Gersyl
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



gers_glm <- glm(Gersyl~  NDVI , data=data, family = "binomial") 
# library(MASS)
# step <- stepAIC(gers_glm)
# step$anova
#testing the data
summary(gers_glm)
anova(gers_glm, test= "Chisq")


# gers_auc_glm<-colAUC(gers_glm, data$Gersyl, plotROC=T)
# gers_auc_gam <- c(gers_auc_gam_p[[1]])

gers_pred_glm_ras<- predict(model=gers_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(gers_pred_glm_ras, main="Predicted Gersyl(GLM)")



#########


#GAM

gers_gam <- gam(Gersyl~  s(NDVI, k=2), data = data, family = "binomial")
plot(gers_gam, pages=1, main= "GAM response curve for Gersyl")
summary(gers_gam)
anova(gers_gam,test = "chisq")
plot(gers_gam, pages=1, main="Response curves Gersyl(GAM)")
#use the calibration data to predict into the raster stack
gers_pred_gam_ras<- predict(object=ras_stack, model=gers_gam, fun=predict.gam,type="response")
plot(gers_pred_gam_ras, main="Predicted Gersyl(GAM)")



########################
#GBM
gers_gbm1<-gbm(formula = Gersyl~ NDVI + Tavg_7 + RR_annual, data=data,
               distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)

summary(gers_gbm1)
best.iter<-gbm.perf(gers_gbm1, plot.it = T, method = "OOB")
gers_gbm1_pred<- predict.gbm(object = gers_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_gers <- cor(gers_gbm1_pred, data$Gersyl, method = "spearman")


gers_pred_gbm1_ras<- predict(object=ras_stack,model=gers_gbm1, fun=predict,
                             n.trees=gers_gbm1$n.trees, type="response")
plot(gers_pred_gbm1_ras, main="Predicted Gersyl(GBM1)")
par(mfrow=c(1,3))
plot.gbm(gers_gbm1, 1, best.iter,main="Response Curve NDVI")
plot.gbm(gers_gbm1, 2, best.iter, main="Response Curve Radiation")
plot.gbm(gers_gbm1, 3, best.iter, main="Response Curve Slope")

par(mfrow=c(1,1))


plot(predict.gbm(gers_gbm1, data, best.iter), data$Gersyl)
lines(lowess(predict.gbm(gers_gbm1, data, best.iter), data$Gersyl), col="red", lwd=3)
r_gers <-cor.test(predict.gbm(gers_gbm1, data, best.iter), data$Gersyl)
r2gers <- r_gers$estimate^2
r2gers
legend("topleft", paste("cor=", round(r2gers,3)))



##############################
#GBM2 with dismo package
gers_gbm2 <- gbm.step(data=data, gbm.x =
                        c("NDVI",  "Tavg_7", "RR_annual"), gbm.y = "Gersyl",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="bernoulli",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(gers_gbm2, plot.it = T, method = "OOB")
gers_gbm2_pred<- predict.gbm(object = gers_gbm2, newdata = data, best.iter2,
                             type="response")
summary(gers_gbm2)
#this immediately does not work as expected, so, i'm using the next
#gers_gbm2_pred<- predict(object=data,model=gers_gbm2, fun=predict,n.trees=gers_gbm2$n.trees, type="response")

gers_pred_gbm2_ras <- predict(object=ras_stack,model=gers_gbm2, fun=predict,
                              n.trees=gers_gbm2$n.trees, type="response")
plot(gers_pred_gbm2_ras,main="Predicted Gersyl(GBM2)")
###########
###For dismo package
plot(predict.gbm(gers_gbm2, data, best.iter2), data$Gersyl, 
     ylab="Observed", xlab="Predicted", main="Gersyl")
lines(lowess(predict.gbm(gers_gbm2, data, best.iter2), data$Gersyl), col="red", lwd=3)
r_gers <-cor.test(predict.gbm(gers_gbm2, data, best.iter2), data$Gersyl)
r2_gers <- r_gers$estimate^2
r2_gers
legend("topleft", paste("cor=", round(r2_gers,3)))




par(mfrow=c(2,2))
plot(gers_pred_glm_ras, main="Predicted Gersyl(GLM)")
plot(gers_pred_gam_ras, main="Predicted Gersyl(GAM)")
plot(gers_pred_gbm1_ras, main="Predicted Gersyl(GBM1)")
plot(gers_pred_gbm2_ras, main="Predicted Gersyl(GBM2)")






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


gers_glm_fut <- glm(Gersyl~  NDVI, data=dataFuture, family = "binomial") 

#testing the dataFuture
summary(gers_glm_fut)
anova(gers_glm_fut, test= "Chisq")


# gers_auc_glm<-colAUC(gers_glm_fut, dataFuture$Gersyl, plotROC=T)
# gers_auc_gam <- c(gers_auc_gam_p[[1]])

gers_pred_glm_ras_fut<- predict(model=gers_glm_fut, object = ras_stackFut, fun=predict.glm, type = "response")
plot(gers_pred_glm_ras_fut, main="Predicted Gersyl(GLM)")

#########


#GAM

gers_gam_fut <- gam(Gersyl~ s(NDVI, k=2), data=dataFuture, family = "binomial")
plot(gers_gam_fut, pages=1, main= "GAM response curve for Gersyl")
summary(gers_gam_fut)
anova(gers_gam_fut,test = "chisq")

plot(gers_gam_fut, pages=1, main="Response curves Gersyl(GAM)")

#use the calibration dataFuture to predict into the raster stack
gers_pred_gam_ras_fut<- predict(object=ras_stackFut, model=gers_gam_fut, fun=predict.gam,type="response")

plot(gers_pred_gam_ras_fut, main="Predicted Future Gersyl(GAM)")



########################
#GBM
gers_gbm1_fut<-gbm(formula = Gersyl~ NDVI + Tavg_7 + RR_annual, data=dataFuture,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)

summary(gers_gbm1_fut)
best.iter_fut<-gbm.perf(gers_gbm1_fut, plot.it = T, method = "OOB")
gers_gbm1_pred_fut<- predict.gbm(object = gers_gbm1_fut, newdata = dataFuture, best.iter,
                                 type="response")
cor_gbm1_gers_fut <- cor(gers_gbm1_pred_fut, dataFuture$Gersyl, method = "spearman")


gers_pred_gbm1_ras_fut<- predict(object=ras_stackFut,model=gers_gbm1_fut, fun=predict,
                                 n.trees=gers_gbm1_fut$n.trees, type="response")
plot(gers_pred_gbm1_ras_fut, main="Predicted Future Gersyl(GBM1)")
par(mfrow=c(1,3))
plot.gbm(gers_gbm1_fut, 1, best.iter_fut,main="Response Curve Tav_7")
plot.gbm(gers_gbm1_fut, 2, best.iter_fut, main="Response Curve Gersyl")
plot.gbm(gers_gbm1_fut, 3, best.iter_fut, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(gers_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Gersyl)
lines(lowess(predict.gbm(gers_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Gersyl), col="red", lwd=3)
r_gers <-cor.test(predict.gbm(gers_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Gersyl)
r2gers <- r_gers$estimate^2
r2gers
legend("topleft", paste("cor=", round(r2gers,3)))



##############################
#GBM2 with dismo package
gers_gbm2_fut <- gbm.step(data=dataFuture, gbm.x =
                            c("NDVI",  "Tavg_7", "RR_annual"), gbm.y = "Gersyl",
                          bag.fraction=0.75, learning.rate = 0.001,
                          family="bernoulli",n.trees=50, n.folds=10,
                          max.trees = 3000, tree.complexity = 6)
best.iter2_fut<-gbm.perf(gers_gbm2_fut, plot.it = T, method = "OOB")
gers_gbm2_pred<- predict.gbm(object = gers_gbm2_fut, newdata = dataFuture, best.iter2_fut,
                             type="response")
summary(gers_gbm2_fut)
#this immediately does not work as expected, so, i'm using the next
#gers_gbm2_pred<- predict(object=dataFuture,model=gers_gbm2_fut, fun=predict,n.trees=gers_gbm2_fut$n.trees, type="response")

gers_pred_gbm2_ras_fut <- predict(object=ras_stackFut,model=gers_gbm2_fut, fun=predict,
                                  n.trees=gers_gbm2_fut$n.trees, type="response")
plot(gers_pred_gbm2_ras_fut,main="Predicted Gersyl(GBM2)")
###########

###For dismo package
plot(predict.gbm(gers_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Gersyl, 
     ylab="Observed", xlab="Predicted", main="Gersyl")
lines(lowess(predict.gbm(gers_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Gersyl), col="red", lwd=3)
r_gers_fut <-cor.test(predict.gbm(gers_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Gersyl)
r2_gers_fut <- r_gers_fut$estimate^2
r2_gers_fut
legend("topleft", paste("cor=", round(r2_gers_fut,3)))




par(mfrow=c(2,4))
plot(gers_pred_glm_ras, main="Present Gersyl(GLM)")
plot(gers_pred_gam_ras, main="Present Gersyl(GAM)")
plot(gers_pred_gbm1_ras, main="Present Gersyl(GBM1)")
plot(gers_pred_gbm2_ras, main="Present Gersyl(GBM2)")

plot(gers_pred_glm_ras_fut, main="Future Gersyl(GLM)")
plot(gers_pred_gam_ras_fut, main="Future Gersyl(GAM)")
plot(gers_pred_gbm1_ras_fut, main="Future (GBM1)")
plot(gers_pred_gbm2_ras_fut, main="Future(GBM2)")

