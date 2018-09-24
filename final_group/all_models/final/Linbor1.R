###############################
##REMEMBER TO CHANGE THE PARAMETERS FOR FUTURE PREDICTIONS


# linbturbation, slope processes, Linbor and
# Linbor
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



linb_glm <- glm(Linbor~  NDVI , data=data, family = "binomial") 
# library(MASS)
# step <- stepAIC(linb_glm)
# step$anova
#testing the data
summary(linb_glm)
anova(linb_glm, test= "Chisq")


# linb_auc_glm<-colAUC(linb_glm, data$Linbor, plotROC=T)
# linb_auc_gam <- c(linb_auc_gam_p[[1]])

linb_pred_glm_ras<- predict(model=linb_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(linb_pred_glm_ras, main="Predicted Linbor(GLM)")



#########


#GAM

linb_gam <- gam(Linbor~  s(NDVI, k=2), data = data, family = "binomial")
plot(linb_gam, pages=1, main= "GAM response curve for Linbor")
summary(linb_gam)
anova(linb_gam,test = "chisq")
plot(linb_gam, pages=1, main="Response curves Linbor(GAM)")
#use the calibration data to predict into the raster stack
linb_pred_gam_ras<- predict(object=ras_stack, model=linb_gam, fun=predict.gam,type="response")
plot(linb_pred_gam_ras, main="Predicted Linbor(GAM)")



########################
#GBM
linb_gbm1<-gbm(formula = Linbor~ NDVI +RR_annual + Tavg_7, data=data,
               distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)

summary(linb_gbm1)
best.iter<-gbm.perf(linb_gbm1, plot.it = T, method = "OOB")
linb_gbm1_pred<- predict.gbm(object = linb_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_linb <- cor(linb_gbm1_pred, data$Linbor, method = "spearman")


linb_pred_gbm1_ras<- predict(object=ras_stack,model=linb_gbm1, fun=predict,
                             n.trees=linb_gbm1$n.trees, type="response")
plot(linb_pred_gbm1_ras, main="Predicted Linbor(GBM1)")
par(mfrow=c(1,3))
plot.gbm(linb_gbm1, 1, best.iter,main="Response Curve NDVI")
plot.gbm(linb_gbm1, 2, best.iter, main="Response Curve Radiation")
plot.gbm(linb_gbm1, 3, best.iter, main="Response Curve Slope")

par(mfrow=c(1,1))


plot(predict.gbm(linb_gbm1, data, best.iter), data$Linbor)
lines(lowess(predict.gbm(linb_gbm1, data, best.iter), data$Linbor), col="red", lwd=3)
r_linb <-cor.test(predict.gbm(linb_gbm1, data, best.iter), data$Linbor)
r2linb <- r_linb$estimate^2
r2linb
legend("topleft", paste("cor=", round(r2linb,3)))



##############################
#GBM2 with dismo package
linb_gbm2 <- gbm.step(data=data, gbm.x =
                        c("NDVI", "RR_annual", "Tavg_7"), gbm.y = "Linbor",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="bernoulli",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(linb_gbm2, plot.it = T, method = "OOB")
linb_gbm2_pred<- predict.gbm(object = linb_gbm2, newdata = data, best.iter2,
                             type="response")
summary(linb_gbm2)
#this immediately does not work as expected, so, i'm using the next
#linb_gbm2_pred<- predict(object=data,model=linb_gbm2, fun=predict,n.trees=linb_gbm2$n.trees, type="response")

linb_pred_gbm2_ras <- predict(object=ras_stack,model=linb_gbm2, fun=predict,
                              n.trees=linb_gbm2$n.trees, type="response")
plot(linb_pred_gbm2_ras,main="Predicted Linbor(GBM2)")
###########
###For dismo package
plot(predict.gbm(linb_gbm2, data, best.iter2), data$Linbor, 
     ylab="Observed", xlab="Predicted", main="Linbor")
lines(lowess(predict.gbm(linb_gbm2, data, best.iter2), data$Linbor), col="red", lwd=3)
r_linb <-cor.test(predict.gbm(linb_gbm2, data, best.iter2), data$Linbor)
r2_linb <- r_linb$estimate^2
r2_linb
legend("topleft", paste("cor=", round(r2_linb,3)))




par(mfrow=c(2,2))
plot(linb_pred_glm_ras, main="Predicted Linbor(GLM)")
plot(linb_pred_gam_ras, main="Predicted Linbor(GAM)")
plot(linb_pred_gbm1_ras, main="Predicted Linbor(GBM1)")
plot(linb_pred_gbm2_ras, main="Predicted Linbor(GBM2)")






##########################################################3
#FUTURE IS HERE!!!!!!!!!!!!
###############################################
# where July air temperatures are expected to
# increase by 5°C and precipitation increase by 10% (
dataFuture<- data
#The below is not needed, only the raster has to be modified for future prediction
# dataFuture$Tavg_7 <- (data$Tavg_7) + 5
# dataFuture$RR_annual <- (dataFuture$RR_annual) * 1.1 #because it is increasing by 10%(new is 110%)

ras_stackFut <- ras_stack
ras_stackFut$Tavg_7 <- ras_stack$Tavg_7 + 5
ras_stackFut$RR_annual <- ras_stackFut$RR_annual*1.1 #increase by 10% i.e *110%


linb_glm_fut <- glm(Linbor~  NDVI, data=dataFuture, family = "binomial") 

#testing the dataFuture
summary(linb_glm_fut)
anova(linb_glm_fut, test= "Chisq")


# linb_auc_glm<-colAUC(linb_glm_fut, dataFuture$Linbor, plotROC=T)
# linb_auc_gam <- c(linb_auc_gam_p[[1]])

linb_pred_glm_ras_fut<- predict(model=linb_glm_fut, object = ras_stackFut, fun=predict.glm, type = "response")
plot(linb_pred_glm_ras_fut, main="Predicted Linbor(GLM)")

#########


#GAM

linb_gam_fut <- gam(Linbor~ s(NDVI, k=2), data=dataFuture, family = "binomial")
plot(linb_gam_fut, pages=1, main= "GAM response curve for Linbor")
summary(linb_gam_fut)
anova(linb_gam_fut,test = "chisq")

plot(linb_gam_fut, pages=1, main="Response curves Linbor(GAM)")

#use the calibration dataFuture to predict into the raster stack
linb_pred_gam_ras_fut<- predict(object=ras_stackFut, model=linb_gam_fut, fun=predict.gam,type="response")

plot(linb_pred_gam_ras_fut, main="Predicted Future Linbor(GAM)")



########################
#GBM
linb_gbm1_fut<-gbm(formula = Linbor~ NDVI +RR_annual + Tavg_7, data=dataFuture,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)

summary(linb_gbm1_fut)
best.iter_fut<-gbm.perf(linb_gbm1_fut, plot.it = T, method = "OOB")
linb_gbm1_pred_fut<- predict.gbm(object = linb_gbm1_fut, newdata = dataFuture, best.iter,
                                 type="response")
cor_gbm1_linb_fut <- cor(linb_gbm1_pred, dataFuture$Linbor, method = "spearman")


linb_pred_gbm1_ras_fut<- predict(object=ras_stackFut,model=linb_gbm1_fut, fun=predict,
                                 n.trees=linb_gbm1_fut$n.trees, type="response")
plot(linb_pred_gbm1_ras_fut, main="Predicted Future Linbor(GBM1)")
par(mfrow=c(1,3))
plot.gbm(linb_gbm1_fut, 1, best.iter_fut,main="Response Curve Tav_7")
plot.gbm(linb_gbm1_fut, 2, best.iter_fut, main="Response Curve Linbor")
plot.gbm(linb_gbm1_fut, 3, best.iter_fut, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(linb_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Linbor)
lines(lowess(predict.gbm(linb_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Linbor), col="red", lwd=3)
r_linb <-cor.test(predict.gbm(linb_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Linbor)
r2linb <- r_linb$estimate^2
r2linb
legend("topleft", paste("cor=", round(r2linb,3)))



##############################
#GBM2 with dismo package
linb_gbm2_fut <- gbm.step(data=dataFuture, gbm.x =
                            c("NDVI", "RR_annual", "Tavg_7"), gbm.y = "Linbor",
                          bag.fraction=0.75, learning.rate = 0.001,
                          family="bernoulli",n.trees=50, n.folds=10,
                          max.trees = 3000, tree.complexity = 6)
best.iter2_fut<-gbm.perf(linb_gbm2_fut, plot.it = T, method = "OOB")
linb_gbm2_pred<- predict.gbm(object = linb_gbm2_fut, newdata = dataFuture, best.iter2_fut,
                             type="response")
summary(linb_gbm2_fut)
#this immediately does not work as expected, so, i'm using the next
#linb_gbm2_pred<- predict(object=dataFuture,model=linb_gbm2_fut, fun=predict,n.trees=linb_gbm2_fut$n.trees, type="response")

linb_pred_gbm2_ras_fut <- predict(object=ras_stackFut,model=linb_gbm2_fut, fun=predict,
                                  n.trees=linb_gbm2_fut$n.trees, type="response")
plot(linb_pred_gbm2_ras_fut,main="Predicted Linbor(GBM2)")
###########

###For dismo package
plot(predict.gbm(linb_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Linbor, 
     ylab="Observed", xlab="Predicted", main="Linbor")
lines(lowess(predict.gbm(linb_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Linbor), col="red", lwd=3)
r_linb_fut <-cor.test(predict.gbm(linb_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Linbor)
r2_linb_fut <- r_linb_fut$estimate^2
r2_linb_fut
legend("topleft", paste("cor=", round(r2_linb_fut,3)))




par(mfrow=c(1,5))
plot(linb_pred_glm_ras, main="Present Linbor(GLM)")
plot(linb_pred_gam_ras, main="Present Linbor(GAM)")
plot(linb_pred_gbm1_ras, main="Present Linbor(GBM1)")
plot(linb_pred_gbm2_ras, main="Present Linbor(GBM2)")

# plot(linb_pred_glm_ras_fut, main="Future Linbor(GLM)")
#plot(linb_pred_gam_ras_fut, main="Future Linbor(GAM)")
# plot(linb_pred_gbm1_ras_fut, main="Future (GBM1)")
plot(linb_pred_gbm2_ras_fut, main="Future(GBM2)")

