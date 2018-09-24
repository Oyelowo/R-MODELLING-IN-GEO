###############################
##REMEMBER TO CHANGE THE PARAMETERS FOR FUTURE PREDICTIONS


# arcaturbation, slope processes, Arcalp and
# Arcalp
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



arca_glm <- glm(Arcalp~  NDVI+ I(NDVI^2), data=data, family = "binomial") 
# library(MASS)
# step <- stepAIC(arca_glm)
# step$anova
#testing the data
summary(arca_glm)
anova(arca_glm, test= "Chisq")


# arca_auc_glm<-colAUC(arca_glm, data$Arcalp, plotROC=T)
# arca_auc_gam <- c(arca_auc_gam_p[[1]])

arca_pred_glm_ras<- predict(model=arca_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(arca_pred_glm_ras, main="Predicted Arcalp(GLM)")



#########


#GAM

arca_gam <- gam(Arcalp~  s(NDVI, k=2) , data = data, family = "binomial")
plot(arca_gam, pages=1, main= "GAM response curve for Arcalp")
summary(arca_gam)
anova(arca_gam,test = "chisq")
plot(arca_gam, pages=1, main="Response curves Arcalp(GAM)")
#use the calibration data to predict into the raster stack
arca_pred_gam_ras<- predict(object=ras_stack, model=arca_gam, fun=predict.gam,type="response")
plot(arca_pred_gam_ras, main="Predicted Arcalp(GAM)")



########################
#GBM
arca_gbm1<-gbm(formula = Arcalp~ NDVI +RR_annual + Tavg_7 + Radiation + Slope, data=data,
               distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)

summary(arca_gbm1)
best.iter<-gbm.perf(arca_gbm1, plot.it = T, method = "OOB")
arca_gbm1_pred<- predict.gbm(object = arca_gbm1, newdata = data, best.iter,
                             type="response")
cor_gbm1_arca <- cor(arca_gbm1_pred, data$Arcalp, method = "spearman")


arca_pred_gbm1_ras<- predict(object=ras_stack,model=arca_gbm1, fun=predict,
                             n.trees=arca_gbm1$n.trees, type="response")
plot(arca_pred_gbm1_ras, main="Predicted Arcalp(GBM1)")
par(mfrow=c(1,5))
plot.gbm(arca_gbm1, 1, best.iter,main="Response Curve NDVI")
plot.gbm(arca_gbm1, 2, best.iter, main="Response Curve RR_annual")
plot.gbm(arca_gbm1, 3, best.iter, main="Response Curve Tavg_7")
plot.gbm(arca_gbm1, 4, best.iter, main="Response Curve Radiation")
plot.gbm(arca_gbm1, 5, best.iter, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(arca_gbm1, data, best.iter), data$Arcalp)
lines(lowess(predict.gbm(arca_gbm1, data, best.iter), data$Arcalp), col="red", lwd=3)
r_arca <-cor.test(predict.gbm(arca_gbm1, data, best.iter), data$Arcalp)
r2arca <- r_arca$estimate^2
r2arca
legend("topleft", paste("cor=", round(r2arca,3)))



##############################
#GBM2 with dismo package
arca_gbm2 <- gbm.step(data=data, gbm.x =
                        c("NDVI", "RR_annual", "Tavg_7","Radiation", "Slope"), gbm.y = "Arcalp",
                      bag.fraction=0.75, learning.rate = 0.001,
                      family="bernoulli",n.trees=50, n.folds=10,
                      max.trees = 3000, tree.complexity = 6)
best.iter2<-gbm.perf(arca_gbm2, plot.it = T, method = "OOB")
arca_gbm2_pred<- predict.gbm(object = arca_gbm2, newdata = data, best.iter2,
                             type="response")
summary(arca_gbm2)
#this immediately does not work as expected, so, i'm using the next
#arca_gbm2_pred<- predict(object=data,model=arca_gbm2, fun=predict,n.trees=arca_gbm2$n.trees, type="response")

arca_pred_gbm2_ras <- predict(object=ras_stack,model=arca_gbm2, fun=predict,
                              n.trees=arca_gbm2$n.trees, type="response")
plot(arca_pred_gbm2_ras,main="Predicted Arcalp(GBM2)")
###########
###For dismo package
plot(predict.gbm(arca_gbm2, data, best.iter2), data$Arcalp, 
     ylab="Observed", xlab="Predicted", main="Arcalp")
lines(lowess(predict.gbm(arca_gbm2, data, best.iter2), data$Arcalp), col="red", lwd=3)
r_arca <-cor.test(predict.gbm(arca_gbm2, data, best.iter2), data$Arcalp)
r2_arca <- r_arca$estimate^2
r2_arca
legend("topleft", paste("cor=", round(r2_arca,3)))




par(mfrow=c(2,2))
plot(arca_pred_glm_ras, main="Predicted Arcalp(GLM)")
plot(arca_pred_gam_ras, main="Predicted Arcalp(GAM)")
plot(arca_pred_gbm1_ras, main="Predicted Arcalp(GBM1)")
plot(arca_pred_gbm2_ras, main="Predicted Arcalp(GBM2)")






##########################################################3
#FUTURE IS HERE!!!!!!!!!!!!
###############################################
# where July air temperatures are expected to
# increase by 5°C and precipitation increase by 10% (
dataFuture<- data
# dataFuture$Tavg_7 <- (data$Tavg_7) + 5
# dataFuture$RR_annual <- (dataFuture$RR_annual) * 1.1 #because it is increasing by 10%(new is 110%)

ras_stackFut <- ras_stack
ras_stackFut$Tavg_7 <- ras_stack$Tavg_7 + 5
ras_stackFut$RR_annual <- ras_stackFut$RR_annual*1.1 #increase by 10% i.e *110%


arca_glm_fut <- glm(Arcalp~ NDVI + I(NDVI^2), data=dataFuture, family = "binomial") 

#testing the dataFuture
summary(arca_glm_fut)
anova(arca_glm_fut, test= "Chisq")


# arca_auc_glm<-colAUC(arca_glm_fut, dataFuture$Arcalp, plotROC=T)
# arca_auc_gam <- c(arca_auc_gam_p[[1]])

arca_pred_glm_ras_fut<- predict(model=arca_glm_fut, object = ras_stackFut, fun=predict.glm, type = "response")
plot(arca_pred_glm_ras_fut, main="Predicted Arcalp(GLM)")

#########


#GAM

arca_gam_fut <- gam(Arcalp~ s(NDVI, k=2) , data=dataFuture, family = "binomial")

plot(arca_gam_fut, pages=1, main= "GAM response curve for Arcalp")
summary(arca_gam_fut)
anova(arca_gam_fut,test = "chisq")

plot(arca_gam_fut, pages=1, main="Response curves Arcalp(GAM)")

#use the calibration dataFuture to predict into the raster stack
arca_pred_gam_ras_fut<- predict(object=ras_stackFut, model=arca_gam_fut, fun=predict.gam,type="response")

plot(arca_pred_gam_ras_fut, main="Predicted Future Arcalp(GAM)")



########################
#GBM
arca_gbm1_fut<-gbm(formula = Arcalp~ NDVI +RR_annual + Tavg_7 + Radiation + Slope, data=dataFuture,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)

summary(arca_gbm1_fut)
best.iter_fut<-gbm.perf(arca_gbm1_fut, plot.it = T, method = "OOB")
arca_gbm1_pred_fut<- predict.gbm(object = arca_gbm1_fut, newdata = dataFuture, best.iter,
                                 type="response")
cor_gbm1_arca_fut <- cor(arca_gbm1_pred, dataFuture$Arcalp, method = "spearman")


arca_pred_gbm1_ras_fut<- predict(object=ras_stackFut,model=arca_gbm1_fut, fun=predict,
                                 n.trees=arca_gbm1_fut$n.trees, type="response")
plot(arca_pred_gbm1_ras_fut, main="Predicted Future Arcalp(GBM1)")
par(mfrow=c(1,3))
plot.gbm(arca_gbm1_fut, 1, best.iter_fut,main="Response Curve Tav_7")
plot.gbm(arca_gbm1_fut, 2, best.iter_fut, main="Response Curve Arcalp")
plot.gbm(arca_gbm1_fut, 3, best.iter_fut, main="Response Curve slope")
par(mfrow=c(1,1))


plot(predict.gbm(arca_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Arcalp)
lines(lowess(predict.gbm(arca_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Arcalp), col="red", lwd=3)
r_arca <-cor.test(predict.gbm(arca_gbm1_fut, dataFuture, best.iter_fut), dataFuture$Arcalp)
r2arca <- r_arca$estimate^2
r2arca
legend("topleft", paste("cor=", round(r2arca,3)))



##############################
#GBM2 with dismo package
arca_gbm2_fut <- gbm.step(data=dataFuture, gbm.x =
                            c("NDVI", "RR_annual", "Tavg_7","Radiation", "Slope"), gbm.y = "Arcalp",
                          bag.fraction=0.75, learning.rate = 0.001,
                          family="bernoulli",n.trees=50, n.folds=10,
                          max.trees = 3000, tree.complexity = 6)
best.iter2_fut<-gbm.perf(arca_gbm2_fut, plot.it = T, method = "OOB")
arca_gbm2_pred<- predict.gbm(object = arca_gbm2_fut, newdata = dataFuture, best.iter2_fut,
                             type="response")
summary(arca_gbm2_fut)
#this immediately does not work as expected, so, i'm using the next
#arca_gbm2_pred<- predict(object=dataFuture,model=arca_gbm2_fut, fun=predict,n.trees=arca_gbm2_fut$n.trees, type="response")

arca_pred_gbm2_ras_fut <- predict(object=ras_stackFut,model=arca_gbm2_fut, fun=predict,
                                  n.trees=arca_gbm2_fut$n.trees, type="response")
plot(arca_pred_gbm2_ras_fut,main="Predicted Arcalp(GBM2)")
###########

###For dismo package
plot(predict.gbm(arca_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Arcalp, 
     ylab="Observed", xlab="Predicted", main="Arcalp")
lines(lowess(predict.gbm(arca_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Arcalp), col="red", lwd=3)
r_arca_fut <-cor.test(predict.gbm(arca_gbm2_fut, dataFuture, best.iter2_fut), dataFuture$Arcalp)
r2_arca_fut <- r_arca_fut$estimate^2
r2_arca_fut
legend("topleft", paste("cor=", round(r2_arca_fut,3)))




par(mfrow=c(1,5))
plot(arca_pred_glm_ras, main="Present Arcalp(GLM)")
plot(arca_pred_gam_ras, main="Present Arcalp(GAM)")
plot(arca_pred_gbm1_ras, main="Present Arcalp(GBM1)")
plot(arca_pred_gbm2_ras, main="Present Arcalp(GBM2)")

#plot(arca_pred_glm_ras_fut, main="Future Arcalp(GLM)")
# plot(arca_pred_gam_ras_fut, main="Future Arcalp(GAM)")
plot(arca_pred_gbm1_ras_fut, main="Future (GBM1)")
# plot(arca_pred_gbm2_ras_fut, main="Future(GBM2)")

