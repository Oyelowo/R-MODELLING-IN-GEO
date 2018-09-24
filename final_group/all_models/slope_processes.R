# Cryoturbation, slope processes, NDVI and
# Nivation
#########################################################################
#SLOPE PROCESSES
#######################################################################
rm(list = ls())
#moisture and nutrient on plant growth
#incoming solar radiation and and air temperature across diffe  rent elevations
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

#install.packages("gbm")
#install.packages("r
#install.packages("effects")

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

#check cryoturbation script for the correlation matrix and the ggpairs plot
#of the predictors and the response variables



#####################################################################################3
#CRYOTURBATION
#I decided to use the below because they have correlations below 0.7
#Next, i check if they are curvilinear and also check if they are interrelated

##############CHECKING INTERACTIONS BETWEEN PREDICTORS################
require(effects); require(viridis)
m <-lm(Slopeproc ~Radiation*RR_annual, data=data)

plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
#radiation has a high effect on slopePrturbation at high annual rainfall

m <-lm(Slopeproc ~Radiation*Tavg_7, data=data)

plot(effect(term="Radiation:Tavg_7", mod=m,xlevels=10, multiline=T, colors = 2, lwd=2))


#After testing the model, using anova and the summary of the model(which uses t-test), we found out that
# that the interactions and other second orders are insignificant.
#however, because of the discrepancy between the significance test of the anova and the summary of the
#mode, I will be building two models with and without the T_Avg and use wilcox test to see
#if it improves the model significantly.

#note: dont use auc for poisson and gaussiam because it is meant for binary variables.

slopePr_glm <- glm(Slopeproc ~Altitude + I(Altitude^2) + NDVI+ I(NDVI^2)+ Radiation+
                     I(Radiation^2)+Tavg_7+ I(Tavg_7^2) + RR_annual+I(RR_annual^2)+ Slope +  I(Slope^2), data=data, family = "poisson")


#After removing the insignificant predictors manually step-by-step, I tried the stepwise
#regression
library(MASS)
step_slopePr_glm<-stepAIC(slopePr_glm, direction = "both")
step_slopePr_glm$anova
slopePr_pred<- predict(object=ras_stack, model=slopePr_glm, fun=predict, type = "response")
plot(slopePr_pred)

# 
# #final model for slopeProcesses
slopePr_glm <- glm(Slopeproc ~RR_annual + Slope + 
                     I(Slope^2), data=data, family = "poisson")
#test for significance of the predictors.
anova (slopePr_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for poisson and binomial
#summary of the model, which uses t-test for it's significance.'
summary(slopePr_glm)
plot(slopePr_glm, pages=1)

# #based on the test from the summary and checking the correclation of the predicted with observed,
# #Tavg_7 did not improve the prediction and hence, not necessary.
# 
# #########
# #now, predict into the raster
# #to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
# #Will also try the LOOCV(i.e leave one out cross validation) method
# 
# slopePr_pred<- predict(model=slopePr_glm, object = ras_stack, fun=predict.glm, type = "response")
# plot(slopePr_pred)


#################################333
#GAM
# #The response curve for gam
# gam_slopePr <- gam(Slopeproc ~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "poisson")
# plot(gam_slopePr, pages=1)
# summary(gam_slopePr)
# anova(gam_slopePr, test = "Chisq")
# #Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
# slopePr_pred_gam<- predict(model=gam_slopePr, object = ras_stack, fun=predict.gam, type = "response")
# plot(slopePr_pred_gam)
# 


###################################################################
#GBM    
#using the normal gbm package.
# names(ras_stack)
# #GBM
# slopePr_gbm<-gbm(formula = Slopeproc ~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
#               distribution = "poisson",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)
# 
# best.iter<-gbm.perf(slopePr_gbm, plot.it = F, method = "OOB")
# pred_slopePr_gbm <- predict(object=ras_stack,
#                          model=slopePr_gbm, fun=predict,
#                          n.trees=slopePr_gbm$n.trees, type="response")
# plot(pred_slopePr_gbm)



#dismo package

# gbm_fit_slopePr <- gbm.step(data=data, gbm.x =
#                            c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Slopeproc ",
#                          bag.fraction=0.75, learning.rate = 0.001,
#                          family="poisson",n.trees=50, n.folds=10,
#                          max.trees = 10000, tree.complexity = 6)
# 
# pred_gbm_slopePr <- predict(object=ras_stack,
#                          model=gbm_fit_slopePr, fun=predict,
#                          n.trees=gbm_fit_slopePr$n.trees, type="response")
# 
# plot(pred_gbm_slopePr)






################################################################
# #LOOCV
# 
# #set.seed(0)
# 
# 
# {pred_slopePr_test <- c()
# for (i in 1:nrow(data)){
#   print(i)
#   cal <- data[-i,]
#   eva <-data[i,]
#   slopePr_glm <- glm(Slopeproc ~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "poisson") 
#   
#   
#   slopePr_glm_pred_test<- predict.glm(object = slopePr_glm, newdata = eva, type="response")
#   pred_slopePr_test[i]<- slopePr_glm_pred_test
#   
#   #slopePr_pred<- predict(object=ras_stack, model=slopePr_glm, fun=predict, type = "response")
# } 
#  pred_obs <- cbind.data.frame(pred_slopePr_test, data$Slopeproc )
#  colnames(pred_obs)<-c("pred_slopePr", "obs_slopePr")
#  #cor(pred_slopePr_test, data$Slopeproc ,method = "spearman")
# }
# 
# cor(pred_obs$pred_slopePr, pred_obs$obs_slopePr, method = "spearman")
# plot(pred_obs$pred_slopePr, pred_obs$obs_slopePr)



######################################################################
#####################################################################
#function to calculate mean error
mean_error<- function(obs, pred){
  me<-mean(abs(obs-pred))
  return(me)
}

# Function that returns Root Mean Squared Error
rmse <- function(obs, pred){
  rmse<-sqrt(mean((obs-pred)^2))
  return(rmse)
}
################################################################
####################################################################



#####################################

#dividing into 70:30
{rep<-100
for (i in 1:rep){
  print(i)
  
  #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
  rand<- sample(1:nrow(data), size = 0.7*nrow(data))
  cal<- data[rand,]
  eva<-data[-rand,]
  slopePr_glm <- glm(Slopeproc ~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "poisson") 
  
  slopePr_glm_pred<- predict.glm(object = slopePr_glm, newdata = eva, type="response")
  
  cor_glm_slopePr<-cor(slopePr_glm_pred, eva$Slopeproc , method = "spearman")
  #slopePr_pred_glm_ras<- predict(model=slopePr_glm, object = ras_stack, fun=predict.glm, type = "response")
  #plot(slopePr_pred_glm_ras)
  
  #########
  #mean error and root mean square error
  error_slopePr_glm<- cbind.data.frame(slopePr_glm_pred, eva$Slopeproc )
  colnames(error_slopePr_glm) <- c("pred_glm_slopePr", "obs_slopePr")
  
  slopePr_glm_me <- mean_error(error_slopePr_glm$obs_slopePr, error_slopePr_glm$pred_glm_slopePr)
  slopePr_glm_rmse <- rmse(error_slopePr_glm$obs_slopePr, error_slopePr_glm$pred_glm_slopePr)
  
  me_rmse_slopePr_glm <- rbind.data.frame(slopePr_glm_me, slopePr_glm_rmse)
  colnames(me_rmse_slopePr_glm)<- c("Cryo_glm")
  
  
  
  
  #GAM
  slopePr_gam <- gam(Slopeproc ~ s(RR_annual, k=3)+ s(Slope, k=3), data = cal, family = "poisson")
  slopePr_gam_pred <- predict.gam(slopePr_gam, newdata = eva, type = "response")
  
  obs_pred_slopePr_gam<- cbind.data.frame(slopePr_gam_pred, eva$Slopeproc )
  colnames(obs_pred_slopePr_gam) <- c("pred_gam_slopePr", "obs_gam_slopePr")
  #you can just calclate the correlation straight away
  cor_gam_slopePr <- cor(slopePr_gam_pred, eva$Slopeproc , method = "spearman")
  
  #use the calibration data to predict into the raster stack
  #slopePr_pred_gam_ras<- predict(object=ras_stack, model=slopePr_gam, fun=predict.gam,type="response")
  #plot(slopePr_pred_gam_ras)
  
  #########
  #mean error and root mean square error
  error_slopePr_gam<- cbind.data.frame(slopePr_gam_pred, eva$Slopeproc )
  colnames(error_slopePr_gam) <- c("pred_gam_slopePr", "obs_slopePr")
  
  slopePr_gam_me <- mean_error(error_slopePr_gam$obs_slopePr, error_slopePr_gam$pred_gam_slopePr)
  slopePr_gam_rmse <- rmse(error_slopePr_gam$obs_slopePr, error_slopePr_gam$pred_gam_slopePr)
  
  me_rmse_slopePr_gam <- rbind.data.frame(slopePr_gam_me, slopePr_gam_rmse)
  colnames(me_rmse_slopePr_gam)<- c("Cryo_gam")
  
  
  
  
  ###################################################################3
  #using the normal gbm, package.
  #GBM
  slopePr_gbm1<-gbm(formula = Slopeproc ~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=cal,
                 distribution = "poisson",n.trees = 300, shrinkage = 0.01, interaction.depth = 6,
                 bag.fraction = 0.75)
  
  best.iter<-gbm.perf(slopePr_gbm1, plot.it = T, method = "OOB")
  slopePr_gbm1_pred<- predict.gbm(object = slopePr_gbm1, newdata = eva, best.iter,
                               type="response")
  cor_gbm1_slopePr <- cor(slopePr_gbm1_pred, eva$Slopeproc , method = "spearman")
  #slopePr_pred_gbm1_ras<- predict(object=ras_stack,model=slopePr_gbm1, fun=predict,
  #                          n.trees=slopePr_gbm1$n.trees, type="response")
  # plot(slopePr_pred_gbm1_ras)
  
  #########
  #mean error and root mean square error
  error_slopePr_gbm1<- cbind.data.frame(slopePr_gbm1_pred, eva$Slopeproc )
  colnames(error_slopePr_gbm1) <- c("pred_gbm1_slopePr", "obs_slopePr")
  
  slopePr_gbm1_me <- mean_error(error_slopePr_gbm1$obs_slopePr, error_slopePr_gbm1$pred_gbm1_slopePr)
  slopePr_gbm1_rmse <- rmse(error_slopePr_gbm1$obs_slopePr, error_slopePr_gbm1$pred_gbm1_slopePr)
  
  me_rmse_slopePr_gbm1 <- rbind.data.frame(slopePr_gbm1_me, slopePr_gbm1_rmse)
  colnames(me_rmse_slopePr_gbm1)<- c("Cryo_gbm1")
  
  par(mfrow=c(2,3))
  plot.gbm(slopePr_gbm1, 1, best.iter)
  plot.gbm(slopePr_gbm1, 2, best.iter)
  plot.gbm(slopePr_gbm1, 3, best.iter)
  plot.gbm(slopePr_gbm1, 4, best.iter)
  plot.gbm(slopePr_gbm1, 5, best.iter)
  plot.gbm(slopePr_gbm1, 6, best.iter)
  par(mfrow=c(1,1))
  
  
  
  ###################################################
  #dismo package
  
  slopePr_gbm2 <-gbm.step(data=cal, gbm.x =c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Cryoturb",
  bag.fraction=0.75, learning.rate = 0.001,family="poisson",n.trees=50, n.folds=10,
  max.trees = 10000, tree.complexity = 6)
  #best.iter<-gbm.perf(slopePr_gbm1, plot.it = T, method = "OOB")
  # slopePr_gbm2_pred<- predict.gbm(object = slopePr_gbm2, newdata = eva, best.iter,
  #                              type="response")
  
  #this immediately does not work as expected, so, i'm using the next
  #slopePr_gbm2_pred<- predict(object=,model=slopePr_gbm2, fun=predict,n.trees=slopePr_gbm2$n.trees, type="response")
  
  slopePr_gbm2_pred <- predict.gbm(slopePr_gbm2, newdata = eva, n.trees=slopePr_gbm2$n.trees, type = "response")
  #slopePr_pred_gbm2_ras <- predict(object=ras_stack,model=slopePr_gbm2, fun=predict,
  #                         n.trees=slopePr_gbm2$n.trees, type="response")
  
  #plot(slopePr_pred_gbm2_ras)
  cor_gbm2_slopePr <- cor(slopePr_gbm2_pred, eva$Slopeproc , method = "spearman")
  
  #########
  #mean error and root mean square error
  error_slopePr_gbm2<- cbind.data.frame(slopePr_gbm2_pred, eva$Slopeproc )
  colnames(error_slopePr_gbm2) <- c("pred_gbm2_slopePr", "obs_slopePr")
  
  slopePr_gbm2_me <- mean_error(error_slopePr_gbm2$obs_slopePr, error_slopePr_gbm2$pred_gbm2_slopePr)
  slopePr_gbm2_rmse <- rmse(error_slopePr_gbm2$obs_slopePr, error_slopePr_gbm2$pred_gbm2_slopePr)
  
  me_rmse_slopePr_gbm2 <- rbind.data.frame(slopePr_gbm2_me, slopePr_gbm2_rmse)
  colnames(me_rmse_slopePr_gbm2)<- c("Cryo_gbm2")
  
  
  
  
} 
#####All correlation
all_cor_slopePr <- cbind.data.frame(cor_glm_slopePr,cor_gam_slopePr,
                                 cor_gbm1_slopePr, cor_gbm2_slopePr)
colnames(all_cor_slopePr)<- c("slopePr_glm", "slopePr_gam", "slopePr_gbm1", "slopePr_gbm2")

#####all error
all_error_slopePr <- cbind.data.frame(me_rmse_slopePr_glm, me_rmse_slopePr_gam,
                                   me_rmse_slopePr_gbm1, me_rmse_slopePr_gbm2)
rownames(all_error_slopePr)<- c("mean error", "RMSE")

}
# plot predicted values
plot(slopePr_gbm1_pred, eva$Slopeproc )
lines(lowess(slopePr_gbm1_pred, eva$Slopeproc ), col= "red")
r2data <- cor.test(predict.gbm(slopePr_gbm1_pred, eva$Slopeproc ), Slopeproc )
slopePrR2 <- (r2data$estimate)^2
legend("topleft",paste
       ("r^2=",round(slopePrR2, 3)), bty="n") # r^2



# plot(slopePr_pred_glm_ras)
# plot(slopePr_pred_gam_ras)
# plot(slopePr_pred_gbm1_ras)
# plot(slopePr_pred_gbm2_ras)

par(mfrow=c(2,3))
plot.gbm(slopePr_gbm1, 1, best.iter)
plot.gbm(slopePr_gbm1, 2, best.iter)
plot.gbm(slopePr_gbm1, 3, best.iter)
plot.gbm(slopePr_gbm1, 4, best.iter)
plot.gbm(slopePr_gbm1, 5, best.iter)
plot.gbm(slopePr_gbm1, 6, best.iter)

par(mfrow=c(1,1))  #reset plot
summary(slopePr_gbm1,n.trees=best.iter)

# # plot predicted values
# plot(predict.gbm(slopePr_gbm1, data,
#                    best.iter, type="response"), Slopeproc )
# lines(lowess(predict.gbm(slopePr_gbm1, data,
#              best.iter, type="response"), Slopeproc ), col= "red")
# r2data <- cor.test(predict.gbm(slopePr_gbm1, data,
#                                 best.iter, type="response"), Slopeproc )
# SPRr2 <- (r2data$estimate)^2
# legend("topleft",paste
#          ("r^2=",round(SPRr2, 3)), bty="n") # r^2




summary(slopePr_gbm1)
summary(slopePr_gbm2_pred)
# summary(slopePr_pred_gbm1_ras)
# summary(slopePr_pred_gbm2_ras)

# slopePr_pred_gbm1_ras<-predict(object=ras_stack,model=slopePr_gbm1, fun=predict,
#                    n.trees=slopePr_gbm1$n.trees, type="response")
# plot(slopePr_pred_gbm1_ras)
# 
# slopePr_pred_gbm2_ras<-predict(object=ras_stack,model=slopePr_gbm2, fun=predict,
#                    n.trees=slopePr_gbm2$n.trees, type="response")
# 
# plot(slopePr_pred_gbm2_ras)
#plot.gbm(slopePr_pred_gbm2_ras)

# ?gbm.step

#