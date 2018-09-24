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

colnames(data)
ras_stack<- stack(raster("Elevation.tif"),raster("NDVI.tif"), raster("Prec_Ann.tif"),
               raster("Radiation_7.tif"), raster("Slope.tif"),raster("Tavg_7.tif"))

# ras_stack<-stack(raster("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124/Prec_Ann.tif"),
# raster("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124/Radiation_7.tif"),
# raster("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124/Slope.tif"),
# raster("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124/Tavg_7.tif"),
# raster("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124/Elevation.tif"),
# raster("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/GIS_data-20171124/NDVI.tif"))

#change the names to make the calibration data match with the prediction raster data
names(ras_stack)<-c("Altitude", "NDVI", "RR_annual","Radiation", "Slope","Tavg_7")


#' NOTE: the radiation variable has 
#' different units in calibration (MJ/cm^2) and raster data (KJ/M^2). 
#' Therefore you need to multiply the radiation values by 1 000 000 in the 
#' calibration data or divide the Radiation raster by 1 000 000. It is no wonder,
#' that the predictions did not make any sense!

#rescale the radiation raster to make it match with that of the dataframe
ras_stack$Radiation<- (ras_stack$Radiation)/1000000



plot(ras_stack)

attach(data)

dim(data)

str(data)

summary(data)

glimpse(data)
plot(data[1:3])

#remember to use ggplot to explore the distribution of the predictors

cor_matrix<- cor(data[1:12])
corrplot(cor_matrix, method = "pie", type="upper" )
corrplot(cor_matrix, method = "number", type="upper" )



#####################################################################################3
#CRYOTURBATION
#I decided to use the below because they have correlations below 0.7
#Next, i check if they are curvilinear and also check if they are interrelated

##############CHECKING INTERACTIONS BETWEEN PREDICTORS################
require(effects); require(viridis)
m <-lm(Cryoturb~Radiation*RR_annual, data=data)

plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
#radiation has a high effect on cryoturbation at high annual rainfall

m <-lm(Cryoturb~Radiation*Tavg_7, data=data)

plot(effect(term="Radiation:Tavg_7", mod=m,xlevels=10, multiline=T, colors = 2, lwd=2))


#After testing the model, using anova and the summary of the model(which uses t-test), we found out that
# that the interactions and other second orders are insignificant.
#however, because of the discrepancy between the significance test of the anova and the summary of the
#mode, I will be building two models with and without the T_Avg and use wilcox test to see
#if it improves the model significantly.

#note: dont use auc for poisson and gaussiam because it is meant for binary variables.

#Slope is curvillinear(i.e) the second order is significant. However, the third order is not signnificant 
#in predicting cryoturbation.

#After removing the insignificant predictors manually step-by-step, I tried the stepwise
#regression
library(MASS)
step_cryo_glm<-stepAIC(cryo_glm, direction = "both")
step_cryo_glm$anova
cryo_pred<- predict(object=ras_stack, model=cryo_glm, fun=predict, type = "response")
plot(cryo_pred)

#final model for cryoturbation
cryo_glm <- glm(Cryoturb~ RR_annual+ Slope +  I(Slope^2), data=data, family = "poisson") 
#test for significance of the predictors.
anova (cryo_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for poisson and binomial
#summary of the model, which uses t-test for it's significance.'
summary(cryo_glm)
plot(cryo_glm, pages=1)

#based on the test from the summary and checking the correclation of the predicted with observed,
#Tavg_7 did not improve the prediction and hence, not necessary.

#########
#now, predict into the raster
#to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
#Will also try the LOOCV(i.e leave one out cross validation) method

cryo_pred<- predict(model=cryo_glm, object = ras_stack, fun=predict.glm, type = "response")
plot(cryo_pred)


#################################333
#GAM
#The response curve for gam
gam_cryo <- gam(Cryoturb~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "poisson")
plot(gam_cryo, pages=1)
summary(gam_cryo)
anova(gam_cryo, test = "Chisq")
#Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
cryo_pred_gam<- predict(model=gam_cryo, object = ras_stack, fun=predict.gam, type = "response")
plot(cryo_pred_gam)



###################################################################
#GBM    
#using the normal gbm package.
names(ras_stack)
#GBM
cryo_gbm<-gbm(formula = Cryoturb~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
              distribution = "poisson",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)

best.iter<-gbm.perf(cryo_gbm, plot.it = F, method = "OOB")
pred_cryo_gbm <- predict(object=ras_stack,
                         model=cryo_gbm, fun=predict,
                         n.trees=cryo_gbm$n.trees, type="response")
plot(pred_cryo_gbm)



#dismo package

gbm_fit_cryo <- gbm.step(data=data, gbm.x =
                           c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Cryoturb",
                         bag.fraction=0.75, learning.rate = 0.001,
                         family="poisson",n.trees=50, n.folds=10,
                         max.trees = 10000, tree.complexity = 6)

pred_gbm_cryo <- predict(object=ras_stack,
                         model=gbm_fit_cryo, fun=predict,
                         n.trees=gbm_fit_cryo$n.trees, type="response")

plot(pred_gbm_cryo)






################################################################
#LOOCV

#set.seed(0)


{pred_cryo_test <- c()
for (i in 1:nrow(data)){
  print(i)
  cal <- data[-i,]
  eva <-data[i,]
  cryo_glm <- glm(Cryoturb~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "poisson") 
  
  
  cryo_glm_pred_test<- predict.glm(object = cryo_glm, newdata = eva, type="response")
  pred_cryo_test[i]<- cryo_glm_pred_test
  
  #cryo_pred<- predict(object=ras_stack, model=cryo_glm, fun=predict, type = "response")
} 
 pred_obs <- cbind.data.frame(pred_cryo_test, data$Cryoturb)
 colnames(pred_obs)<-c("pred_cryo", "obs_cryo")
 #cor(pred_cryo_test, data$Cryoturb,method = "spearman")
}

cor(pred_obs$pred_cryo, pred_obs$obs_cryo, method = "spearman")
plot(pred_obs$pred_cryo, pred_obs$obs_cryo)



#####################################3
#dividing into 70:30
{rep<-100
for (i in 1:rep){
  print(i)
  
  #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
  rand<- sample(1:nrow(data), size = 0.7*nrow(data))
  cal<- data[rand,]
  eva<-data[-rand,]
  cryo_glm <- glm(Cryoturb~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "poisson") 
  
  cryo_glm_pred<- predict.glm(object = cryo_glm, newdata = eva, type="response")
  obs_pred_cryo<- cbind.data.frame(cryo_glm_pred, eva$Cryoturb)
  colnames(obs_pred_cryo) <- c("pred_Cryoturb", "obs_Cryoturb")
  
  cryo_cor_glm<-cor(cryo_glm_pred, eva$Cryoturb, method = "spearman")
  #cryo_pred_glm_ras<- predict(model=cryo_glm, object = ras_stack, fun=predict.glm, type = "response")
  #plot(cryo_pred_glm_ras)
  
  #GAM
  cryo_gam <- gam(Cryoturb~ s(RR_annual, k=3)+ s(Slope, k=3), data = cal, family = "poisson")
  cryo_gam_pred <- predict.gam(cryo_gam, newdata = eva, type = "response")
  
  obs_pred_cryo_gam<- cbind.data.frame(cryo_gam_pred, eva$Cryoturb)
  colnames(obs_pred_cryo_gam) <- c("pred_gam_cryo", "obs_cryo")
  #you can just calclate the correlation straight away
  cor_gam_cryo <- cor(cryo_gam_pred, eva$Cryoturb, method = "spearman")
  
  #use the calibration data to predict into the raster stack
  #cryo_pred_gam_ras< <- predict(object=ras_stack, model=cryo_gam, fun=predict.gam,type="response")
  #plot(cryo_pred_gam_ras)
  
  ###################################################################3
  #using the normal gbm, package.
  #GBM
  cryo_gbm1<-gbm(formula = Cryoturb~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=cal,
                distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 6,
                bag.fraction = 0.75)
  
  best.iter<-gbm.perf(cryo_gbm1, plot.it = F, method = "OOB")
  cryo_gbm_pred1<- predict.gbm(object = cryo_gbm1, newdata = eva, best.iter,
                               n.trees=cryo_gbm2$n.trees,
                               type="response")
  cor_gbm1_cryo <- cor(cryo_gbm_pred1, eva$Cryoturb, method = "spearman")
  # cryo_pred_gbm1_ras< <- predict(object=ras_stack,model=cryo_gbm1, fun=predict,
  #                          n.trees=cryo_gbm1$n.trees, type="response")
  # plot(cryo_pred_gbm1_ras)
  
  ###################################################
  #dismo package
  
  cryo_gbm2 <- gbm.step(data=cal, gbm.x =c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Cryoturb",
                           bag.fraction=0.75, learning.rate = 0.001,
                           family="poisson",n.trees=3000, n.folds=10,
                           max.trees = 10000, tree.complexity = 6)
  
  cryo_gbm_pred2<- predict.gbm(object = cryo_gbm2,n.trees=cryo_gbm2$n.trees,  newdata = eva, type="response")
  cor_gbm2_cryo <- cor(cryo_gbm_pred2, eva$Cryoturb, method = "spearman")
  # cryo_pred_gbm2_ras <- predict(object=ras_stack,model=cryo_gbm2, fun=predict,
  #                          n.trees=cryo_gbm2$n.trees, type="response")
  # 
  # plot(cryo_pred_gbm2_ras)
  
  
  } all_cor_Cryo <- cbind.data.frame(cor_glm_cryo,cor_gam_cryo,
                                         cor_gbm1_cryo, cor_gbm2_cryo)
  colnames(all_cor_cryo)<- c("glm", "gama", "gbm1", "gbm2")
}

summary(cryo_gbm1)
summary(cryo_gbm2)
# summary(cryo_pred_gbm1_ras)
# summary(cryo_pred_gbm2_ras)

# cryo_pred_gbm1_ras<-predict(object=ras_stack,model=cryo_gbm1, fun=predict,
#                    n.trees=cryo_gbm1$n.trees, type="response")
# plot(cryo_pred_gbm1_ras)
# 
# cryo_pred_gbm2_ras<-predict(object=ras_stack,model=cryo_gbm2, fun=predict,
#                    n.trees=cryo_gbm2$n.trees, type="response")
# 
# plot(cryo_pred_gbm2_ras)

# ?gbm.step

###################################################################3
#using the normal gbm, package.
#now, i'll try with the dismo technique
names(ras_stack)
#GBM
cryo_gbm<-gbm(formula = Cryoturb~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
           distribution = "poisson",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)

best.iter<-gbm.perf(cryo_gbm, plot.it = F, method = "OOB")
pred_cryo_gbm <- predict(object=ras_stack,
                    model=cryo_gbm, fun=predict,
                    n.trees=cryo_gbm$n.trees, type="response")
plot(pred_cryo_gbm)



pred_h_gbm<-predict.gbm(h_gbm,newdata = eva, best.iter, type = "response")
h_cor_gbm<-cor(pred_h_gbm, eva$veg_height, method = "spearman")


#dismo package

gbm_fit_cryo <- gbm.step(data=data, gbm.x =
                           c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Cryoturb",
                         bag.fraction=0.75, learning.rate = 0.001,
                         family="poisson",n.trees=50, n.folds=10,
                         max.trees = 10000, tree.complexity = 6)

pred_gbm_cryo <- predict(object=ras_stack,
                         model=gbm_fit_cryo, fun=predict,
                         n.trees=gbm_fit_cryo$n.trees, type="response")

plot(pred_gbm_cryo)















#corelation
cor_obs_pred<-cor(obs_pred_cryo$pred_Cryoturb, obs_pred_cryo$obs_Cryoturb, method = "spearman")


#function to calculate mean error
mean_error<- function(obs, pred){
  me<-mean(abs(obs-pred))
  return(me)
}
me_obs_pred<-mean_error(obs = obs_pred_cryo$obs_Cryoturb, 
                            pred = obs_cryo_pred$pred_Cryoturb)
me_obs_pred

    #####################################################################
#this was done to test the function
# obs_pred_temp$test<-abs((obs_pred_temp$predicted_temp)-(obs_pred_temp$observed_temp))
# mean(obs_pred_temp$test)
#####################################################################


# Function that returns Root Mean Squared Error
rmse <- function(obs, pred){
  sqrt(mean((obs-pred)^2))
}
rmse_obs_pred<-rmse(obs = obs_pred_cryo$obs_Cryoturb, 
                        pred = obs_cryo_pred$pred_Cryoturb)
rmse_obs_pred


cor_obs_pred<-cor(obs_pred_cryo$obs_Cryoturb,obs_pred_cryo$pred_Cryoturb, method = "spearman")

plot(obs_Cryoturb~pred_Cryoturb, data=obs_pred_cryo,
     xlab="predicted Cryoturbation", ylab = "observed Cryoturbation")
lines( obs_pred_cryo$pred_Cryoturb ,obs_pred_cryo$obs_Cryoturbb )

legend("topleft", paste(paste("mean error = ", round((me_obs_pred),2)),";",
                        paste("cor=", round((cor_obs_pred),2))) )








    





















gam_moist <- gam(Cryoturb~s(Altitude, k=3)+ s(Tavg_7, k=3), data = data, family = "gaussian")
plot(gam_moist)
pred.gam_moist <- predict.gam(gam_moist, newdata=ras_stack, type="response")





gbm_fit_cryo <- gbm.step(data=data, gbm.x =
                      c("Altitude","Tavg_7"), gbm.y = "Cryoturb",
                    bag.fraction=0.75, learning.rate = 0.01,
                    family="gaussian",n.trees=50, n.folds=10,
                    max.trees = 10000, tree.complexity = 6)

pred_gbm_cryo <- predict(object=ras_stack,
                    model=gbm_fit, fun=predict,
                    n.trees=gbm_fit$n.trees, type="response")



















{
temp_pred<-c()
for (i in 1:nrow(data)){
  print(i)
  lm_temp2<- lm(temp~elev+I(elev^2), data = data[-i,])
  eval_data<- data[i,]
  p<- predict(lm_temp2, eval_data)
  temp_pred[i]<-(p)
  
}
#combine the predicted temperature and observed into a dataframe
obs_pred_temp<-cbind.data.frame(temp_pred, data$temp)
colnames(obs_pred_temp)<-c("predicted_temp", "observed_temp")
}


gbm_fit <- gbm.step(data=data, gbm.x =
                      c("Altitude","Tavg_7"), gbm.y = "Cryoturb",
                    bag.fraction=0.75, learning.rate = 0.01,
                    family="gaussian",n.trees=50, n.folds=10,
                    max.trees = 10000, tree.complexity = 6)

pred_gbm <- predict(object=ras_stack,
                    model=gbm_fit, fun=predict,
                    n.trees=gbm_fit$n.trees, type="response")

gam_moist <- gam(Cryoturb~s(Altitude, k=3)+ s(Tavg_7, k=3), data = data, family = "gaussian")
plot(gam_moist)
pred.gam_moist <- predict.gam(gam_moist, newdata=ras_stack, type="response")

