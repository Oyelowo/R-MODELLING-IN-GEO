# Cryoturbation, slope processes, NDVI and
# Gersyl 
#########################################################################
#Gersyl 
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



# #####################################################################################3
# #CRYOTURBATION
# #I decided to use the below because they have correlations below 0.7
# #Next, i check if they are curvilinear and also check if they are interrelated
# 
# ##############CHECKING INTERACTIONS BETWEEN PREDICTORS################
# require(effects); require(viridis)
# m <-lm(Gersyl  ~Radiation*RR_annual, data=data)
# 
# plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
# #radiation has a high effect on gersturbation at high annual rainfall 
# #recheck the above statement with that of cryoturbation script
# 
# m <-lm(Gersyl  ~Radiation*Tavg_7, data=data)
# 
# plot(effect(term="Radiation:Tavg_7", mod=m,xlevels=10, multiline=T, colors = 2, lwd=2))
# 
# 
# #After testing the model, using anova and the summary of the model(which uses t-test), we found out that
# # that the interactions and other second orders are insignificant.
# #however, because of the discrepancy gersween the significance test of the anova and the summary of the
# #mode, I will be building two models with and without the T_Avg and use wilcox test to see
# #if it improves the model significantly.
# 
# #note:use auc for binomial because it is meant for binary variables.
# 
# gers_glm <- glm(Gersyl  ~Altitude + I(Altitude^2) + NDVI+ I(NDVI^2)+ Radiation+
#                   I(Radiation^2)+Tavg_7+ I(Tavg_7^2) + RR_annual+I(RR_annual^2)+ Slope +  I(Slope^2), data=data, family = "binomial")
# 
# 
# #After removing the insignificant predictors manually step-by-step, I tried the stepwise
# #regression
# library(MASS)
# step_gers_glm<-stepAIC(gers_glm, direction = "both")
# step_gers_glm$anova
# gers_pred<- predict(object=ras_stack, model=gers_glm, fun=predict, type = "response")
# plot(gers_pred)
# 
# # 
# # #final model for gersocesses
# gers_glm <- glm(Gersyl  ~RR_annual + Slope + 
#                   I(Slope^2), data=data, family = "binomial")
# #test for significance of the predictors.
# anova (gers_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for binomial and binomial
# #summary of the model, which uses t-test for it's significance.'
# summary(gers_glm)
# plot(gers_glm, pages=1)

# #based on the test from the summary and checking the correclation of the predicted with observed,
# #Tavg_7 did not improve the prediction and hence, not necessary.
# 
# #########
# #now, predict into the raster
# #to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
# #Will also try the LOOCV(i.e leave one out cross validation) method
# 
# gers_pred<- predict(model=gers_glm, object = ras_stack, fun=predict.glm, type = "response")
# plot(gers_pred)


#################################333
#GAM
# #The response curve for gam
# gam_gers <- gam(Gersyl  ~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "binomial")
# plot(gam_gers, pages=1)
# summary(gam_gers)
# anova(gam_gers, test = "Chisq")
# #Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
# gers_pred_gam<- predict(model=gam_gers, object = ras_stack, fun=predict.gam, type = "response")
# plot(gers_pred_gam)
# 


###################################################################
#GBM    
#using the normal gbm package.
# names(ras_stack)
# #GBM
# gers_gbm<-gbm(formula = Gersyl  ~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
#               distribution = "binomial",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)
# 
# best.iter<-gbm.perf(gers_gbm, plot.it = F, method = "OOB")
# pred_gers_gbm <- predict(object=ras_stack,
#                          model=gers_gbm, fun=predict,
#                          n.trees=gers_gbm$n.trees, type="response")
# plot(pred_gers_gbm)



#dismo package

# gbm_fit_gers <- gbm.step(data=data, gbm.x =
#                            c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Gersyl  ",
#                          bag.fraction=0.75, learning.rate = 0.001,
#                          family="binomial",n.trees=50, n.folds=10,
#                          max.trees = 10000, tree.complexity = 6)
# 
# pred_gbm_gers <- predict(object=ras_stack,
#                          model=gbm_fit_gers, fun=predict,
#                          n.trees=gbm_fit_gers$n.trees, type="response")
# 
# plot(pred_gbm_gers)






################################################################
# #LOOCV
# 
# #set.seed(0)
# 
# 
# {pred_gers_test <- c()
# for (i in 1:nrow(data)){
#   print(i)
#   cal <- data[-i,]
#   eva <-data[i,]
#   gers_glm <- glm(Gersyl  ~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "binomial") 
#   
#   
#   gers_glm_pred_test<- predict.glm(object = gers_glm, newdata = eva, type="response")
#   pred_gers_test[i]<- gers_glm_pred_test
#   
#   #gers_pred<- predict(object=ras_stack, model=gers_glm, fun=predict, type = "response")
# } 
#  pred_obs <- cbind.data.frame(pred_gers_test, data$Gersyl  )
#  colnames(pred_obs)<-c("pred_gers", "obs_gers")
#  #cor(pred_gers_test, data$Gersyl  ,method = "spearman")
# }
# 
# cor(pred_obs$pred_gers, pred_obs$obs_gers, method = "spearman")
# plot(pred_obs$pred_gers, pred_obs$obs_gers)



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
{
  
  gers_auc_glm<-gers_auc_gam<-gers_auc_gbm1<-gers_auc_gbm2<-c()  
  rep<-10
  for (i in 1:rep){
    print(i)
    
    #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
    rand<- sample(1:nrow(data), size = 0.7*nrow(data))
    cal<- data[rand,]
    eva<-data[-rand,]
    gers_glm <- glm(Gersyl~  NDVI, data=cal, family = "binomial") 
    
    
    gers_glm_pred<- predict.glm(object = gers_glm, newdata = eva, type="response")
    #check the AUC of the compared prediction and evaluation
    gers_auc_glm_p<-colAUC(gers_glm_pred, eva$Gersyl , plotROC=T)
    gers_auc_glm <- c(gers_auc_glm, gers_auc_glm_p[[1]])
    
    #GAM
    gers_gam<-gam(Gersyl~  s(NDVI, k=2), data = cal, family = "binomial")
    
    pred_gers_gam<-predict.gam(gers_gam, newdata = eva, type = "response")
    gers_auc_gam_p<-colAUC(pred_gers_gam, eva$Gersyl , plotROC=T)
    gers_auc_gam <- c(gers_auc_gam, gers_auc_gam_p[[1]])
    
    #GBM1
    gers_gbm1<-gbm(formula = Gersyl~ NDVI + Tavg_7 + RR_annual, data=cal,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)
    best.iter1<-gbm.perf(gers_gbm1, plot.it = F, method = "OOB")
    pred_gers_gbm1<-predict.gbm(gers_gbm1,newdata = eva, best.iter1, type = "response")
    
    gers_auc_gbm_p1<-colAUC(pred_gers_gbm1, eva$Gersyl , plotROC = F)
    gers_auc_gbm1<- c(gers_auc_gbm1, gers_auc_gbm_p1[[1]])
    
    #GBM2 dismo
    gers_gbm2<-gbm.step(data=cal, gbm.x =
                          c("NDVI",  "Tavg_7", "RR_annual"), gbm.y = "Gersyl",
                        bag.fraction=0.75, learning.rate = 0.001,
                        family="bernoulli",n.trees=50, n.folds=10,
                        max.trees = 3000, tree.complexity = 6)
    
    #prediction
    pred_gers_gbm2 <- predict.gbm(gers_gbm2, newdata = eva, n.trees=gers_gbm2$n.trees, type = "response")
    #The above can also be done usin the next two steps below.
    # best.iter2<-gbm.perf(gers_gbm2, plot.it = F, method = "OOB")
    # pred_gers_gbm2<-predict.gbm(gers_gbm2,newdata = eva, best.iter2, type = "response")
    # 
    
    #plotting ROC curve and getting the value
    gers_auc_gbm_p2<-colAUC(pred_gers_gbm2, eva$Gersyl , plotROC = T)
    #coombining t he value into a list
    gers_auc_gbm2<- c(gers_auc_gbm2, gers_auc_gbm_p2[[1]])
    
    
  } 
  compared_model_gers=cbind.data.frame(gers_auc_glm, gers_auc_gam, 
                                       gers_auc_gbm1,gers_auc_gbm2)
}
compared_model_gers
mean_auc_gers<-colMeans(compared_model_gers)

fpa <- "C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/validation_tables/Species"
write.csv(compared_model_gers, file=paste0(fpa,"/AUC_gers_1.csv"))

write.csv(mean_auc_gers, file=paste0(fpa,"/MEAN_aUC_gers.csv"))
