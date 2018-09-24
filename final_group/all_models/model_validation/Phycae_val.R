# Cryoturbation, slope processes, NDVI and
# Phycae 
#########################################################################
#Phycae 
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
# m <-lm(Phycae  ~Radiation*RR_annual, data=data)
# 
# plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
# #radiation has a high effect on phycturbation at high annual rainfall 
# #recheck the above statement with that of cryoturbation script
# 
# m <-lm(Phycae  ~Radiation*Tavg_7, data=data)
# 
# plot(effect(term="Radiation:Tavg_7", mod=m,xlevels=10, multiline=T, colors = 2, lwd=2))
# 
# 
# #After testing the model, using anova and the summary of the model(which uses t-test), we found out that
# # that the interactions and other second orders are insignificant.
# #however, because of the discrepancy phycween the significance test of the anova and the summary of the
# #mode, I will be building two models with and without the T_Avg and use wilcox test to see
# #if it improves the model significantly.
# 
# #note:use auc for binomial because it is meant for binary variables.
# 
# phyc_glm <- glm(Phycae  ~Altitude + I(Altitude^2) + NDVI+ I(NDVI^2)+ Radiation+
#                   I(Radiation^2)+Tavg_7+ I(Tavg_7^2) + RR_annual+I(RR_annual^2)+ Slope +  I(Slope^2), data=data, family = "binomial")
# 
# 
# #After removing the insignificant predictors manually step-by-step, I tried the stepwise
# #regression
# library(MASS)
# step_phyc_glm<-stepAIC(phyc_glm, direction = "both")
# step_phyc_glm$anova
# phyc_pred<- predict(object=ras_stack, model=phyc_glm, fun=predict, type = "response")
# plot(phyc_pred)
# 
# # 
# # #final model for phycocesses
# phyc_glm <- glm(Phycae  ~RR_annual + Slope + 
#                   I(Slope^2), data=data, family = "binomial")
# #test for significance of the predictors.
# anova (phyc_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for binomial and binomial
# #summary of the model, which uses t-test for it's significance.'
# summary(phyc_glm)
# plot(phyc_glm, pages=1)

# #based on the test from the summary and checking the correclation of the predicted with observed,
# #Tavg_7 did not improve the prediction and hence, not necessary.
# 
# #########
# #now, predict into the raster
# #to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
# #Will also try the LOOCV(i.e leave one out cross validation) method
# 
# phyc_pred<- predict(model=phyc_glm, object = ras_stack, fun=predict.glm, type = "response")
# plot(phyc_pred)


#################################333
#GAM
# #The response curve for gam
# gam_phyc <- gam(Phycae  ~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "binomial")
# plot(gam_phyc, pages=1)
# summary(gam_phyc)
# anova(gam_phyc, test = "Chisq")
# #Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
# phyc_pred_gam<- predict(model=gam_phyc, object = ras_stack, fun=predict.gam, type = "response")
# plot(phyc_pred_gam)
# 


###################################################################
#GBM    
#using the normal gbm package.
# names(ras_stack)
# #GBM
# phyc_gbm<-gbm(formula = Phycae  ~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
#               distribution = "binomial",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)
# 
# best.iter<-gbm.perf(phyc_gbm, plot.it = F, method = "OOB")
# pred_phyc_gbm <- predict(object=ras_stack,
#                          model=phyc_gbm, fun=predict,
#                          n.trees=phyc_gbm$n.trees, type="response")
# plot(pred_phyc_gbm)



#dismo package

# gbm_fit_phyc <- gbm.step(data=data, gbm.x =
#                            c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Phycae  ",
#                          bag.fraction=0.75, learning.rate = 0.001,
#                          family="binomial",n.trees=50, n.folds=10,
#                          max.trees = 10000, tree.complexity = 6)
# 
# pred_gbm_phyc <- predict(object=ras_stack,
#                          model=gbm_fit_phyc, fun=predict,
#                          n.trees=gbm_fit_phyc$n.trees, type="response")
# 
# plot(pred_gbm_phyc)






################################################################
# #LOOCV
# 
# #set.seed(0)
# 
# 
# {pred_phyc_test <- c()
# for (i in 1:nrow(data)){
#   print(i)
#   cal <- data[-i,]
#   eva <-data[i,]
#   phyc_glm <- glm(Phycae  ~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "binomial") 
#   
#   
#   phyc_glm_pred_test<- predict.glm(object = phyc_glm, newdata = eva, type="response")
#   pred_phyc_test[i]<- phyc_glm_pred_test
#   
#   #phyc_pred<- predict(object=ras_stack, model=phyc_glm, fun=predict, type = "response")
# } 
#  pred_obs <- cbind.data.frame(pred_phyc_test, data$Phycae  )
#  colnames(pred_obs)<-c("pred_phyc", "obs_phyc")
#  #cor(pred_phyc_test, data$Phycae  ,method = "spearman")
# }
# 
# cor(pred_obs$pred_phyc, pred_obs$obs_phyc, method = "spearman")
# plot(pred_obs$pred_phyc, pred_obs$obs_phyc)



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
  
  phyc_auc_glm<-phyc_auc_gam<-phyc_auc_gbm1<-phyc_auc_gbm2<-c()  
  rep<-10
  for (i in 1:rep){
    print(i)
    
    #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
    rand<- sample(1:nrow(data), size = 0.7*nrow(data))
    cal<- data[rand,]
    eva<-data[-rand,]
    phyc_glm <- glm(Phycae~  RR_annual + I(RR_annual^2), data=cal, family = "binomial") 

      
    phyc_glm_pred<- predict.glm(object = phyc_glm, newdata = eva, type="response")
    #check the AUC of the compared prediction and evaluation
    phyc_auc_glm_p<-colAUC(phyc_glm_pred, eva$Phycae , plotROC=T)
    phyc_auc_glm <- c(phyc_auc_glm, phyc_auc_glm_p[[1]])
    
    #GAM
    phyc_gam<-gam(Phycae~ s(RR_annual, k=2), data = cal, family = "binomial")

    pred_phyc_gam<-predict.gam(phyc_gam, newdata = eva, type = "response")
    phyc_auc_gam_p<-colAUC(pred_phyc_gam, eva$Phycae , plotROC=T)
    phyc_auc_gam <- c(phyc_auc_gam, phyc_auc_gam_p[[1]])
    
    #GBM1
    phyc_gbm1<-gbm(formula = Phycae~ RR_annual + NDVI + Tavg_7 + Radiation + Slope, data=cal,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)
    best.iter1<-gbm.perf(phyc_gbm1, plot.it = F, method = "OOB")
    pred_phyc_gbm1<-predict.gbm(phyc_gbm1,newdata = eva, best.iter1, type = "response")
    
    phyc_auc_gbm_p1<-colAUC(pred_phyc_gbm1, eva$Phycae , plotROC = F)
    phyc_auc_gbm1<- c(phyc_auc_gbm1, phyc_auc_gbm_p1[[1]])
    
    #GBM2 dismo
    phyc_gbm2<-gbm.step(data=cal, gbm.x =
                          c("RR_annual","NDVI", "Tavg_7","Radiation","Slope"), gbm.y = "Phycae",
                        bag.fraction=0.75, learning.rate = 0.001,
                        family="bernoulli",n.trees=50, n.folds=10,
                        max.trees = 3000, tree.complexity = 6)
    
    #prediction
    pred_phyc_gbm2 <- predict.gbm(phyc_gbm2, newdata = eva, n.trees=phyc_gbm2$n.trees, type = "response")
    #The above can also be done usin the next two steps below.
    # best.iter2<-gbm.perf(phyc_gbm2, plot.it = F, method = "OOB")
    # pred_phyc_gbm2<-predict.gbm(phyc_gbm2,newdata = eva, best.iter2, type = "response")
    # 
    
    #plotting ROC curve and getting the value
    phyc_auc_gbm_p2<-colAUC(pred_phyc_gbm2, eva$Phycae , plotROC = T)
    #coombining t he value into a list
    phyc_auc_gbm2<- c(phyc_auc_gbm2, phyc_auc_gbm_p2[[1]])
    
    
  } 
  compared_model_phyc=cbind.data.frame(phyc_auc_glm, phyc_auc_gam, 
                                       phyc_auc_gbm1,phyc_auc_gbm2)
}
compared_model_phyc
mean_auc_phyc<-colMeans(compared_model_phyc)

fpa <- "C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/validation_tables/Species"
write.csv(compared_model_phyc, file=paste0(fpa,"/AUC_phyc_1.csv"))

write.csv(mean_auc_phyc, file=paste0(fpa,"/MEAN_aUC_phyc.csv"))
