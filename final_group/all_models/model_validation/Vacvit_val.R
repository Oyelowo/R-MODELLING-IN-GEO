# Cryoturbation, slope processes, NDVI and
# Vacvit 
#########################################################################
#Vacvit 
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
# m <-lm(Vacvit  ~Radiation*RR_annual, data=data)
# 
# plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
# #radiation has a high effect on vacvturbation at high annual rainfall 
# #recheck the above statement with that of cryoturbation script
# 
# m <-lm(Vacvit  ~Radiation*Tavg_7, data=data)
# 
# plot(effect(term="Radiation:Tavg_7", mod=m,xlevels=10, multiline=T, colors = 2, lwd=2))
# 
# 
# #After testing the model, using anova and the summary of the model(which uses t-test), we found out that
# # that the interactions and other second orders are insignificant.
# #however, because of the discrepancy vacvween the significance test of the anova and the summary of the
# #mode, I will be building two models with and without the T_Avg and use wilcox test to see
# #if it improves the model significantly.
# 
# #note:use auc for binomial because it is meant for binary variables.
# 
# vacv_glm <- glm(Vacvit  ~Altitude + I(Altitude^2) + NDVI+ I(NDVI^2)+ Radiation+
#                   I(Radiation^2)+Tavg_7+ I(Tavg_7^2) + RR_annual+I(RR_annual^2)+ Slope +  I(Slope^2), data=data, family = "binomial")
# 
# 
# #After removing the insignificant predictors manually step-by-step, I tried the stepwise
# #regression
# library(MASS)
# step_vacv_glm<-stepAIC(vacv_glm, direction = "both")
# step_vacv_glm$anova
# vacv_pred<- predict(object=ras_stack, model=vacv_glm, fun=predict, type = "response")
# plot(vacv_pred)
# 
# # 
# # #final model for vacvocesses
# vacv_glm <- glm(Vacvit  ~RR_annual + Slope + 
#                   I(Slope^2), data=data, family = "binomial")
# #test for significance of the predictors.
# anova (vacv_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for binomial and binomial
# #summary of the model, which uses t-test for it's significance.'
# summary(vacv_glm)
# plot(vacv_glm, pages=1)

# #based on the test from the summary and checking the correclation of the predicted with observed,
# #Tavg_7 did not improve the prediction and hence, not necessary.
# 
# #########
# #now, predict into the raster
# #to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
# #Will also try the LOOCV(i.e leave one out cross validation) method
# 
# vacv_pred<- predict(model=vacv_glm, object = ras_stack, fun=predict.glm, type = "response")
# plot(vacv_pred)


#################################333
#GAM
# #The response curve for gam
# gam_vacv <- gam(Vacvit  ~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "binomial")
# plot(gam_vacv, pages=1)
# summary(gam_vacv)
# anova(gam_vacv, test = "Chisq")
# #Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
# vacv_pred_gam<- predict(model=gam_vacv, object = ras_stack, fun=predict.gam, type = "response")
# plot(vacv_pred_gam)
# 


###################################################################
#GBM    
#using the normal gbm package.
# names(ras_stack)
# #GBM
# vacv_gbm<-gbm(formula = Vacvit  ~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
#               distribution = "binomial",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)
# 
# best.iter<-gbm.perf(vacv_gbm, plot.it = F, method = "OOB")
# pred_vacv_gbm <- predict(object=ras_stack,
#                          model=vacv_gbm, fun=predict,
#                          n.trees=vacv_gbm$n.trees, type="response")
# plot(pred_vacv_gbm)



#dismo package

# gbm_fit_vacv <- gbm.step(data=data, gbm.x =
#                            c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Vacvit  ",
#                          bag.fraction=0.75, learning.rate = 0.001,
#                          family="binomial",n.trees=50, n.folds=10,
#                          max.trees = 10000, tree.complexity = 6)
# 
# pred_gbm_vacv <- predict(object=ras_stack,
#                          model=gbm_fit_vacv, fun=predict,
#                          n.trees=gbm_fit_vacv$n.trees, type="response")
# 
# plot(pred_gbm_vacv)






################################################################
# #LOOCV
# 
# #set.seed(0)
# 
# 
# {pred_vacv_test <- c()
# for (i in 1:nrow(data)){
#   print(i)
#   cal <- data[-i,]
#   eva <-data[i,]
#   vacv_glm <- glm(Vacvit  ~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "binomial") 
#   
#   
#   vacv_glm_pred_test<- predict.glm(object = vacv_glm, newdata = eva, type="response")
#   pred_vacv_test[i]<- vacv_glm_pred_test
#   
#   #vacv_pred<- predict(object=ras_stack, model=vacv_glm, fun=predict, type = "response")
# } 
#  pred_obs <- cbind.data.frame(pred_vacv_test, data$Vacvit  )
#  colnames(pred_obs)<-c("pred_vacv", "obs_vacv")
#  #cor(pred_vacv_test, data$Vacvit  ,method = "spearman")
# }
# 
# cor(pred_obs$pred_vacv, pred_obs$obs_vacv, method = "spearman")
# plot(pred_obs$pred_vacv, pred_obs$obs_vacv)



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
  
  vacv_auc_glm<-vacv_auc_gam<-vacv_auc_gbm1<-vacv_auc_gbm2<-c()  
  rep<-10
  for (i in 1:rep){
    print(i)
    
    #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
    rand<- sample(1:nrow(data), size = 0.7*nrow(data))
    cal<- data[rand,]
    eva<-data[-rand,]
    vacv_glm <- glm(Vacvit~  NDVI + I(NDVI^2) , data=cal, family = "binomial")
    
    vacv_glm_pred<- predict.glm(object = vacv_glm, newdata = eva, type="response")
    #check the AUC of the compared prediction and evaluation
    vacv_auc_glm_p<-colAUC(vacv_glm_pred, eva$Vacvit , plotROC=F)
    vacv_auc_glm <- c(vacv_auc_glm, vacv_auc_glm_p[[1]])
    
    #GAM
    vacv_gam<-gam(Vacvit~  s(NDVI, k=2) , data = cal, family = "binomial")
    
    pred_vacv_gam<-predict.gam(vacv_gam, newdata = eva, type = "response")
    vacv_auc_gam_p<-colAUC(pred_vacv_gam, eva$Vacvit , plotROC=F)
    vacv_auc_gam <- c(vacv_auc_gam, vacv_auc_gam_p[[1]])
    
    #GBM1
    vacv_gbm1<-gbm(formula = Vacvit~ NDVI + Tavg_7 + RR_annual, data=cal,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)
    
    best.iter1<-gbm.perf(vacv_gbm1, plot.it = F, method = "OOB")
    pred_vacv_gbm1<-predict.gbm(vacv_gbm1,newdata = eva, best.iter1, type = "response")
    
    vacv_auc_gbm_p1<-colAUC(pred_vacv_gbm1, eva$Vacvit , plotROC = F)
    vacv_auc_gbm1<- c(vacv_auc_gbm1, vacv_auc_gbm_p1[[1]])
    
    #GBM2 dismo
    vacv_gbm2<-gbm.step(data=cal, gbm.x =
                          c("NDVI", "Tavg_7", "RR_annual"), gbm.y = "Vacvit",
                        bag.fraction=0.75, learning.rate = 0.001,
                        family="bernoulli",n.trees=50, n.folds=10,
                        max.trees = 3000, tree.complexity = 6)
    
    #prediction
    pred_vacv_gbm2 <- predict.gbm(vacv_gbm2, newdata = eva, n.trees=vacv_gbm2$n.trees, type = "response")
    #The above can also be done usin the next two steps below.
    # best.iter2<-gbm.perf(vacv_gbm2, plot.it = F, method = "OOB")
    # pred_vacv_gbm2<-predict.gbm(vacv_gbm2,newdata = eva, best.iter2, type = "response")
    # 
    
    #plotting ROC curve and getting the value
    vacv_auc_gbm_p2<-colAUC(pred_vacv_gbm2, eva$Vacvit , plotROC = F)
    #coombining t he value into a list
    vacv_auc_gbm2<- c(vacv_auc_gbm2, vacv_auc_gbm_p2[[1]])
    
    
  } 
  compared_model_vacv=cbind.data.frame(vacv_auc_glm, vacv_auc_gam, 
                                       vacv_auc_gbm1,vacv_auc_gbm2)
}
compared_model_vacv
mean_auc_vacv<-colMeans(compared_model_vacv)

fpa <- "C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/validation_tables/Species"
write.csv(compared_model_vacv, file=paste0(fpa,"/AUC_vacv_1.csv"))

write.csv(mean_auc_vacv, file=paste0(fpa,"/MEAN_aUC_vacv.csv"))
