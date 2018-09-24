# slopePRturbation, slope processes, NDVI and
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



#plot(ras_stack)
# 
# attach(data)
# 
# dim(data)
# 
# str(data)
# 
# summary(data)
# 
# #glimpse(data)
# plot(data[1:3])
# 
# #let's see the distribution of the predictors and response variables
# predictors <- data[,c("Altitude", "NDVI", "RR_annual","Radiation", "Slope","Tavg_7")]
# ggpairs(predictors)
# 
# #Now, divide NDVI into classes
# predictors$NDVI_classes<- cut(predictors$NDVI, breaks=c(0,0.5,1), labels=c( 
#                      "low", "high"))
# #use the classfiied NDVI to visualise
# ggpairs(predictors,aes(col=NDVI_classes), lower = list(combo = wrap("facethist", bins = 20)),
#         title = "Distribution of Predictors")
# 
# 
# 
# predictors2 <- data[,c("Altitude", "NDVI", "RR_annual","Radiation", "Slope","Tavg_7")]
# 
# predictors2$slope_classes<- cut(predictors$Slope, breaks=c(0,15, 30,40), labels=c( 
#   "low", "medium","top"))
# ggpairs(predictors2,aes(col=slope_classes), lower = list(combo = wrap("facethist", bins = 20)),
#         title = "Distribution of Predictors")
# 
# 
# #remember to use ggplot to explore the distribution of the predictors
# response_var <- data[, c()]
# 
# cor_matrix<- cor(data[1:12])
# corrplot(cor_matrix, method = "pie", type="upper" )
# corrplot(cor_matrix, method = "number", type="upper" )
# 


#####################################################################################3
#CRYOTURBATION
#I decided to use the below because they have correlations below 0.7
#Next, i check if they are curvilinear and also check if they are interrelated

##############CHECKING INTERACTIONS BETWEEN PREDICTORS################
# require(effects); require(viridis)
# m <-lm(Slopeproc~Radiation*RR_annual, data=data)
# 
# plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
# #radiation has a high effect on slopePRturbation at high annual rainfall
# 
# m <-lm(Slopeproc~Radiation*Tavg_7, data=data)
# 
# plot(effect(term="Radiation:Tavg_7", mod=m,xlevels=10, multiline=T, colors = 2, lwd=2))
# 

#After testing the model, using anova and the summary of the model(which uses t-test), we found out that
# that the interactions and other second orders are insignificant.
#however, because of the discrepancy between the significance test of the anova and the summary of the
#mode, I will be building two models with and without the T_Avg and use wilcox test to see
#if it improves the model significantly.

#note: dont use auc for poisson and gaussiam because it is meant for binary variables.

#Slope is curvillinear(i.e) the second order is significant. However, the third order is not signnificant 
#in predicting slopePRturbation.

#After removing the insignificant predictors manually step-by-step, I tried the stepwise
#regression
# library(MASS)
# step_slopePR_glm<-stepAIC(slopePR_glm, direction = "both")
# step_slopePR_glm$anova
# slopePR_pred<- predict(object=ras_stack, model=slopePR_glm, fun=predict, type = "response")
# plot(slopePR_pred)
# 
# 
# #final model for slopePRturbation
# slopePR_glm <- glm(Slopeproc~ RR_annual+ Slope +  I(Slope^2), data=data, family = "poisson") 
# #test for significance of the predictors.
# anova (slopePR_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for poisson and binomial
# #summary of the model, which uses t-test for it's significance.'
# summary(slopePR_glm)
# plot(slopePR_glm, pages=1)
# 
# #based on the test from the summary and checking the correclation of the predicted with observed,
# #Tavg_7 did not improve the prediction and hence, not necessary.
# 
# #########
# #now, predict into the raster
# #to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
# #Will also try the LOOCV(i.e leave one out cross validation) method
# 
# slopePR_pred<- predict(model=slopePR_glm, object = ras_stack, fun=predict.glm, type = "response")
# plot(slopePR_pred)


#################################333
#GAM
# #The response curve for gam
# gam_slopePR <- gam(Slopeproc~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "poisson")
# plot(gam_slopePR, pages=1)
# summary(gam_slopePR)
# anova(gam_slopePR, test = "Chisq")
# #Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
# slopePR_pred_gam<- predict(model=gam_slopePR, object = ras_stack, fun=predict.gam, type = "response")
# plot(slopePR_pred_gam)
# 


###################################################################
#GBM    
#using the normal gbm package.
# names(ras_stack)
# #GBM
# slopePR_gbm<-gbm(formula = Slopeproc~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
#               distribution = "poisson",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)
# 
# best.iter<-gbm.perf(slopePR_gbm, plot.it = F, method = "OOB")
# pred_slopePR_gbm <- predict(object=ras_stack,
#                          model=slopePR_gbm, fun=predict,
#                          n.trees=slopePR_gbm$n.trees, type="response")
# plot(pred_slopePR_gbm)



#dismo package

# gbm_fit_slopePR <- gbm.step(data=data, gbm.x =
#                            c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "Slopeproc",
#                          bag.fraction=0.75, learning.rate = 0.001,
#                          family="poisson",n.trees=50, n.folds=10,
#                          max.trees = 10000, tree.complexity = 6)
# 
# pred_gbm_slopePR <- predict(object=ras_stack,
#                          model=gbm_fit_slopePR, fun=predict,
#                          n.trees=gbm_fit_slopePR$n.trees, type="response")
# 
# plot(pred_gbm_slopePR)






################################################################
# #LOOCV
# 
# #set.seed(0)
# 
# 
# {pred_slopePR_test <- c()
# for (i in 1:nrow(data)){
#   print(i)
#   cal <- data[-i,]
#   eva <-data[i,]
#   slopePR_glm <- glm(Slopeproc~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "poisson") 
#   
#   
#   slopePR_glm_pred_test<- predict.glm(object = slopePR_glm, newdata = eva, type="response")
#   pred_slopePR_test[i]<- slopePR_glm_pred_test
#   
#   #slopePR_pred<- predict(object=ras_stack, model=slopePR_glm, fun=predict, type = "response")
# } 
#  pred_obs <- cbind.data.frame(pred_slopePR_test, data$Slopeproc)
#  colnames(pred_obs)<-c("pred_slopePR", "obs_slopePR")
#  #cor(pred_slopePR_test, data$Slopeproc,method = "spearman")
# }
# 
# cor(pred_obs$pred_slopePR, pred_obs$obs_slopePR, method = "spearman")
# plot(pred_obs$pred_slopePR, pred_obs$obs_slopePR)



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
{rep<-10
for (i in 1:rep){
  print(i)
  
  #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
  rand<- sample(1:nrow(data), size = 0.7*nrow(data))
  cal<- data[rand,]
  eva<-data[-rand,]
  slopePR_glm <- glm(Slopeproc~  Slope+ I(Slope^2) +I(Slope^3) +RR_annual
                     , data=cal, family = "poisson")
    
  slopePR_glm_pred<- predict.glm(object = slopePR_glm, newdata = eva, type="response")
  
  cor_glm_slopePR<-cor(slopePR_glm_pred, eva$Slopeproc, method = "spearman")
  #slopePR_pred_glm_ras<- predict(model=slopePR_glm, object = ras_stack, fun=predict.glm, type = "response")
  #plot(slopePR_pred_glm_ras)
  
  #########
  #mean error and root mean square error
  error_slopePR_glm<- cbind.data.frame(slopePR_glm_pred, eva$Slopeproc)
  colnames(error_slopePR_glm) <- c("pred_glm_slopePR", "obs_slopePR")
  
  slopePR_glm_me <- mean_error(error_slopePR_glm$obs_slopePR, error_slopePR_glm$pred_glm_slopePR)
  slopePR_glm_rmse <- rmse(error_slopePR_glm$obs_slopePR, error_slopePR_glm$pred_glm_slopePR)
  
  me_rmse_slopePR_glm <- rbind.data.frame(slopePR_glm_me, slopePR_glm_rmse)
  colnames(me_rmse_slopePR_glm)<- c("slopePR_glm")
  
  
  
  
  #GAM
  slopePR_gam <- gam(Slopeproc~s(Slope, k=2) + s(RR_annual, k=2), data = cal, family = "poisson")

  slopePR_gam_pred <- predict.gam(slopePR_gam, newdata = eva, type = "response")
  
  obs_pred_slopePR_gam<- cbind.data.frame(slopePR_gam_pred, eva$Slopeproc)
  colnames(obs_pred_slopePR_gam) <- c("pred_gam_slopePR", "obs_gam_slopePR")
  #you can just calclate the correlation straight away
  cor_gam_slopePR <- cor(slopePR_gam_pred, eva$Slopeproc, method = "spearman")
  
  #use the calibration data to predict into the raster stack
  #slopePR_pred_gam_ras<- predict(object=ras_stack, model=slopePR_gam, fun=predict.gam,type="response")
  #plot(slopePR_pred_gam_ras)
  
  #########
  #mean error and root mean square error
  error_slopePR_gam<- cbind.data.frame(slopePR_gam_pred, eva$Slopeproc)
  colnames(error_slopePR_gam) <- c("pred_gam_slopePR", "obs_slopePR")
  
  slopePR_gam_me <- mean_error(error_slopePR_gam$obs_slopePR, error_slopePR_gam$pred_gam_slopePR)
  slopePR_gam_rmse <- rmse(error_slopePR_gam$obs_slopePR, error_slopePR_gam$pred_gam_slopePR)
  
  me_rmse_slopePR_gam <- rbind.data.frame(slopePR_gam_me, slopePR_gam_rmse)
  colnames(me_rmse_slopePR_gam)<- c("slopePR_gam")
  
  
  
  
  ###################################################################3
  #using the normal gbm, package.
  #GBM1
  slopePR_gbm1<-gbm(formula = Slopeproc~ Slope + RR_annual , data=cal,
                    distribution = "poisson",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                    bag.fraction = 0.75)
  
  best.iter<-gbm.perf(slopePR_gbm1, plot.it = F, method = "OOB")
  slopePR_gbm1_pred<- predict.gbm(object = slopePR_gbm1, newdata = eva, best.iter,
                               type="response")
  cor_gbm1_slopePR <- cor(slopePR_gbm1_pred, eva$Slopeproc, method = "spearman")
  #slopePR_pred_gbm1_ras<- predict(object=ras_stack,model=slopePR_gbm1, fun=predict,
  #                          n.trees=slopePR_gbm1$n.trees, type="response")
  # plot(slopePR_pred_gbm1_ras)
  
  #########
  #mean error and root mean square error
  error_slopePR_gbm1<- cbind.data.frame(slopePR_gbm1_pred, eva$Slopeproc)
  colnames(error_slopePR_gbm1) <- c("pred_gbm1_slopePR", "obs_slopePR")
  
  slopePR_gbm1_me <- mean_error(error_slopePR_gbm1$obs_slopePR, error_slopePR_gbm1$pred_gbm1_slopePR)
  slopePR_gbm1_rmse <- rmse(error_slopePR_gbm1$obs_slopePR, error_slopePR_gbm1$pred_gbm1_slopePR)
  
  me_rmse_slopePR_gbm1 <- rbind.data.frame(slopePR_gbm1_me, slopePR_gbm1_rmse)
  colnames(me_rmse_slopePR_gbm1)<- c("slopePR_gbm1")
  
  # par(mfrow=c(2,3))
  # plot.gbm(slopePR_gbm1, 1, best.iter)
  # plot.gbm(slopePR_gbm1, 2, best.iter)
  # plot.gbm(slopePR_gbm1, 3, best.iter)
  # plot.gbm(slopePR_gbm1, 4, best.iter)
  # plot.gbm(slopePR_gbm1, 5, best.iter)
  # plot.gbm(slopePR_gbm1, 6, best.iter)
  # par(mfrow=c(1,1))
  
  
  
  ###################################################
  #dismo package
  
  slopePR_gbm2 <- gbm.step(data=cal, gbm.x =
                             c( "Slope", "RR_annual"), gbm.y = "Slopeproc",
                           bag.fraction=0.75, learning.rate = 0.001,
                           family="poisson",n.trees=50, n.folds=10,
                           max.trees = 3000, tree.complexity = 6)
  #best.iter<-gbm.perf(slopePR_gbm1, plot.it = T, method = "OOB")
  # slopePR_gbm2_pred<- predict.gbm(object = slopePR_gbm2, newdata = eva, best.iter,
  #                              type="response")
  
  #this immediately does not work as expected, so, i'm using the next
  #slopePR_gbm2_pred<- predict(object=,model=slopePR_gbm2, fun=predict,n.trees=slopePR_gbm2$n.trees, type="response")
  
  slopePR_gbm2_pred <- predict.gbm(slopePR_gbm2, newdata = eva, n.trees=slopePR_gbm2$n.trees, type = "response")
  #slopePR_pred_gbm2_ras <- predict(object=ras_stack,model=slopePR_gbm2, fun=predict,
  #                         n.trees=slopePR_gbm2$n.trees, type="response")
  
  #plot(slopePR_pred_gbm2_ras)
  cor_gbm2_slopePR <- cor(slopePR_gbm2_pred, eva$Slopeproc, method = "spearman")
  
  #########
  #mean error and root mean square error
  error_slopePR_gbm2<- cbind.data.frame(slopePR_gbm2_pred, eva$Slopeproc)
  colnames(error_slopePR_gbm2) <- c("pred_gbm2_slopePR", "obs_slopePR")
  
  slopePR_gbm2_me <- mean_error(error_slopePR_gbm2$obs_slopePR, error_slopePR_gbm2$pred_gbm2_slopePR)
  slopePR_gbm2_rmse <- rmse(error_slopePR_gbm2$obs_slopePR, error_slopePR_gbm2$pred_gbm2_slopePR)
  
  me_rmse_slopePR_gbm2 <- rbind.data.frame(slopePR_gbm2_me, slopePR_gbm2_rmse)
  colnames(me_rmse_slopePR_gbm2)<- c("slopePR_gbm2")
  
  
  
  
} 
#####All correlation
all_cor_slopePR <- cbind.data.frame(cor_glm_slopePR,cor_gam_slopePR,
                                 cor_gbm1_slopePR, cor_gbm2_slopePR)
colnames(all_cor_slopePR)<- c("slopePR_glm", "slopePR_gam", "slopePR_gbm1", "slopePR_gbm2")

#####all error
all_error_slopePR <- cbind.data.frame(me_rmse_slopePR_glm, me_rmse_slopePR_gam,
                                   me_rmse_slopePR_gbm1, me_rmse_slopePR_gbm2)
rownames(all_error_slopePR)<- c("MAE", "RMSE")

}

fpa <- "C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/validation_tables"
write.csv(all_cor_slopePR, file=paste0(fpa,"/correlation_Slopeproc.csv"))

write.csv(all_error_slopePR, file=paste0(fpa,"/error_Slopeproc.csv"))


# # plot predicted values
# plot(slopePR_gbm1_pred, eva$Slopeproc)
# lines(lowess(slopePR_gbm1_pred, eva$Slopeproc), col= "red")
# r2data <- cor.test(predict.gbm(slopePR_gbm1_pred, eva$Slopeproc), Slopeproc)
# slopePRR2 <- (r2data$estimate)^2
# legend("topleft",paste
#        ("r^2=",round(slopePRR2, 3)), bty="n") # r^2
# 
# # plot predicted values
# plot(slopePR_gbm1_pred, eva$Slopeproc)
# lines(lowess(slopePR_gbm1_pred, eva$Slopeproc), col= "red")
# r2data <- cor_gbm1_slopePR
# slopePRR2 <- r2data^2
# legend("topleft",paste
#        ("r^2=",round(slopePRR2, 3)), bty="n") # r^2


# plot(slopePR_pred_glm_ras)
# plot(slopePR_pred_gam_ras)
# plot(slopePR_pred_gbm1_ras)
# plot(slopePR_pred_gbm2_ras)
# 
# par(mfrow=c(2,3))
# plot.gbm(slopePR_gbm1, 1, best.iter)
# plot.gbm(slopePR_gbm1, 2, best.iter)
# plot.gbm(slopePR_gbm1, 3, best.iter)
# plot.gbm(slopePR_gbm1, 4, best.iter)
# plot.gbm(slopePR_gbm1, 5, best.iter)
# plot.gbm(slopePR_gbm1, 6, best.iter)

# par(mfrow=c(1,1))  #reset plot
# summary(slopePR_gbm1,n.trees=best.iter)

# # plot predicted values
# plot(predict.gbm(slopePR_gbm1, data,
#                    best.iter, type="response"), Slopeproc)
# lines(lowess(predict.gbm(slopePR_gbm1, data,
#              best.iter, type="response"), Slopeproc), col= "red")
# r2data <- cor.test(predict.gbm(slopePR_gbm1, data,
#                                 best.iter, type="response"), Slopeproc)
# SPRr2 <- (r2data$estimate)^2
# legend("topleft",paste
#          ("r^2=",round(SPRr2, 3)), bty="n") # r^2




# summary(slopePR_gbm1)
# summary(slopePR_gbm2)
# summary(slopePR_pred_gbm1_ras)
# summary(slopePR_pred_gbm2_ras)

# slopePR_pred_gbm1_ras<-predict(object=ras_stack,model=slopePR_gbm1, fun=predict,
#                    n.trees=slopePR_gbm1$n.trees, type="response")
# plot(slopePR_pred_gbm1_ras)
# 
# slopePR_pred_gbm2_ras<-predict(object=ras_stack,model=slopePR_gbm2, fun=predict,
#                    n.trees=slopePR_gbm2$n.trees, type="response")
# 
# plot(slopePR_pred_gbm2_ras)
#plot.gbm(slopePR_pred_gbm2_ras)

# ?gbm.step

#