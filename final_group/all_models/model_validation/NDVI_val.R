# ndviMturbation, slope processes, NDVI and
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
# m <-lm(NDVI~Radiation*RR_annual, data=data)
# 
# plot(effect(term="Radiation:RR_annual", mod=m, xlevels=5, multiline=T, colors = 2, lwd=2))
# #radiation has a high effect on ndviMturbation at high annual rainfall
# 
# m <-lm(NDVI~Radiation*Tavg_7, data=data)
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
#in predicting ndviMturbation.

#After removing the insignificant predictors manually step-by-step, I tried the stepwise
#regression
# library(MASS)
# step_ndviM_glm<-stepAIC(ndviM_glm, direction = "both")
# step_ndviM_glm$anova
# ndviM_pred<- predict(object=ras_stack, model=ndviM_glm, fun=predict, type = "response")
# plot(ndviM_pred)
# 
# 
# #final model for ndviMturbation
# ndviM_glm <- glm(NDVI~ RR_annual+ Slope +  I(Slope^2), data=data, family = "poisson") 
# #test for significance of the predictors.
# anova (ndviM_glm, test="Chisq")   #F or Chisq for gaussian. Chisq for poisson and binomial
# #summary of the model, which uses t-test for it's significance.'
# summary(ndviM_glm)
# plot(ndviM_glm, pages=1)
# 
# #based on the test from the summary and checking the correclation of the predicted with observed,
# #Tavg_7 did not improve the prediction and hence, not necessary.
# 
# #########
# #now, predict into the raster
# #to do this, i'd didide the data into 70% training data and 30% testing and perform a cross-validation
# #Will also try the LOOCV(i.e leave one out cross validation) method
# 
# ndviM_pred<- predict(model=ndviM_glm, object = ras_stack, fun=predict.glm, type = "response")
# plot(ndviM_pred)


#################################333
#GAM
# #The response curve for gam
# gam_ndviM <- gam(NDVI~ s(RR_annual, k=3)+ s(Slope, k=3), data = data, family = "poisson")
# plot(gam_ndviM, pages=1)
# summary(gam_ndviM)
# anova(gam_ndviM, test = "Chisq")
# #Others appeared to be insignificant, so , i will stick to RR_annual and slope laone
# ndviM_pred_gam<- predict(model=gam_ndviM, object = ras_stack, fun=predict.gam, type = "response")
# plot(ndviM_pred_gam)
# 


###################################################################
#GBM    
#using the normal gbm package.
# names(ras_stack)
# #GBM
# ndviM_gbm<-gbm(formula = NDVI~Altitude + NDVI+ RR_annual + Radiation+ Slope+Tavg_7, data=data,
#               distribution = "poisson",n.trees = 10000, shrinkage = 0.001, interaction.depth = 6)
# 
# best.iter<-gbm.perf(ndviM_gbm, plot.it = F, method = "OOB")
# pred_ndviM_gbm <- predict(object=ras_stack,
#                          model=ndviM_gbm, fun=predict,
#                          n.trees=ndviM_gbm$n.trees, type="response")
# plot(pred_ndviM_gbm)



#dismo package

# gbm_fit_ndviM <- gbm.step(data=data, gbm.x =
#                            c("Altitude","NDVI","RR_annual","Radiation","Slope","Tavg_7"), gbm.y = "NDVI",
#                          bag.fraction=0.75, learning.rate = 0.001,
#                          family="poisson",n.trees=50, n.folds=10,
#                          max.trees = 10000, tree.complexity = 6)
# 
# pred_gbm_ndviM <- predict(object=ras_stack,
#                          model=gbm_fit_ndviM, fun=predict,
#                          n.trees=gbm_fit_ndviM$n.trees, type="response")
# 
# plot(pred_gbm_ndviM)






################################################################
# #LOOCV
# 
# #set.seed(0)
# 
# 
# {pred_ndviM_test <- c()
# for (i in 1:nrow(data)){
#   print(i)
#   cal <- data[-i,]
#   eva <-data[i,]
#   ndviM_glm <- glm(NDVI~ RR_annual+ Slope +  I(Slope^2), data=cal, family = "poisson") 
#   
#   
#   ndviM_glm_pred_test<- predict.glm(object = ndviM_glm, newdata = eva, type="response")
#   pred_ndviM_test[i]<- ndviM_glm_pred_test
#   
#   #ndviM_pred<- predict(object=ras_stack, model=ndviM_glm, fun=predict, type = "response")
# } 
#  pred_obs <- cbind.data.frame(pred_ndviM_test, data$NDVI)
#  colnames(pred_obs)<-c("pred_ndviM", "obs_ndviM")
#  #cor(pred_ndviM_test, data$NDVI,method = "spearman")
# }
# 
# cor(pred_obs$pred_ndviM, pred_obs$obs_ndviM, method = "spearman")
# plot(pred_obs$pred_ndviM, pred_obs$obs_ndviM)



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
  ndviM_glm <- glm(NDVI~  RR_annual + Slope+ Radiation, data=cal, family = "gaussian") 

  ndviM_glm_pred<- predict.glm(object = ndviM_glm, newdata = eva, type="response")
  
  cor_glm_ndviM<-cor(ndviM_glm_pred, eva$NDVI, method = "spearman")
  #ndviM_pred_glm_ras<- predict(model=ndviM_glm, object = ras_stack, fun=predict.glm, type = "response")
  #plot(ndviM_pred_glm_ras)
  
  #########
  #mean error and root mean square error
  error_ndviM_glm<- cbind.data.frame(ndviM_glm_pred, eva$NDVI)
  colnames(error_ndviM_glm) <- c("pred_glm_ndviM", "obs_ndviM")
  
  ndviM_glm_me <- mean_error(error_ndviM_glm$obs_ndviM, error_ndviM_glm$pred_glm_ndviM)
  ndviM_glm_rmse <- rmse(error_ndviM_glm$obs_ndviM, error_ndviM_glm$pred_glm_ndviM)
  
  me_rmse_ndviM_glm <- rbind.data.frame(ndviM_glm_me, ndviM_glm_rmse)
  colnames(me_rmse_ndviM_glm)<- c("ndvi_glm")
  
  
  
  
  #GAM
  ndviM_gam <- gam(NDVI~ s(RR_annual, k=2)+ s(Slope, k=2)+ s(Radiation, k=2), data = cal, family = "gaussian")

  ndviM_gam_pred <- predict.gam(ndviM_gam, newdata = eva, type = "response")
  
  obs_pred_ndviM_gam<- cbind.data.frame(ndviM_gam_pred, eva$NDVI)
  colnames(obs_pred_ndviM_gam) <- c("pred_gam_ndviM", "obs_gam_ndviM")
  #you can just calclate the correlation straight away
  cor_gam_ndviM <- cor(ndviM_gam_pred, eva$NDVI, method = "spearman")
  
  #use the calibration data to predict into the raster stack
  #ndviM_pred_gam_ras<- predict(object=ras_stack, model=ndviM_gam, fun=predict.gam,type="response")
  #plot(ndviM_pred_gam_ras)
  
  #########
  #mean error and root mean square error
  error_ndviM_gam<- cbind.data.frame(ndviM_gam_pred, eva$NDVI)
  colnames(error_ndviM_gam) <- c("pred_gam_ndviM", "obs_ndviM")
  
  ndviM_gam_me <- mean_error(error_ndviM_gam$obs_ndviM, error_ndviM_gam$pred_gam_ndviM)
  ndviM_gam_rmse <- rmse(error_ndviM_gam$obs_ndviM, error_ndviM_gam$pred_gam_ndviM)
  
  me_rmse_ndviM_gam <- rbind.data.frame(ndviM_gam_me, ndviM_gam_rmse)
  colnames(me_rmse_ndviM_gam)<- c("ndvi_gam")
  
  
  
  
  ###################################################################3
  #using the normal gbm, package.
  #GBM
  ndviM_gbm1<-gbm(formula = NDVI~ RR_annual + Radiation + Slope, data=cal,
                              distribution = "gaussian",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                              bag.fraction = 0.75)
  
  best.iter<-gbm.perf(ndviM_gbm1, plot.it = F, method = "OOB")
  ndviM_gbm1_pred<- predict.gbm(object = ndviM_gbm1, newdata = eva, best.iter,
                               type="response")
  cor_gbm1_ndviM <- cor(ndviM_gbm1_pred, eva$NDVI, method = "spearman")
  #ndviM_pred_gbm1_ras<- predict(object=ras_stack,model=ndviM_gbm1, fun=predict,
  #                          n.trees=ndviM_gbm1$n.trees, type="response")
  # plot(ndviM_pred_gbm1_ras)
  
  #########
  #mean error and root mean square error
  error_ndviM_gbm1<- cbind.data.frame(ndviM_gbm1_pred, eva$NDVI)
  colnames(error_ndviM_gbm1) <- c("pred_gbm1_ndviM", "obs_ndviM")
  
  ndviM_gbm1_me <- mean_error(error_ndviM_gbm1$obs_ndviM, error_ndviM_gbm1$pred_gbm1_ndviM)
  ndviM_gbm1_rmse <- rmse(error_ndviM_gbm1$obs_ndviM, error_ndviM_gbm1$pred_gbm1_ndviM)
  
  me_rmse_ndviM_gbm1 <- rbind.data.frame(ndviM_gbm1_me, ndviM_gbm1_rmse)
  colnames(me_rmse_ndviM_gbm1)<- c("ndvi_gbm1")
  
  # par(mfrow=c(2,3))
  # plot.gbm(ndviM_gbm1, 1, best.iter)
  # plot.gbm(ndviM_gbm1, 2, best.iter)
  # plot.gbm(ndviM_gbm1, 3, best.iter)
  # plot.gbm(ndviM_gbm1, 4, best.iter)
  # plot.gbm(ndviM_gbm1, 5, best.iter)
  # plot.gbm(ndviM_gbm1, 6, best.iter)
  # par(mfrow=c(1,1))
  
  
  
  ###################################################
  #dismo package
  
  ndviM_gbm2 <- gbm.step(data=cal, gbm.x =
                  c("RR_annual", "Radiation", "Slope"), gbm.y = "NDVI",
                         bag.fraction=0.75, learning.rate = 0.001,
                         family="gaussian",n.trees=50, n.folds=10,
                         max.trees = 3000, tree.complexity = 6)
  #best.iter<-gbm.perf(ndviM_gbm1, plot.it = T, method = "OOB")
  # ndviM_gbm2_pred<- predict.gbm(object = ndviM_gbm2, newdata = eva, best.iter,
  #                              type="response")
  
  #this immediately does not work as expected, so, i'm using the next
  #ndviM_gbm2_pred<- predict(object=,model=ndviM_gbm2, fun=predict,n.trees=ndviM_gbm2$n.trees, type="response")
  
  ndviM_gbm2_pred <- predict.gbm(ndviM_gbm2, newdata = eva, n.trees=ndviM_gbm2$n.trees, type = "response")
  #ndviM_pred_gbm2_ras <- predict(object=ras_stack,model=ndviM_gbm2, fun=predict,
  #                         n.trees=ndviM_gbm2$n.trees, type="response")
  
  #plot(ndviM_pred_gbm2_ras)
  cor_gbm2_ndviM <- cor(ndviM_gbm2_pred, eva$NDVI, method = "spearman")
  
  #########
  #mean error and root mean square error
  error_ndviM_gbm2<- cbind.data.frame(ndviM_gbm2_pred, eva$NDVI)
  colnames(error_ndviM_gbm2) <- c("pred_gbm2_ndviM", "obs_ndviM")
  
  ndviM_gbm2_me <- mean_error(error_ndviM_gbm2$obs_ndviM, error_ndviM_gbm2$pred_gbm2_ndviM)
  ndviM_gbm2_rmse <- rmse(error_ndviM_gbm2$obs_ndviM, error_ndviM_gbm2$pred_gbm2_ndviM)
  
  me_rmse_ndviM_gbm2 <- rbind.data.frame(ndviM_gbm2_me, ndviM_gbm2_rmse)
  colnames(me_rmse_ndviM_gbm2)<- c("ndvi_gbm2")
  
  
  
  
} 
#####All correlation
all_cor_ndviM <- cbind.data.frame(cor_glm_ndviM,cor_gam_ndviM,
                                 cor_gbm1_ndviM, cor_gbm2_ndviM)
colnames(all_cor_ndviM)<- c("ndviM_glm", "ndviM_gam", "ndviM_gbm1", "ndviM_gbm2")

#####all error
all_error_ndviM <- cbind.data.frame(me_rmse_ndviM_glm, me_rmse_ndviM_gam,
                                   me_rmse_ndviM_gbm1, me_rmse_ndviM_gbm2)
rownames(all_error_ndviM)<- c("mean abs error", "RMSE")

}

fpa <- "C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/final_group/validation_tables"
write.csv(all_cor_ndviM, file=paste0(fpa,"/correlation_NDVI.csv"))

write.csv(all_error_ndviM, file=paste0(fpa,"/error_NDVI.csv"))


# # plot predicted values
# plot(ndviM_gbm1_pred, eva$NDVI)
# lines(lowess(ndviM_gbm1_pred, eva$NDVI), col= "red")
# r2data <- cor.test(predict.gbm(ndviM_gbm1_pred, eva$NDVI), NDVI)
# ndviMR2 <- (r2data$estimate)^2
# legend("topleft",paste
#        ("r^2=",round(ndviMR2, 3)), bty="n") # r^2
# 
# # plot predicted values
# plot(ndviM_gbm1_pred, eva$NDVI)
# lines(lowess(ndviM_gbm1_pred, eva$NDVI), col= "red")
# r2data <- cor_gbm1_ndviM
# ndviMR2 <- r2data^2
# legend("topleft",paste
#        ("r^2=",round(ndviMR2, 3)), bty="n") # r^2


# plot(ndviM_pred_glm_ras)
# plot(ndviM_pred_gam_ras)
# plot(ndviM_pred_gbm1_ras)
# plot(ndviM_pred_gbm2_ras)
# 
# par(mfrow=c(2,3))
# plot.gbm(ndviM_gbm1, 1, best.iter)
# plot.gbm(ndviM_gbm1, 2, best.iter)
# plot.gbm(ndviM_gbm1, 3, best.iter)
# plot.gbm(ndviM_gbm1, 4, best.iter)
# plot.gbm(ndviM_gbm1, 5, best.iter)
# plot.gbm(ndviM_gbm1, 6, best.iter)

# par(mfrow=c(1,1))  #reset plot
# summary(ndviM_gbm1,n.trees=best.iter)

# # plot predicted values
# plot(predict.gbm(ndviM_gbm1, data,
#                    best.iter, type="response"), NDVI)
# lines(lowess(predict.gbm(ndviM_gbm1, data,
#              best.iter, type="response"), NDVI), col= "red")
# r2data <- cor.test(predict.gbm(ndviM_gbm1, data,
#                                 best.iter, type="response"), NDVI)
# SPRr2 <- (r2data$estimate)^2
# legend("topleft",paste
#          ("r^2=",round(SPRr2, 3)), bty="n") # r^2




# summary(ndviM_gbm1)
# summary(ndviM_gbm2)
# summary(ndviM_pred_gbm1_ras)
# summary(ndviM_pred_gbm2_ras)

# ndviM_pred_gbm1_ras<-predict(object=ras_stack,model=ndviM_gbm1, fun=predict,
#                    n.trees=ndviM_gbm1$n.trees, type="response")
# plot(ndviM_pred_gbm1_ras)
# 
# ndviM_pred_gbm2_ras<-predict(object=ras_stack,model=ndviM_gbm2, fun=predict,
#                    n.trees=ndviM_gbm2$n.trees, type="response")
# 
# plot(ndviM_pred_gbm2_ras)
#plot.gbm(ndviM_pred_gbm2_ras)

# ?gbm.step

#