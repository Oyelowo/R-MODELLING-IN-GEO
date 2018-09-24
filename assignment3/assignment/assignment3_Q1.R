# Question 1: Divide the saana.csv data randomly into two different datasets:
#   ??? model calibration data (70%)
# ??? model evaluation data (30%)
# Build the models based on the calibration data and test the predictive performance of the models
# using the evaluation data. What is the predictive performance of the GLM, GAM and GBM models
# for Betnan, Cashyp, Empher and Salret based on AUC-values of the model evaluation data? Use
# mesotopo, soil_moist, soil_temp and soil_ph as predictors.
# Report the results in one short paragraph (max 5 sentences).
# Note: you can use sample-function to divide the data, e.g.

data<- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment3/Data-20171114 (1)/saana.csv"
                ,sep=";")

# Use the caTools package to extract the AUC values and compare them
library(caTools)

#number of times to repeat the models
rep<-20
bet_auc_vector1<-c()
for (i in 1:rep){
  print(i)
  rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
  cal<- data[rand_sam,]
  eva<- data[-rand_sam,]
  bet_glm<-glm(Betnan~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_bet<- eva[,grep("Betnan", colnames(data))]
  #which(colnames(data)=="Betnan") or grep("Betnan", colnames(data))
  pred_bet<-predict.glm(bet_glm, newdata = eva, type = "response")
  bet_auc<-colAUC(pred_bet, eva$Betnan, plotROC=F)
  bet_auc_vector1 <- c(bet_auc_vector1, bet_auc[[1]])
  
  bet_glm<-glm(Cashyp~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_bet<- eva[,grep("Betnan", colnames(data))]
  #which(colnames(data)=="Betnan") or grep("Betnan", colnames(data))
  pred_bet<-predict.glm(bet_glm, newdata = eva, type = "response")
  bet_auc<-colAUC(pred_bet, eva$Betnan, plotROC=F)
  bet_auc_vector1 <- c(bet_auc_vector1, bet_auc[[1]])
  
  bet_glm<-glm(Empher~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_bet<- eva[,grep("Betnan", colnames(data))]
  #which(colnames(data)=="Betnan") or grep("Betnan", colnames(data))
  pred_bet<-predict.glm(bet_glm, newdata = eva, type = "response")
  bet_auc<-colAUC(pred_bet, eva$Betnan, plotROC=F)
  bet_auc_vector1 <- c(bet_auc_vector1, bet_auc[[1]])
  
  bet_glm<-glm(Salret~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_bet<- eva[,grep("Betnan", colnames(data))]
  #which(colnames(data)=="Betnan") or grep("Betnan", colnames(data))
  pred_bet<-predict.glm(bet_glm, newdata = eva, type = "response")
  bet_auc<-colAUC(pred_bet, eva$Betnan, plotROC=F)
  bet_auc_vector1 <- c(bet_auc_vector1, bet_auc[[1]])
  }

aucl <- colAUC(preddryoct, dataeva$dryoct, plotROC=FALSE) #gbm-prediction AUC-score for the evaluation data
AUC_vector1 <- c(AUC_vector1, aucl[[1]])