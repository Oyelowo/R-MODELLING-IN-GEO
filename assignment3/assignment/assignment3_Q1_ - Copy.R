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
library(mgcv)
library(gbm)

#number of times to repeat the models
{rep<-7
bet_auc_glm<-c()
bet_auc_gam<-c()
bet_auc_gbm<-c()
for (i in 1:rep){
  print(i)
  rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
  cal<- data[rand_sam,]
  eva<- data[-rand_sam,]
  bet_glm<-glm(Betnan~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_bet<- eva[,grep("Betnan", colnames(data))]
  #which(colnames(data)=="Betnan") or grep("Betnan", colnames(data))
  pred_bet_glm<-predict.glm(bet_glm, newdata = eva, type = "response")
  
  bet_auc_glm_p<-colAUC(pred_bet_glm, eva$Betnan, plotROC=T)
  bet_auc_glm <- c(bet_auc_glm, bet_auc_glm_p[[1]])
  
  #GAM
  bet_gam<-gam(Betnan~s(mesotopo,soil_moist,soil_temp,soil_ph, k=3), data=data,family ="binomial")
  pred_bet_gam<-predict.gam(bet_gam, newdata = eva, type = "response")
  bet_auc_gam_p<-colAUC(pred_bet_gam, eva$Betnan, plotROC=T)
  bet_auc_gam <- c(bet_auc_gam, bet_auc_gam_p[[1]])
  
  #GBM
  bet_gbm<-gbm(formula = Betnan~mesotopo+soil_moist+soil_temp+soil_ph, data=data,
              distribution = "bernoulli",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
  best.iter<-gbm.perf(bet_gbm, plot.it = F, method = "OOB")
  pred_bet_gbm<-predict.gbm(bet_gbm,newdata = eva, best.iter, type = "response")
  bet_auc_gbm_p<-colAUC(pred_bet_gbm, eva$Betnan, plotROC = T)
  bet_auc_gbm<- c(bet_auc_gbm, bet_auc_gbm_p[[1]])
  
  # plot(predict.gbm(SR_gbm, NWdata, best.iter, type = "response"), totalspr)
} 
compared_model_bat=cbind.data.frame(bet_auc_glm, bet_auc_gam, bet_auc_gbm)
}


######################################################################
#Cashyp

{rep<-7
  cas_auc_glm<-c()
  cas_auc_gam<-c()
  cas_auc_gbm<-c()
  for (i in 1:rep){
    print(i)
    rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
    cal<- data[rand_sam,]
    eva<- data[-rand_sam,]
    cas_glm<-glm(Cashyp~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
    eva_cas<- eva[,grep("Cashyp", colnames(data))]
    #which(colnames(data)=="Cashyp") or grep("Cashyp", colnames(data))
    pred_cas_glm<-predict.glm(cas_glm, newdata = eva, type = "response")
    
    cas_auc_glm_p<-colAUC(pred_cas_glm, eva$Cashyp, plotROC=T)
    cas_auc_glm <- c(cas_auc_glm, cas_auc_glm_p[[1]])
    
    #GAM
    cas_gam<-gam(Cashyp~s(mesotopo,soil_moist,soil_temp,soil_ph, k=3), data=data,family ="binomial")
    pred_cas_gam<-predict.gam(cas_gam, newdata = eva, type = "response")
    cas_auc_gam_p<-colAUC(pred_cas_gam, eva$Cashyp, plotROC=T)
    cas_auc_gam <- c(cas_auc_gam, cas_auc_gam_p[[1]])
    
    #GBM
    cas_gbm<-gbm(formula = Cashyp~mesotopo+soil_moist+soil_temp+soil_ph, data=data,
                 distribution = "bernoulli",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
    best.iter<-gbm.perf(cas_gbm, plot.it = F, method = "OOB")
    pred_cas_gbm<-predict.gbm(cas_gbm,newdata = eva, best.iter, type = "response")
    cas_auc_gbm_p<-colAUC(pred_cas_gbm, eva$Cashyp, plotROC = T)
    cas_auc_gbm<- c(cas_auc_gbm, cas_auc_gbm_p[[1]])
    
    # plot(predict.gbm(SR_gbm, NWdata, best.iter, type = "response"), totalspr)
  } 
  compared_model_cas=cbind.data.frame(cas_auc_glm, cas_auc_gam, cas_auc_gbm)
}




######################################################################
#Empher

{rep<-7
emp_auc_glm<-c()
emp_auc_gam<-c()
emp_auc_gbm<-c()
for (i in 1:rep){
  print(i)
  rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
  cal<- data[rand_sam,]
  eva<- data[-rand_sam,]
  emp_glm<-glm(Empher~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_emp<- eva[,grep("Empher", colnames(data))]
  #which(colnames(data)=="Empher") or grep("Empher", colnames(data))
  pred_emp_glm<-predict.glm(emp_glm, newdata = eva, type = "response")
  
  emp_auc_glm_p<-colAUC(pred_emp_glm, eva$Empher, plotROC=T)
  emp_auc_glm <- c(emp_auc_glm, emp_auc_glm_p[[1]])
  
  #GAM
  emp_gam<-gam(Empher~s(mesotopo,soil_moist,soil_temp,soil_ph, k=3), data=data,family ="binomial")
  pred_emp_gam<-predict.gam(emp_gam, newdata = eva, type = "response")
  emp_auc_gam_p<-colAUC(pred_emp_gam, eva$Empher, plotROC=T)
  emp_auc_gam <- c(emp_auc_gam, emp_auc_gam_p[[1]])
  
  #GBM
  emp_gbm<-gbm(formula = Empher~mesotopo+soil_moist+soil_temp+soil_ph, data=data,
               distribution = "bernoulli",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
  best.iter<-gbm.perf(emp_gbm, plot.it = F, method = "OOB")
  pred_emp_gbm<-predict.gbm(emp_gbm,newdata = eva, best.iter, type = "response")
  emp_auc_gbm_p<-colAUC(pred_emp_gbm, eva$Empher, plotROC = T)
  emp_auc_gbm<- c(emp_auc_gbm, emp_auc_gbm_p[[1]])
  
  # plot(predict.gbm(SR_gbm, NWdata, best.iter, type = "response"), totalspr)
} 
compared_model_emp=cbind.data.frame(emp_auc_glm, emp_auc_gam, emp_auc_gbm)
}





######################################################################
#Salret

{rep<-7
sal_auc_glm<-c()
sal_auc_gam<-c()
sal_auc_gbm<-c()
for (i in 1:rep){
  print(i)
  rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
  cal<- data[rand_sam,]
  eva<- data[-rand_sam,]
  sal_glm<-glm(Salret~mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="binomial")
  eva_sal<- eva[,grep("Salret", colnames(data))]
  #which(colnames(data)=="Salret") or grep("Salret", colnames(data))
  pred_sal_glm<-predict.glm(sal_glm, newdata = eva, type = "response")
  
  sal_auc_glm_p<-colAUC(pred_sal_glm, eva$Salret, plotROC=F)
  sal_auc_glm <- c(sal_auc_glm, sal_auc_glm_p[[1]])
  
  #GAM
  sal_gam<-gam(Salret~s(mesotopo,soil_moist,soil_temp,soil_ph, k=3), data=data,family ="binomial")
  pred_sal_gam<-predict.gam(sal_gam, newdata = eva, type = "response")
  sal_auc_gam_p<-colAUC(pred_sal_gam, eva$Salret, plotROC=T)
  sal_auc_gam <- c(sal_auc_gam, sal_auc_gam_p[[1]])
  
  #GBM
  sal_gbm<-gbm(formula = Salret~mesotopo+soil_moist+soil_temp+soil_ph, data=data,
               distribution = "bernoulli",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
  best.iter<-gbm.perf(sal_gbm, plot.it = F, method = "OOB")
  pred_sal_gbm<-predict.gbm(sal_gbm,newdata = eva, best.iter, type = "response")
  sal_auc_gbm_p<-colAUC(pred_sal_gbm, eva$Salret, plotROC = F)
  sal_auc_gbm<- c(sal_auc_gbm, sal_auc_gbm_p[[1]])
  
  # plot(predict.gbm(SR_gbm, NWdata, best.iter, type = "response"), totalspr)
} 
compared_model_sal=cbind.data.frame(sal_auc_glm, sal_auc_gam, sal_auc_gbm)
}















# 
# lines(eva_bet, pred_bet)
#   wilcox.test(bet_auc_glm, bet_auc_gam, paired = F)
  
  