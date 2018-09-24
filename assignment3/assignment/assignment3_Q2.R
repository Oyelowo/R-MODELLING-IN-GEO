# Question 2. What is the predictive performance of the GLM, GAM and GBM models for veg_height
# and vasc_spr? Again, build the models using calibration data and test the models using evaluation
# data. Use Spearman correlation -values as the evaluation metrics. Use the same set of predictors that
# you used in question 1).
# Report the results in one short paragraph (max 5 sentences).

data<- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment3/Data-20171114 (1)/saana.csv"
                ,sep=";")


# Use the caTools package to extract the AUC values and compare them
library(caTools)
library(mgcv)
library(gbm)

#number of times to repeat the models
{rep<-7
  h_auc_glm<-c()
  h_auc_gam<-c()
  h_auc_gbm<-c()
  for (i in 1:rep){
    print(i)
    #sample all the rows, and keep 70%(0.7)
    rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
    cal<- data[rand_sam,]   #get the 70% rows for calibration
    eva<- data[-rand_sam,]  #get the remaining 30% for evaluation
    
    #create the glm for veg_height occurences
    h_glm<-glm(veg_height~mesotopo+soil_moist+soil_temp+soil_ph, data=cal,family ="gaussian")
    pred_h_glm<-predict.glm(h_glm, newdata = eva, type = "response")
    h_cor_glm<-cor(pred_h_glm, eva$veg_height, method = "spearman")
   
  
    
    #GAM
    h_gam<-gam(veg_height~s(mesotopo, k=3) + s(soil_moist, k=3) + s(soil_temp, k=3) + 
                 s(soil_ph, k=3), data=cal,family ="gaussian")
    pred_h_gam<-predict.gam(h_gam, newdata = eva, type = "response")
    h_cor_gam<-cor(pred_h_gam, eva$veg_height, method = "spearman")
    
    #GBM
    h_gbm<-gbm(formula = veg_height~mesotopo+soil_moist+soil_temp+soil_ph, data=data,
                 distribution = "gaussian",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
    best.iter<-gbm.perf(h_gbm, plot.it = F, method = "OOB")
    pred_h_gbm<-predict.gbm(h_gbm,newdata = eva, best.iter, type = "response")
    h_cor_gbm<-cor(pred_h_gbm, eva$veg_height, method = "spearman")
  } 
  compared_model_h=cbind.data.frame(h_cor_glm, h_cor_gam, h_cor_gbm)
}
compared_model_h



######################################################################
#vasc_spr

{rep<-7
vspr_auc_glm<-c()
vspr_auc_gam<-c()
vspr_auc_gbm<-c()
for (i in 1:rep){
  print(i)
  #sample all the rows, and keep 70%(0.7)
  rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
  cal<- data[rand_sam,]   #get the 70% rows for calibration
  eva<- data[-rand_sam,]  #get the remaining 30% for evaluation
  
  #create the glm for veg_height occurences
  vspr_glm<-glm(vasc_spr~mesotopo+soil_moist+soil_temp+soil_ph, data=cal,family ="poisson")
  pred_vspr_glm<-predict.glm(vspr_glm, newdata = eva, type = "response")
  vspr_cor_glm<-cor(pred_vspr_glm, eva$vasc_spr, method = "spearman")
  
  #GAM
  vspr_gam<-gam(vasc_spr~s(mesotopo, k=3) + s(soil_moist, k=3) + s(soil_temp, k=3) + 
                  s(soil_ph, k=3), data=cal,family ="poisson")
  pred_vspr_gam<-predict.gam(vspr_gam, newdata = eva, type = "response")
  vspr_cor_gam<-cor(pred_vspr_gam, eva$vasc_spr, method = "spearman")
  
  #GBM
  vspr_gbm<-gbm(formula = vasc_spr~mesotopo+soil_moist+soil_temp+soil_ph, data=data,
             distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
  best.iter<-gbm.perf(vspr_gbm, plot.it = F, method = "OOB")
  pred_vspr_gbm<-predict.gbm(vspr_gbm,newdata = eva, best.iter, type = "response")
  vspr_cor_gbm<-cor(pred_vspr_gbm, eva$vasc_spr, method = "spearman")
} 
compared_model_vspr=cbind.data.frame(vspr_cor_glm, vspr_cor_gam, vspr_cor_gbm)
}
compared_model_vspr
