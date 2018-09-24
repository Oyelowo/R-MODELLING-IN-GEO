# Question 4. Does the cover of Empetrum hermaphroditum (Empher_cover) have an effect on the
# vasc_spr when all other predictors are controlled for? Use the same set of predictors as used in
# question 1). Use all three modelling frameworks to test the hypothesis.
# Report the results in one short paragraph (max 5 sentences).
# The main idea behind this question: as a dominant species Empher_cover might
# have a strong influence on the vegetation properties - can we see the effect?
#   Please, test it!

data<- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment3/Data-20171114 (1)/saana.csv"
                ,sep=";")

# Use the caTools package to extract the AUC values and compare them
library(caTools)
library(mgcv)
library(gbm)

attach(data)

#create the glm for vasc_spr occurences'
#GLM
vaspr_glm<-glm(vasc_spr~Empher_cover+mesotopo+soil_moist+soil_temp+soil_ph, data=data,family ="poisson")
summary(vaspr_glm)

#GAM
vaspr_gam<-gam(vasc_spr~s(Empher_cover)+s(mesotopo)+s(soil_moist)+
                   s(soil_temp)+s(soil_ph), data=data,family ="poisson")
summary(vaspr_gam)
    
#GBM
vaspr_gbm<-gbm(formula = vasc_spr~Empher_cover+mesotopo+soil_moist+soil_temp+soil_ph, data=data,
                 distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
summary(vaspr_gbm)

#From the above, Empher_cover appears to be a significant predictor and has the third relative
#importance as shown in GBM





{rep<-7
  vaspr_auc_glm<-vaspr_auc_gam<-vaspr_auc_gbm<-c()
  for (i in 1:rep){
    print(i)
    rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
    cal<- data[rand_sam,]
    eva<- data[-rand_sam,]
    vaspr_glm<-glm(vasc_spr~mesotopo+soil_moist+soil_temp+soil_ph, data=cal,family ="poisson")
    pred_vaspr_glm<-predict.glm(vaspr_glm, newdata = eva, type = "response")
    vaspr_auc_glm_p<-colAUC(pred_vaspr_glm, eva$vasc_spr, plotROC=F)
    vaspr_auc_glm <- c(vaspr_auc_glm, vaspr_auc_glm_p[[1]])
    
    #GAM
    vaspr_gam<-gam(vasc_spr~s(mesotopo, k=3) + s(soil_moist, k=3) + s(soil_temp, k=3) + 
                   s(soil_ph, k=3), data=cal,family ="poisson")
    pred_vaspr_gam<-predict.gam(vaspr_gam, newdata = eva, type = "response")
    vaspr_auc_gam_p<-colAUC(pred_vaspr_gam, eva$vasc_spr, plotROC=F)
    vaspr_auc_gam <- c(vaspr_auc_gam, vaspr_auc_gam_p[[1]])
    
    #GBM
    vaspr_gbm<-gbm(formula = vasc_spr~mesotopo+soil_moist+soil_temp+soil_ph, data=cal,
                 distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
    best.iter<-gbm.perf(vaspr_gbm, plot.it = F, method = "OOB")
    pred_vaspr_gbm<-predict.gbm(vaspr_gbm,newdata = eva, best.iter, type = "response")
    vaspr_auc_gbm_p<-colAUC(pred_vaspr_gbm, eva$vasc_spr, plotROC = F)
    vaspr_auc_gbm<- c(vaspr_auc_gbm, vaspr_auc_gbm_p[[1]])
  } 
  compared_model_vaspr1=cbind.data.frame(vaspr_auc_glm, vaspr_auc_gam, vaspr_auc_gbm)
}





{rep<-7
  vaspr_auc_glm<-vaspr_auc_gam<-vaspr_auc_gbm<-c()
  for (i in 1:rep){
    print(i)
    rand_sam<-sample(1:nrow(data), size = 0.7*nrow(data) )
    cal<- data[rand_sam,]
    eva<- data[-rand_sam,]
    vaspr_glm<-glm(vasc_spr~Empher_cover+mesotopo+soil_moist+soil_temp+soil_ph, data=cal,family ="poisson")
    pred_vaspr_glm<-predict.glm(vaspr_glm, newdata = eva, type = "response")
    vaspr_auc_glm_p<-colAUC(pred_vaspr_glm, eva$vasc_spr, plotROC=F)
    vaspr_auc_glm <- c(vaspr_auc_glm, vaspr_auc_glm_p[[1]])
    
    #GAM
    vaspr_gam<-gam(vasc_spr~s(Empher_cover, k=3)+  s(mesotopo, k=3) + s(soil_moist, k=3) + s(soil_temp, k=3) + 
                     s(soil_ph, k=3), data=cal,family ="poisson")
    pred_vaspr_gam<-predict.gam(vaspr_gam, newdata = eva, type = "response")
    vaspr_auc_gam_p<-colAUC(pred_vaspr_gam, eva$vasc_spr, plotROC=F)
    vaspr_auc_gam <- c(vaspr_auc_gam, vaspr_auc_gam_p[[1]])
    
    #GBM
    vaspr_gbm<-gbm(formula = vasc_spr~Empher_cover+mesotopo+soil_moist+soil_temp+soil_ph, data=cal,
                   distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
    best.iter<-gbm.perf(vaspr_gbm, plot.it = F, method = "OOB")
    pred_vaspr_gbm<-predict.gbm(vaspr_gbm,newdata = eva, best.iter, type = "response")
    vaspr_auc_gbm_p<-colAUC(pred_vaspr_gbm, eva$vasc_spr, plotROC = F)
    vaspr_auc_gbm<- c(vaspr_auc_gbm, vaspr_auc_gbm_p[[1]])
  } 
  compared_model_vaspr2=cbind.data.frame(vaspr_auc_glm, vaspr_auc_gam, vaspr_auc_gbm)
}

compared_model_vaspr1
compared_model_vaspr2

wilcox.test(mean(compared_model_vaspr1[,1]), mean(compared_model_vaspr2[,1]))
wilcox.test(mean(compared_model_vaspr1[,2]), mean(compared_model_vaspr2[,2]))
wilcox.test(mean(compared_model_vaspr1[,3]), mean(compared_model_vaspr2[,3]))

#I decided to try my hands on building the model with and without Empher_cover and comparing the auc
#values. There seems to be a slight improvement, as shown in the tables, However, the predictions did
#not improve significantly




######################################################################
# #vasc_spr
#  
# 
# 
# 
#    
# #create a function to calculate the deviance  
# #Explained deviance (D-squared) = (Null deviance - Residual deviance) / Null deviance
# d2<- function(model, digits=4){
#   dev<-(model$null.deviance- model$deviance)/(model$null.deviance)
#   #can also be simply written as
#   #(1- (model$deviance/model$null.deviance))
#   return(round(dev, digits))
# } 
# 
#   summary(vspr_glm)
#   vspr_glm$deviance
# 
# 
#   
#   d2 = function(model, digits = 4) 
#     { round(1-(model$deviance/model$null.deviance),digits=digits) }  
#   
# dsquared(vspr_glm)
#   
