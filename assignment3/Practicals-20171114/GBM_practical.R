



##########################################################
### The aim of this practical is to get familiar with  ###
###      Generalized Boosted Models (GBM) using R      ###
##########################################################


# GBMs (BRTs) are more complex than GAMs, but have many advantages (see ppt). 
# GBMs combine the strengths of two algorithms:
#       Regression trees (relate response to predictors by recursive binary splits)
#       Boosting (sequential fitting, combines a collection of models to give improved predictive performance)




# GETTING STARTED!


# Clear workspace
rm(list=ls())

# Read and attach the data, define --> [...] so that R can locate your data
NWdata <- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment3/Data-20171114 (1)/NW_Lapland_data.csv", header=TRUE, dec=".", sep=";") # define!
 
attach(NWdata)

# Check the data 
summary(NWdata)

# Load the GBM library
#install.packages("gbm")
library(gbm) 




# 1. GBM - model building 


# Example of some script used in our published paper: 
# "Drivers of high-latitude plant diversity hotspots and their congruence"

# As we could see from the example in the powerpoint slides,
# climatic predictors do not drive all of the variation in total species richness
# So, let's build a more realistic model to explain species richness!


# a.) Fit a gbm model of totalspr with predictors: fdd + gdd + wab + calc + relalt
# set the number of trees at 3000, learning rate (shrinkage) at 0.001, and interaction depth at 4
#?gbm
SR_gbm<-gbm(formula = totalspr~fdd + gdd + wab + calc + relalt,
            distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)


# b.) Estimate the optimal number of boosting iterations using an out-of-bag estimator, "OOB"
# use function: gbm.perf() and name it as "best.iter"
# (You don't have to worry about the warning message here)
#?gbm.perf
best.iter<-gbm.perf(SR_gbm, plot.it = T, method = "OOB")

# c.) Plot response curves (this needs to be done separately for all the predictors
# in the model) for the best model (best.iter) in same window 
# (e.g. in 2 rows and 4 columns to leave space for the plots from d.) and e.), below)
par(mfrow=c(2,4))
plot.gbm(SR_gbm, 1, best.iter)
plot.gbm(SR_gbm, 2, best.iter)
plot.gbm(SR_gbm, 3, best.iter)
plot.gbm(SR_gbm, 4, best.iter)
plot.gbm(SR_gbm, 5, best.iter)

# d.) Extract and plot the relative influence 
summary(SR_gbm)
# e.) Plot the predicted values based on the gbmSPR2 model and add lowess lines 
#     Predict to the same dataset we used to build the model (NWdata)
# remember! type="response")
#?predict.gbm()
plot(predict.gbm(SR_gbm, NWdata, best.iter, type = "response"), totalspr)

lines(lowess(predict.gbm(SR_gbm, NWdata, best.iter, type = "response"), totalspr), col="red")

# f.) Assess the fit of the model with cor.test and extract r^2
#     paste the r^2 value as a legend to the previous plot
r2data<- cor.test(predict.gbm(SR_gbm, NWdata, best.iter), totalspr)
SPRr2<- (r2data$estimate)^2
legend("topleft", paste("r^2=", round(SPRr2, 3)),  bty = "n")


# Questions:
# Looking at your response curves and relative influence table and diagram:
#   What are the relative influences of the varibles on total species richness?
#   Are they the same as your neighbours? (hint: If you used "set.seed(0)" before calling the gbm, they should be)
#   Which variable is the most influential for total species richness?
#   Are the relationships between total species richness and the predictors mainly positive or negative?
#   Is this second model better than the climate-only model (example in slides) at explaining species richness?
#   (note: we cannot, however, yet tell if the difference is significant)





# 2. Investigate the distribution of Dryas octopetala (presence-absence) in 
# relation to environmental parameters

# a.) Do 70/30 10-fold CV; make a calibration and evaluation set from the NWdata 
# b.) Fit and inspect a GBM with only climate variables
# c.) FILL IN THE SPACE and fit a GBM with all the climate and topography predictors
# d.) Compare the models, statistically and visually 

# Use the caTools package to extract the AUC values and compare them
library(caTools)

# After loading the package, name empty vectors to collect the AUC scores into
# You can give the vectors more descriptive names
# You should always null the vectors before running the for-loop

AUC_vector1 <- NULL
AUC_vector2 <- NULL

# How many times should the model validate itself
reps <- 10

for (i in 1:reps) {
  
  print(i)
  randomsam <- sample(1:1451, 1016) #70% random sample of all the observation data 
  datacal <- NWdata[randomsam,] #70% calibration
  dataeva <- NWdata[-randomsam,] #30% evaluation
  
  # 1. CLIMATE MODEL
  gbmdryoct <- gbm(dryoct ~ gdd + wab + fdd, distribution="bernoulli", n.trees = 3000, interaction.depth = 4, data=datacal)
  best.iter <- gbm.perf(gbmdryoct,method="OOB",plot.it=FALSE) #Estimates optimal number of boosting iterations for gbm object 
  preddryoct <- predict.gbm(gbmdryoct, dataeva, best.iter, type="response") #Predicted values based on a GBM object: Returns vector of predictions. e.g. for Bernoulli the returned value is on the log odds scale,
  aucl <- colAUC(preddryoct, dataeva$dryoct, plotROC=FALSE) #gbm-prediction AUC-score for the evaluation data
  AUC_vector1 <- c(AUC_vector1, aucl[[1]])
  
  # 2. FULL MODEL
  ## FILL IN THIS SPACE ##
  gbmdryoct2 <- gbm(dryoct ~ fdd + +wab+gdd + wab + calc + relalt, distribution="bernoulli", n.trees = 3000, interaction.depth = 4, data=datacal)
  best.iter2 <- gbm.perf(gbmdryoct2,method="OOB",plot.it=FALSE) #Estimates optimal number of boosting iterations for gbm object 
  preddryoct2 <- predict.gbm(gbmdryoct2, dataeva, best.iter2, type="response") #Predicted values based on a GBM object: Returns vector of predictions. e.g. for Bernoulli the returned value is on the log odds scale,
  auc2 <- colAUC(preddryoct2, dataeva$dryoct, plotROC=FALSE) #gbm-prediction AUC-score for the evaluation data
  AUC_vector2 <- c(AUC_vector2, auc2[[1]])
  
}


## Are the differences between the models significant?
compare_model<-cbind.data.frame(AUC_vector1, AUC_vector2)
# Use a PAIRED Wilcoxons test to compare AUC values
# hint: ?wilcox.test
?wilcox.test

wilcox.test(AUC_vector1, AUC_vector2, paired = T)


# 3. Compare how total species richness and rare species richness
# are affected by environmental predictors (follow same logic as previous for-loop exercise)

# Responses are found in columns 3 and 4
# Plot results on e.g. 2 rows with 7 columns for easy comparison
#   Fit model with all predictors and the correct family
#   Display r^2
#   Display summary

# Briefly describe:
# a.) the model statistics
# b.) the effects of environmental predictors on these two responses









# 4. Let's say we want to take a quick look at what effect GDD, the 
# most important predictor for species richness, has on the different species

# Use similar steps as above, but for the individual species (presence/absence)
# and put them inside a for-loop, and loop through the species distributions and model their gdd response curves
# Hint: Before the loop, specify the species you want to model, which data, which column
# e.g. par(mfrow=c(3,3)) would help to inspect results

# Questions:
# What effect does GDD have on the different species?
# Give an example of a species found in cooler sites (not likely to be present in areas of high gdd)






 
