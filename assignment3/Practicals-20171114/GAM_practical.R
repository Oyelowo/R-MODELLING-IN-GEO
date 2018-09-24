



##########################################################
### The aim of this practical is to get familiar with  ###
###     Generalized Additive Models (GAM) using R      ###
##########################################################




# GETTING STARTED!


# Clear workspace
rm(list=ls())

# Read and attach the data, define --> [...] so that R can locate your data
NWdata <- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment3/Data-20171114 (1)/NW_Lapland_data.csv", header=TRUE, dec=".", sep=";") # define!
 
attach(NWdata)

# Check the data (e.g. that each var is in it's own column, and that there are no NA's)
summary(NWdata)

# Load the GAM library
library(mgcv) 




# 1. GAM - Model building

# You will need to supply the gam() function with certain elements:
#     The response variable 
#     The predictor variable/s, and for each predictor variable:
#         s = smoothing spline
#         k = number of knots/degrees of freedom (d.o.f) for spline 
#     Family = gaussian, binomial, poisson..


# Let's build a GAM to explain variation in total plant 
#species richness (totalspr; the response) with
# growing season temperature (gdd; the predictor) using 4 d.o.f ("k")

# a.) First, what "family" does the response variable belong to?
# b.) Build and name the model 
# c.) Get the model summary

# Question:
# Is growing season temperature is significant for species richness?


# We can also use this model to predict Yi (observation of totalspr) at Xi (gdd at the site of observation)
# i.e. what is the species richness in any particular GDD condition

# First, let's create a new data frame of gdd ("gdd2"") to predict plant richness into:

gdd2 <- seq(min(gdd), max(gdd), 0.01)
newdata <- data.frame(gdd=gdd2)

# d.) PREDICT gam1 into the new data
pred.gam1 <- X 
# remember to use: type = "response"!


# Then, plot the variables in the model and add lines of the predictions
# for example, as follows:
plot(gdd, totalspr, pch=19, cex=0.2, col="grey",type="n") 
lines(gdd2, pred.gam1, lty=1,lwd= 2,col="red") 

# Questions:
# How much of the variation in species richness can gdd explain? Is it enough? (see model summary)







# Let's build a GAM to model species richness but this time
# using three climatic predictors to make the model more realistic

# a.) Build the model (using 4 dof)

# b.) Extract a summary

# c.) Plot all 3 response curves in one window

# Questions:
# What do the statistical (summary) and visual (plot) clues tell you about the model?:

# Based on the model summary:
# Is the effect of climate on species richness significant?
# What is the adjusted R-sq., and what does this tell you about the model?

# Based on the response curve plot:
# What do the shapes of the curves tell you?
# What are the precise freedom terms?
# Are the smooth terms needed?
# What causes the slight increase in the confidence interval at both ends of the gradient?

# Is there autocorrelation present that would prevent us from using these variables?
cor(NWdata[,6:8], method = "spearman") # plots correlations for columns 6 to 8 in the data (our climatic var)

# d.) Use anova and AIC to compare the models (1 variable vs 3 variables)

# Question:
# According to anova and AIC, which GAM has a better fit?





# 2. GAM - Variable selection 
                                                    
# Using multiple predictors can increase the realism of GAM models 
                                                    
# Let's investigate the best predictors for two plant species:
#       "Empher" (Empetrum hermaphroditum, arctic crowberry/variksenmarja), and 
#       "Dryoct" (Dryas octopetala, mountain avens/lapinvuokko), 
# in relation to environmental parameters using GAM models
 
# Name the responses
# Choose four environmental predictors from the NW data set:
#     growing degree days
#     relative altitude
#     water balance
#     calcareousness
# Set degrees of freedom
# Set family for this presence/absence response 
  
# a.) Fit the models for the species (build two gams)

# b.) View the model parameters

# c.) Plot all four response curves of both models on the same page:



# Questions:
# What does the model tell you statistically speaking?
# What conclusions can you reach regarding the individual effects?
# Are the variables statistically significant?
# What are the differences between the models? 
# Does Dryas octopetala prefer sites that have more calcareous soils?
# Which of the plant species can be explained better with this model?
# How would you improve the models?

 








# 3. Investigate the distribution of Dryas octopetala (presence-absence) in 
# relation to environmental parameters. Look for the nonlinear effects 
# for each covariate by using GAMs.

# a.) Fit the first GAM with only climate variables
# check response curves
# plot response curves on same page

# b.) Fit a second GAM with two topo-edaphic variables 
# check response curves
# plot both response curves on same page

# c.) Thirdly, fit a GAM with all the climate and topography predictors
# check response curves
# plot all response curves on the same page

# d.) Compare all the models, statistically and visually 

# Then answer the following:
# What do the models tell you?
# What do the statistical test/s you chose tell you?

# Which model accounts for the most variance in the species' distribution?
# Which variables are most important?
#     What conclusions can you reach regarding the individual effects?
#     Are they significant?
#     Should some only be included as a linear effect? Why?
#     Should some not be included at all? Why?


# Finally, fit the best GAM you can with THREE of the most important predictors
# Check response curves
# Plot all response curves on the same page
# Is this model better than the model with more, but insignificant, variables?
# Which model would you use?



                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                    
                                                  