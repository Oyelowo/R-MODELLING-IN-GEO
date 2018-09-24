# Read data in 
d <- read.csv("F:\\Opetus\\AAM2017\\Harjoitukset\\AirTemperatureData.csv", sep=";")

### 1) Based on our data ("AirTemperatureData.csv"), what are single most 
# influential variables (elevation, lake, sea, geographic location) 
# explaining the spatial variation in average September air temperatures in Finland? 
# Are the effects (i) linear or curvilinear and (ii) positive or negative? 

model1 <- lm(temp ~ elev + I(elev^2), data=d); summary(model1)
anova(model1, test="F")
# curvilinear, positive parabolic, R-squared = 0.65

model2 <- lm(temp ~ lake + I(lake^2), data=d); summary(model2)
anova(model2, test="F")
# curvilinear, positive parabolic, R-squared = 0.07

model3 <- lm(temp ~ sea + I(sea^2), data=d); summary(model3)
anova(model3, test="F")
model3 <- lm(temp ~ sea, data=d); summary(model3)
# positive linear, R-squared = 0.35

model4 <- lm(temp ~ x + I(x^2), data=d); summary(model4)
anova(model4, test="F")
# curvilinear, positive parabolic, R-squared = 0.17

model5 <- lm(temp ~ y + I(y^2), data=d); summary(model5)
anova(model5, test="F")
# curvilinear, positive parabolic, R-squared = 0.83

### 2) What is the effect of elevation on air temperatures, 
### after the effect of other environmental factors is controlled
### (based on first order polynomial terms)? How would you interpret the results?
model6 <- lm(temp ~ elev + lake + sea + x + y, data=d)
anova(model6, test = "F")
summary(model6)

# Answer: temperatures decrease 0.004deg/m -> 4deg/km

### 3) What is the effect of lake predictor of air temperatures after the 
### effect of other environmental factors is controlled (based on first order terms)? 
### How much of the variation in air temperatures this model explains?
summary(model6)

# Answer: temperatures increase by 0.01 deg by one percent increase in lake cover
# -> 0.1deg / 10% increase in lake cover

### 4) Find the most parsimonious model explaining average air temperatures 
### using backward stepwise variable selection (based on first and second order terms, 
### define interaction term for geographical location [x and y]). 
### Consider for multicollinearity among predictors (r<[0.7]). 

# First let's see how the predictors are correlated with each other
cor(d[,c("elev", "lake", "sea", "x", "y")], method="spearman")

# elevation and sea variable are stronly correlated (rs=-0.81),
# so we can omit sea from the subsequent model

# First, fit a full model (all predictors except "sea")
model7 <- lm(temp ~ elev + I(elev^2) + lake + I(lake^2) + x + y + I(x^2) + I(y^2) + x:y, data=d)
anova(model7, test = "F")

# Drop x-y interaction
model8 <- lm(temp ~ elev + I(elev^2) + lake + I(lake^2) + x + y + I(x^2) + I(y^2), data=d)
anova(model8, test = "F")

# # Drop x^2 
model9 <- lm(temp ~ elev + I(elev^2) + lake + I(lake^2) + x + y + I(y^2), data=d)
anova(model9, test = "F")
