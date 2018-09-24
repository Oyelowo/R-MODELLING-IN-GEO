# Read data in 
d <- read.csv("F:\\Opetus\\AAM2017\\Harjoitukset\\AirTemperatureData.csv", sep=";")
d$x <- d$x/1000; d$y <- d$y/1000; 

### 1) Based on the data ("AirTemperatureData.csv"), 
### are air temperatures related to proximity to Baltic sea ("sea")? 
### Is this relationship straight line or curvilinear? 
### Plot both linear and quadratic response curves. 
model1 <- lm(temp~sea, data=d); anova(model1, test="F")
model2 <- lm(temp~sea+I(sea^2), data=d); anova(model2, test="F")

# Answer: the relationship is linear (2nd order term not significant)

plot(d$sea, d$temp, xlab="Sea variable", ylab="Air temperature")
abline(model1, col="red")
new <- data.frame(sea=seq(0, 100, 1))
p <- predict(model2, new)
lines(p, col="blue", lwd=2)

### 2) Describe is the effect of latitude on average air temperatures?
model3 <- lm(temp ~ y + I(y^2), data=d); anova(model3, test="F")
summary(model3)

# Answer: air temperatures have a curvilinear relationship with latitude
# (positive parabolic). Latitude explains ~ 83 % of the variation in 
# air temperatures

### 3) According to our data, which one explains air temperatures better, 
### lake predictor or longitude (x coordinate)? Plot response curves
model4 <- lm(temp ~ lake + I(lake^2), data=d); anova(model4, test="F")
summary(model4)$adj.r.squared

plot(d$lake, d$temp, xlab="Lake variable", ylab="Air temperature")
new2 <- data.frame(lake=seq(0, 100, 1))
p <- predict(model4, new2)
lines(p, col="blue", lwd=2)

# Lake explains ~ 7 % of the variation in air temperatures.
# Is the response curve based on 2nd order polynomial realistic?

model5 <- lm(temp ~ x + I(x^2), data=d); anova(model5, test="F")
summary(model5)$adj.r.squared

plot(d$x, d$temp, xlab="X coordinate", ylab="Air temperature")
new3 <- data.frame(x=seq(min(d$x), max(d$x), 1))
p <- predict(model5, new3)
lines(p, col="blue", lwd=2)

# Answer: longitude explains more variation in air temperatures than lake

### 4) Using July precipitation data ("PrecipitationData.csv") 
### what single predictor explains the most variation in monthly precipitation sums?

d2 <- read.csv("F:\\Opetus\\AAM2017\\Harjoitukset\\PrecipitationData.csv", sep=";")
model1 <- lm(prec~sea+I(sea^2), data=d2); anova(model1, test="F")
summary(model1)$adj.r.squared

model2 <- lm(prec~lake+I(lake^2), data=d2); anova(model2, test="F")
summary(model2)$adj.r.squared

model3 <- lm(prec~elev+I(elev^2), data=d2); anova(model3, test="F")
summary(model3)$adj.r.squared

model4 <- lm(prec~x+I(x^2), data=d2); anova(model4, test="F")
summary(model4)$adj.r.squared

model5 <- lm(prec~y+I(y^2), data=d2); anova(model5, test="F")
summary(model5)$adj.r.squared

# Answer: latitude

### 5) Fit models of precipitation sum using (i) first order elevation term, 
### and (ii) first and second order elevation terms. 
### Which of the models fits best to the data? Based on the models, 
### what are the predicted precipitation sum at 4500 m a.s.l.? 

model1 <- lm(prec~elev, data=d2)
model2 <- lm(prec~elev+I(elev^2), data=d2)
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
anova(model1, model2)

# Answer: model with 2nd order term does not imporve the first model
# -> model1 is better. 
# Moreover, it's always preferrable to use simpler model over a complex one

predict(model1, data.frame(elev=4500)) # ~ 895 mm
predict(model2, data.frame(elev=4500)) # ~ -2452 mm

