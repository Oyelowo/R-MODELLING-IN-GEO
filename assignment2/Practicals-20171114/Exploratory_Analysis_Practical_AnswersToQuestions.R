# Read data in
d <- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment2/Data-20171114/SaanaSoilMoisture.csv")

### 1) Summarize the variation in soil moisture and vegetation cover
mean(d$soil_moist)
median(d$soil_moist)
sd(d$soil_moist)
range(d$soil_moist)

mean(d$veg_cover, na.rm = TRUE)
median(d$veg_cover, na.rm = TRUE)
sd(d$veg_cover, na.rm = TRUE)
range(d$veg_cover, na.rm = TRUE)

# Plot histograms to the same figure
par(mfrow=c(1,2))
hist(d$soil_moist, main="", xlab = "Soil moisture")
abline(v=mean(d$soil_moist), col="red", lwd=2)

hist(d$veg_cover, main="", xlab = "Vegetation cover")
abline(v=mean(d$veg_cover, na.rm = TRUE), col="red", lwd=2)

### 2) Find 2.5 % and 97.5 % percentiles for soil moisture
### Calculate the length of 95 % range of variation. 
quantile(d$soil_moist, probs = c(0.025, 0.975))
quantile(d$soil_moist, probs = 0.975) - quantile(d$soil_moist, probs = 0.025)

### 3) Comparison among groups
d$mt_levels <- cut(d$mesotopo,c(0, 5, 10)) # Create a factor of two levels 
levels(d$mt_levels) <- c("valley", "rigde") # Rename the factor levels

# Plot soil moisture at each groups
par(mfrow=c(1,1)) # Return to normal graphics settings
plot(d$soil_moist ~ d$mt_levels, ylab = "Soil moisture")

tapply(d$soil_moist, d$mt_levels, mean) # Mean over the two groups
tapply(d$soil_moist, d$mt_levels, sd) # Standard deviation over the two groups

t.test(d$soil_moist~d$mt_levels)
t.test(d$veg_height~d$mt_levels)

### 4) Calculate correlation matrices for variables "mesotopo", "soil_moist", 
### "veg_height" and "veg_cover", based (i) Pearson's 
### and (ii) Spearman's correlation coefficients. 
cor(d[, c("mesotopo", "soil_moist", "veg_cover", "veg_height")], 
    use="complete.obs")

cor(d[, c("mesotopo", "soil_moist", "veg_cover", "veg_height")], 
    method="spearman", use="complete.obs")

### 5) Plot soil moisture againts all other continues variables organized as a 2 x 2 matrix.
par(mfrow=c(2,2))
plot(d$mesotopo, d$soil_moist, xlab = "Mesotopo", ylab = "Soil moisture")
mtext(side = 3, text = round(cor(d$mesotopo, d$soil_moist, method = "spearman"), 2))

plot(d$veg_height, d$soil_moist, xlab = "Vegetation height", ylab = "Soil moisture")
mtext(side = 3, text = round(cor(d$veg_height, d$soil_moist, method = "spearman"), 2))

plot(d$veg_cover, d$soil_moist, xlab = "Vegetation cover", ylab = "Soil moisture")
mtext(side = 3, text = round(cor(d$veg_cover, d$soil_moist, 
                                 method = "spearman", use = "complete.obs"), 2))

