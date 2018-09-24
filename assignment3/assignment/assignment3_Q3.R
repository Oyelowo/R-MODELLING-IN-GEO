# Question 3. 
#Characterize soil_moist, soil_temp, soil_ph, veg_height and vasc_spr conditions along the
# mesotopographic gradient using GAM. Model the values of these five responses at the valley bottom
# (mesotopo 1), mid-slope (mesotopo 5) and ridge-top (mesotopo 10). Present the results as an
# informative figure.
# Report the results in one short paragraph (max 5 sentences).
library(mgcv)
data<- read.csv("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/assignment3/Data-20171114 (1)/saana.csv"
                ,sep=";")
attach(data)
#This is the first method makes the prediction without classifying the mesotopography
#GAM 
##Soil moisture
gam_moist <- gam(soil_moist~s(mesotopo, k=3), data = data, family = "gaussian")
summary(gam_moist)

par(mfrow=c(1,2))
plot(gam_moist, main = "soil moisture")

#the values at the valley bottom, mid-slope and ridge-top
mesotopo2 <- c(1,5,10)
newdata <- data.frame(mesotopo=mesotopo2)
pred.gam_moist <- predict.gam(gam_moist, newdata, type="response")
plot(mesotopo, soil_moist, pch=19, cex=0.2, col="grey",type="n", main="soil moisture") 
lines(mesotopo2, pred.gam_moist, lty=1,lwd= 2,col="orange") 
points(mesotopo2, pred.gam_moist, lty=1,lwd= 2,col="green")

#soil temperature
gam_temp<- gam(soil_temp~s(mesotopo, k=3), data = data, family = "gaussian")
summary(gam_temp)   #summary soil temperature

par(mfrow=c(1,2))
plot(gam_temp, main="soil temperature") #response curve
pred.gam_temp <- predict.gam(gam_temp, newdata, type="response")
plot(mesotopo, soil_temp, pch=19, cex=0.2, col="grey",type="n", main = "soil temp vs mesotopo") 
lines(mesotopo2, pred.gam_temp, lty=1,lwd= 2,col="orange")
points(mesotopo2, pred.gam_temp, lty=1,lwd= 2,col="orange")

#soil pH
gam_ph<- gam(soil_ph~s(mesotopo, k=3), data = data, family = "gaussian")
summary(gam_ph)   #summary soil_pH

par(mfrow=c(1,2))
plot(gam_ph)  #response curve

pred.gam_ph <- predict.gam(gam_ph, newdata, type="response")
plot(mesotopo, soil_ph, pch=19, cex=0.2, col="grey",type="n") 
lines(mesotopo2, pred.gam_ph, lty=1,lwd= 2,col="orange")
points(mesotopo2, pred.gam_ph, lty=1,lwd= 2,col="orange")



par(mfrow=c(1,2))
gam_vh<- gam(veg_height~s(mesotopo, k=3), data = data, family = "poisson")
summary(gam_vh)
plot(gam_ph)
pred.gam_ph <- predict.gam(gam_ph, newdata, type="response")
plot(mesotopo, soil_ph, pch=19, cex=0.2, col="grey",type="n") 
lines(mesotopo2, pred.gam_ph, lty=1,lwd= 2,col="orange")
points(mesotopo2, pred.gam_ph, lty=1,lwd= 2,col="orange")

#Vascular species richness
gam_vaspr<- gam(vasc_spr~s( mesotopo, k=3), data = data, family = "poisson")
summary(gam_vaspr)

par(mfrow=c(1,2))
plot(gam_vaspr, main = "Vasc_Spr")
pred.gam_vaspr <- predict.gam(gam_vaspr, newdata, type="response")
plot(mesotopo, vasc_spr, pch=19, cex=0.2, col="grey",type="n", main = "Vasc_Spr vs mesotopo") 
lines(mesotopo2, pred.gam_vaspr, lty=1,lwd= 2,col="orange")
points(mesotopo2, pred.gam_vaspr, lty=1,lwd= 2,col="orange")


###############################
#data$topo_level<-cut(mesotopo, breaks = c(0,4,7,10))
#levels(data$topo_level)<-c("valley-bottom","mid-slope","ridge-top")

val_bot<-data[mesotopo==1,]    #valley bottom
mid_sl<-data[mesotopo==5,]      #mid-slope
r_top<-data[mesotopo==10,]      #ridge-top

#create data frame to impute the modlled values at variouis topo gradients.
topo<-matrix(ncol = 5, nrow = 3)
topo<- data.frame(topo)
row.names(topo)<- c("valley_bottom", "mid-slope", "ridge-top")
colnames(topo)<-c("soil_moist", "soil_temp", "soil_ph", "vasc_spr", "veg_height")

#predicting the values at the valley bottom for the responses
vb1<- topo[1,1]<- mean(predict.gam(gam_moist, val_bot, type="response"))
vb2<- topo[1,2]<- mean(predict.gam(gam_temp, val_bot, type="response"))
vb3<- topo[1,3]<- mean(predict.gam(gam_ph, val_bot, type="response"))
vb4<- topo[1,4]<- mean(predict.gam(gam_vaspr, val_bot, type="response"))
vb5<- topo[1,5]<- mean(predict.gam(gam_vh, val_bot, type="response"))

#predicting the values at the mid-slope for the responses
ms1<- topo[2,1]<- mean(predict.gam(gam_moist, mid_sl, type="response"))
ms2<- topo[2,2]<- mean(predict.gam(gam_temp, mid_sl, type="response"))
ms3<- topo[2,3]<- mean(predict.gam(gam_ph, mid_sl, type="response"))
ms4<- topo[2,4]<- mean(predict.gam(gam_vaspr, mid_sl, type="response"))
ms5<- topo[2,5]<- mean(predict.gam(gam_vh, mid_sl, type="response"))

#predicting the values at the ridge-top for the responses
rt1<- topo[3,1]<- mean(predict.gam(gam_moist, r_top, type="response"))
rt2<- topo[3,2]<- mean(predict.gam(gam_temp, r_top, type="response"))
rt3<- topo[3,3]<- mean(predict.gam(gam_ph, r_top, type="response"))
rt4<- topo[3,4]<- mean(predict.gam(gam_vaspr, r_top, type="response"))
rt5<- topo[3,5]<- mean(predict.gam(gam_vh, r_top, type="response"))

#view the dataframe
topo

#see the boxplot
boxplot(topo)
