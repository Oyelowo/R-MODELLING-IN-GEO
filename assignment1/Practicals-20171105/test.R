library(boot)

boot::brambles
ages<- table(brambles$age)
barplot(ages)
par(new=T)
hist(brambles$x, breaks = 10)
boxplot(brambles[,c(1,2)], range = 0.1)
plot(y~x, data = brambles, col = factor(brambles$age), pch = 16)

a<- seq(from=4, to=202)
b<- seq(from=4, to=400, by=2)
plot(a,b, type = "l")
lines(a-102, b-150, col="blue")
?scale
?Sample

plot(x = c(2,8), y = c(7,7), xlim = c(0,10), ylim = c(0,10), col = "blue")
lines(c(6,4,3,4,6)~c(1,3,5,7,9), col = "red")
dev.off()
plot(x = c(1:9),
     y = c(7,8.5,9,8,7,8,9,8.5,7),
     xlim = c(0,10),
     col = "red",
     type = "l",ylim = c(1,10))
cc<-(1:50)**3
cc     
quantile(cc)
quantile(cc, probs = 0.5)
median(cc)
max(cc)

fc<-runif(n=100,min = 0,max = 1)
fc
hist(fc, breaks = 5)
mean(0:1)
bs<- replicate(n=1222, expr = mean(sample(brambles$x,replace = T)))
bs <- replicate(n = 999, expr = mean(sample(brambles$x,replace = TRUE)))
bs <- c(bs, mean(brambles$x))
confi <- quantile(bs,probs = c(0.025, 0.975))
hist(bs)
abline(v=confi, lty=2)
bs

options(stringsAsFactors = F)
getwd()
setwd("C:/Users/oyeda/Desktop/MODELLING_PHYSICAL_GEOGRAPHY/Practicals-20171105")
list.files("sentence")

paths<- list.files("sentence", full.names = T)
paths
str(paths)
words <- lapply(paths, FUN = read.csv)
words
head(words)
rbind(words[[1]], words[[2]], words[[3]])


words<- do.call(what = rbind, args = words) #combine data frames to one
words[,2]

str(words)
words <- words[order(words$wordno),]
words
cat(words $word)
write.csv(words, file = "words.csv", row.names = FALSE)

if(NROW(brambles) < 10){
  cat("The dataset is small")
} else {
  cat("The dataset is large")
}
?cat


for(i in 1:6) {
  cat(" Round", i)
}

for(i in 1:6) {
  print(paste("Round ",i))
}


names <- c("Juha", "Konsta", "Annina", "Miska")
for(nimi in names){
  print(paste0(nimi, ", step forward!"))
}

for(i in names){
  if(i %in% c("Konsta", "Juha")){
    print("There you are teacher!")
  } else {
    print("You are not the teacher today!")
  }
}


for(i in names){
  if(i == "Konsta"){
    next # skips this iteration
  } else if(i == "Juha"){
    7
    print("There you are Juha!")
  } else if(i == "Annina") {
    print("There you are Annina!")
    break
  } else {
    print("Hello professor!")
  }
}


## Create the container: a numeric vector with length equaling
## the number of operations we need to conduct.
## Here we calculate the mean for each column of brambles
result <- numeric(NCOL(brambles))
#result <- brambles[1,]
#result <- c()
for(i in 1:NCOL(brambles)){
  result[i] <- mean(brambles[,i])
}
result

sapply(brambles,mean)

colMeans(brambles)
