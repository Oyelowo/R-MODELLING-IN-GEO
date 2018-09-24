altitude <- c(2,3,5,6,7,6,7,5,4,12,15,16,20,22)
tree_presence <- c(0,0,0,0,0,0,0,0,0,1,1,1,1,1)
tree_nr <- c(0,0,0,0,0,0,0,0,0,3,5,6,5,8)
data_tree <- data.frame(altitude,tree_presence, tree_nr)
str(data_tree)
summary(data_tree)
plot(data_tree)

# 1) Insert data in R, use c(..) -command and check that the data are valid
# 2) What is the number of trees at the altitude of 1 m and 10 m based on a
# linear model? Follow the lecture notes from week 2.
# 3) What is the probability of tree presence at the altitude of 1 m and 10 m
# based on a linear model? Follow the lecture notes from week 2.
# 4) What is the interpretation of the models?
#   5) Are these results realistic? 

m=data.frame(altitude=c(1,10))
bb<-lm(tree_nr~altitude, data = data_tree)
predict(bb, m)

        