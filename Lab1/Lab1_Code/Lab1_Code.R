#########Jingtang Ma
#########Lab1
###Q1
##a
college = read.csv("College.csv")
##b
rownames (college) = college [,1]
college = college[,-1]
##c
#I
summary(college)
#II
pairs(college[,1:10])
#III
plot(college$Private,college$Outstate)
#IV
Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]="Yes"
Elite =as.factor (Elite)
college =data.frame(college ,Elite)
summary(Elite)
summary(college)
plot(college$Elite, college$Outstate)
#V
par(mfrow=c(2,2)) 
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Top10perc)
#VI
par(mfrow=c(1,2))
college[which.min(college$Accept/college$Apps), ]
college[which.max(college$PhD), ]
plot(college$Accept/college$Apps,college$Top10perc)
plot(college$Top10perc,college$Grad.Rate)

###Q2
##a
auto = read.csv("Auto.csv", header = T, na.strings = "?")
auto = na.omit(auto)
quantitative_col = c(1,2,3,4,5,6,7)
qualitative_col = c(8,9)
##b
range(auto[, 1])
range(auto[, 2])
range(auto[, 3])
range(auto[, 4])
range(auto[, 5])
range(auto[, 6])
range(auto[, 7])
##c
sapply(auto[, quantitative_col], mean)
sapply(auto[, quantitative_col], sd)
##d
sapply(auto[-seq(10, 85), quantitative_col], range)
sapply(auto[-seq(10, 85), quantitative_col], mean)
sapply(auto[-seq(10, 85), quantitative_col], sd)
##e
par(mfrow=c(1,3))
pairs(auto[, quantitative_col])
plot(auto$cylinders, auto$mpg)
plot(auto$weight, auto$mpg)
plot(auto$weight, auto$horsepower)
##f
pairs(auto[, quantitative_col])
###Q3
##a
library(MASS)
Boston
?Boston
##b
pairs(Boston[,])
pairs(Boston[,c(1, )])
##c
par(mfrow=c(2,2))
plot(Boston$crim, Boston$age)
plot(Boston$crim, Boston$tax)
plot(Boston$crim, Boston$dis)
plot(Boston$crim, Boston$medv)
##d
par(mfrow=c(1,3))
hist(Boston$crim)
hist(Boston$tax)
hist(Boston$ptratio)
##e
sum(Boston$chas)
##f
median(Boston$ptratio)
##g
min(Boston$medv)
Boston[which.min(Boston$medv), ]
##h
sum(Boston$rm>7)
sum(Boston$rm>8)

