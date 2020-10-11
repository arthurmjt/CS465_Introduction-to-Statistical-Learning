require(ISLR)

### Q2
# A
attach(Weekly)
summary(Weekly)
pairs(Weekly) 

# B
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)

# C
glm.probs = predict(glm.fit, type = "response")
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction)
mean(glm.pred==Direction)

# D
training.data = Year<=2008
test.data = Year>2008
glm.fit2 = glm(Direction~Lag2, data= Weekly, family = binomial, subset = training.data)
summary(glm.simp)

glm.testprobs = predict(glm.simp, Weekly[test.data, ], type = "response")
glm.testpred = ifelse(glm.testprobs>0.5, "Up", "Down")
table(glm.testpred, Direction[test.data])
mean(glm.testpred==Direction[test.data])

# E
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = training.data)
lda.fit
summary(lda.fit)

lda.pred = predict(lda.fit, Weekly[test.data, ])
table(lda.pred$class, Direction[test.data])
mean(lda.pred$class==Direction[test.data])

# F
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = training.data)
qda.fit
summary(qda.fit)
qda.pred <- predict(qda.fit, Weekly[test.data, ])
table(qda.pred$class, Direction[test.data])
mean(qda.pred$class==Direction[test.data])

# G
library(class)
training.X = as.matrix(Lag2[training.data])
test.X = as.matrix(Lag2[test.data])
training.Direction = Direction[training.data]
set.seed(1)
knn.pred = knn(training.X, test.X, training.Direction, k = 1)
table(knn.pred, Direction[test.data])
mean(knn.pred==Direction[test.data])

# I
##########Logistic regression: Lag2:Lag3
glm.fit3 = glm(Direction ~ Lag2:Lag3, data = Weekly, family = binomial, subset = training.data)
glm.probs3 = predict(glm.fit3, Weekly[test.data, ], type = "response")
glm.pred3 = ifelse(glm.probs3>0.5, "Up", "Down")
table(glm.pred3, Direction[test.data])
mean(glm.pred3 == Direction[test.data])

########### LDA: Lag2:Lag3
lda.fit2 = lda(Direction ~ Lag2:Lag3, data = Weekly, subset = training.data)
lda.pred2 = predict(lda.fit2, Weekly[test.data, ])
mean(lda.pred2$class == Direction[test.data])

############ QDA: sqrt(abs(Lag2))
qda.fit2 = qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = training.data)
qda.pred2 = predict(qda.fit2, Weekly[test.data, ])
table(qda.pred2$class, Direction[test.data])
mean(qda.pred2$class == Direction[test.data])

############ KNN k = 5
knn.pred2 = knn(training.X, test.X, training.Direction, k = 5)
table(knn.pred2, Direction[test.data])
mean(knn.pred2 == Direction[test.data])

### Q3
# A
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)
head(Auto)

# B
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "year vs mpg01")
boxplot(origin ~ mpg01, data = Auto, main = "origin vs mpg01")

# C
trainid = (year %% 2 == 0)
train = Auto[trainid, ]
test = Auto[!trainid, ]
test.mpg01 = mpg01[!trainid]

# D
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = trainid)
lda.fit
lda.pred = predict(lda.fit, test)
table(lda.pred$class, test.mpg01)
lda.mean = mean(lda.pred$class == test.mpg01)
paste( "Rate of Test Error: " ,(1-lda.mean)*100, "%")

# E
qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = trainid)
qda.fit
qda.pred = predict(qda.fit, test)
table(qda.pred$class, test.mpg01)
qda.mean = mean(qda.pred$class == test.mpg01)
paste( "Rate of Test Error: " ,(1-qda.mean)*100, "%")

# F
glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = trainid)
summary(glm.fit)
glm.probs = predict(glm.fit, test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, test.mpg01)
glm.mean = mean(glm.pred == test.mpg01)
paste( "Rate of Test Error: " ,(1-glm.mean)*100, "%")

# G
training.X = cbind(cylinders, weight, displacement, horsepower)[trainid, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[!trainid, ]
training.mpg01 = mpg01[trainid]
set.seed(1)
####### K = 5
knn.pred = knn(training.X, test.X, training.mpg01, k = 5)
table(knn.pred, test.mpg01)
knn.mean = mean(knn.pred == test.mpg01)
paste( "Rate of Test Error: " ,(1-knn.mean)*100, "%")
####### K = 10
knn.pred = knn(training.X, test.X, training.mpg01, k = 10)
table(knn.pred, test.mpg01)
knn.mean = mean(knn.pred == test.mpg01)
paste( "Rate of Test Error: " ,(1-knn.mean)*100, "%")
####### K = 200
knn.pred = knn(training.X, test.X, training.mpg01, k = 200)
table(knn.pred, test.mpg01)
knn.mean = mean(knn.pred == test.mpg01)
paste( "Rate of Test Error: " ,(1-knn.mean)*100, "%")





