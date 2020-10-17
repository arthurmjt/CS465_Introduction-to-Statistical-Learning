library(ISLR)

### Q2 ###
attach(Default)
set.seed(1)
## A 
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

## B
# (i)
train = sample(dim(Default)[1], dim(Default)[1]*0.8)

# (ii)
glm.fit = glm(default ~ income + balance, family = "binomial", subset = train)
summary(glm.fit)

# (iii)
prob = predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred = rep("No", length(prob))
glm.pred[prob > 0.5] = "Yes"

# (iv)
mean(glm.pred != Default[-train, ]$default)

## C
# Train:Validation = 9:1
train = sample(dim(Default)[1], dim(Default)[1]*0.9)
glm.fit = glm(default ~ income + balance, family = "binomial", subset = train)
prob = predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred = rep("No", length(prob))
glm.pred[prob > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)
# Train:Validation = 7:3
train = sample(dim(Default)[1], dim(Default)[1]*0.7)
glm.fit = glm(default ~ income + balance, family = "binomial", subset = train)
prob = predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred = rep("No", length(prob))
glm.pred[prob > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)
# Train:Validation = 5:5
train = sample(dim(Default)[1], dim(Default)[1]*0.5)
glm.fit = glm(default ~ income + balance, family = "binomial", subset = train)
prob = predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred = rep("No", length(prob))
glm.pred[prob > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)

## D
train = sample(dim(Default)[1], dim(Default)[1]*0.8)
glm.fit = glm(default ~ income + balance + student, family = "binomial", subset = train)
prob = predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred = rep("No", length(prob))
glm.pred[prob > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)


### Q3 ###
## A
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = "binomial")
summary(glm.fit)

## B
boot.fn = function(dataset, i) {
  fit = glm(default ~ income + balance, data = dataset, family = "binomial", subset = i)
  return (coef(fit))
}

## C
library(boot)
boot(Default, boot.fn, 100)

### Q4 ###
library(MASS)
attach(Boston)
set.seed(1)
## A
u = mean(medv)
u

## B
standardError = sd(medv) / sqrt(dim(Boston)[1])
standardError

## C
boot.fn = function(data, i) {
  u = mean(data[i])
  return (u)
}
boot(medv, boot.fn, 100)

## D
t.test(medv)
confidenceIn = c(22.53281 - 2 * 0.4313303, 22.53281 + 2 * 0.4313303)
confidenceIn

## E
med = median(medv)
med

## F
boot.fn = function(data, i) {
  u = median(data[i])
  return (u)
}
boot(medv, boot.fn, 100)

## G
percent = quantile(medv, c(0.1))
percent

## H
boot.fn = function(data, i) {
  u = quantile(data[i], c(0.1))
  return (u)
}
boot(medv, boot.fn, 100)
