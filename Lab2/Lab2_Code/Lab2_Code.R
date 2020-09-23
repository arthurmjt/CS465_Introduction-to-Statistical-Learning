library(ISLR)
###Q1###
fit1 = lm(mpg ~ horsepower, data = Auto)
summary(fit1)
#predicted mpg associated with a horsepower of 98
predict(fit1, data.frame(horsepower = 98))
#95% prediction intervals
predict(fit1, data.frame(horsepower = 98), interval="prediction", level=0.95)
#95% confidence intervals
predict(fit1, data.frame(horsepower = 98), interval="confidence", level=0.95)
plot(Auto$horsepower, Auto$mpg, xlab = "horsepower", ylab = "mpg")
abline(fit1, col = "red")
par(mfrow = c(2, 2))
plot(fit1)

###Q2###
pairs(Auto)
names(Auto)
cor(Auto[1:8])
fit2 = lm(mpg ~ .-name, Auto)
summary(fit2)
par(mfrow = c(2, 2))
plot(fit2)
fit3 = lm(mpg ~ displacement * horsepower+displacement * weight, Auto); summary(fit3)
#Try a few different transformations of the variables, such as log(X), X, X . Comment on your findings.
par(mfrow = c(2, 2))
plot(mpg ~ horsepower, Auto)
abline(fit1, col = "red")

fit_log = lm(mpg ~ log(horsepower), Auto)
plot(mpg ~ log(horsepower), Auto)
abline(fit_log, col = "red")

fit_sqrt = lm(mpg ~ sqrt(horsepower), Auto)
plot(mpg ~ sqrt(horsepower), Auto)
abline(fit_sqrt, col = "red")

fit_squa = lm(mpg ~ (horsepower)^2, Auto)
plot(mpg ~ (horsepower)^2, data = Auto, xlab = "horsepower^2")
abline(fit_squa, col = "red")

###Q3###
Carseats
fit4 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(fit4)
fit5 = lm(Sales ~ Price + US, data = Carseats)
summary(fit5)
confint(fit5, level = 0.95)
par(mfrow = c(2, 2))
plot(fit5)

###Q4###
set.seed(1)
x = rnorm(100) 
y=2*x+rnorm (100)
fit6 = lm(y ~ x + 0)
summary(fit6)
fit7 = lm(x ~ y + 0)
summary(fit7)
len = length(x)
t = sqrt(len - 1)*(x %*% y)/sqrt(sum(x^2) * sum(y^2) - (x %*% y)^2)
print(as.numeric(t))
fit8 = lm(y ~ x)
summary(fit8)
fit9 = lm(x ~ y)
summary(fit9)
