library(ISLR)

# Validation Set Approach
set.seed(1)
?sample
train=sample(392,196)

lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
lm.pred = predict(lm.fit, Auto)
mean((mpg-lm.pred)[-train]^2)
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
lm.pred2 = predict(lm.fit2, Auto)
mean((mpg-lm.pred2)[-train]^2)
lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
lm.pred3 = predict(lm.fit3, Auto)
mean((mpg-lm.pred3)[-train]^2)

set.seed(2)
train=sample(392,196)

lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
lm.pred = predict(lm.fit, Auto)
mean((mpg-lm.pred)[-train]^2)
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
lm.pred2 = predict(lm.fit2, Auto)
mean((mpg-lm.pred2)[-train]^2)
lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
lm.pred3 = predict(lm.fit3, Auto)
mean((mpg-lm.pred3)[-train]^2)

# LOOCV
glm.fit=glm(mpg~horsepower, data=Auto) # This is just lm. By not doing family.
?glm
coef(glm.fit)
lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.error = cv.glm(Auto, glm.fit)
cv.error
?cv.glm
cv.error$delta
cv.error = rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# k-fold CV
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# The Bootstrap
alpha.fn = function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y) - cov(X,Y)) / (var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
 
boot(Portfolio, alpha.fn, R=1000) # This is the same as the last one but with 1000 estimates

boot.fn=function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
boot.fn(Auto, sample(392, 392, replace=T))

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef
boot.fn = function(data, index){
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))
