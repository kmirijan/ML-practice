### Q8
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
library(MASS)
?rnorm

set.seed(1)
x = rnorm(100)
err = rnorm(100)

beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3

y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + err

data.full = data.frame(x=x, y=y)
?poly

# Full
regfit.full=regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10) # raw data vs orthonganal???
regfull.summ = summary(regfit.full)
names(regfull.summ)
best.cp = which.min(regfull.summ$cp)
best.bic = which.min(regfull.summ$bic)
best.adjr2 = which.max(regfull.summ$adjr2)

par(mfrow=c(2,2))
plot(regfull.summ$cp, xlab='Num Vars', pch=19, type='b')
points(best.cp, regfull.summ$cp[best.cp], pch = 19, col = "red", lwd = 7)
plot(regfull.summ$bic, xlab='Num Vars', pch=19, type='b')
points(best.bic, regfull.summ$bic[best.bic], pch = 19, col = "red", lwd = 7)
plot(regfull.summ$adjr2, xlab='Num Vars', pch=19, type='b')
points(best.adjr2, regfull.summ$adjr2[best.adjr2], pch = 19, col = "red", lwd = 7)

coef(regfit.full, 3) #Poly 1,2,7

# Forward
regfit.fwd=regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10, method = 'forward')
regfwd.summ = summary(regfit.fwd)
best.cp = which.min(regfwd.summ$cp)
best.bic = which.min(regfwd.summ$bic)
best.adjr2 = which.max(regfwd.summ$adjr2)

par(mfrow=c(2,2))
plot(regfwd.summ$cp, xlab='Num Vars', pch=19, type='b')
points(best.cp, regfwd.summ$cp[best.cp], pch = 19, col = "red", lwd = 7)
plot(regfwd.summ$bic, xlab='Num Vars', pch=19, type='b')
points(best.bic, regfwd.summ$bic[best.bic], pch = 19, col = "red", lwd = 7)
plot(regfwd.summ$adjr2, xlab='Num Vars', pch=19, type='b')
points(best.adjr2, regfwd.summ$adjr2[best.adjr2], pch = 19, col = "red", lwd = 7)

coef(regfit.fwd, 3) #Same as full

# Backward
regfit.bwd=regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10, method = 'backward')
regbwd.summ = summary(regfit.bwd)
best.cp = which.min(regbwd.summ$cp)
best.bic = which.min(regbwd.summ$bic)
best.adjr2 = which.max(regbwd.summ$adjr2)

par(mfrow=c(2,2))
plot(regbwd.summ$cp, xlab='Num Vars', pch=19, type='b')
points(best.cp, regbwd.summ$cp[best.cp], pch = 19, col = "red", lwd = 7)
plot(regbwd.summ$bic, xlab='Num Vars', pch=19, type='b')
points(best.bic, regbwd.summ$bic[best.bic], pch = 19, col = "red", lwd = 7)
plot(regbwd.summ$adjr2, xlab='Num Vars', pch=19, type='b')
points(best.adjr2, regbwd.summ$adjr2[best.adjr2], pch = 19, col = "red", lwd = 7)

coef(regfit.bwd, 3)#Poly 1,2,9

#Fitting to Lasso
xmat=model.matrix(y~poly(x,10,raw=T), data=data.full)[, -1]
lasso.mod=glmnet(x=xmat, y=y, lambda=grid)
par(mfrow=c(1,1))
plot(lasso.mod)
lasso.cv=cv.glmnet(x=xmat, y=y)
plot(lasso.cv)
best.lambda = lasso.cv$lambda.min
best.lambda
predict(lasso.mod, type='coefficients', s=best.lambda)

# f

beta7 = 7
y = beta0 + beta7*x^7 + err
data.full = data.frame(x=x, y=y)
regfit.full=regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10)
regfull.summ = summary(regfit.full)

best.cp = which.min(regfull.summ$cp)
best.bic = which.min(regfull.summ$bic)
best.adjr2 = which.max(regfull.summ$adjr2)

par(mfrow=c(2,2))
plot(regfull.summ$cp, xlab='Num Vars', pch=19, type='b')
points(best.cp, regfull.summ$cp[best.cp], pch = 19, col = "red", lwd = 7)
plot(regfull.summ$bic, xlab='Num Vars', pch=19, type='b')
points(best.bic, regfull.summ$bic[best.bic], pch = 19, col = "red", lwd = 7)
plot(regfull.summ$adjr2, xlab='Num Vars', pch=19, type='b')
points(best.adjr2, regfull.summ$adjr2[best.adjr2], pch = 19, col = "red", lwd = 7)

#Each metric gives a different model that it thinks would be best. Probably CV testing would be good here
xmat=model.matrix(y~poly(x,10,raw=T), data=data.full)[, -1]
lasso.mod=glmnet(x=xmat, y=y, lambda=grid)
par(mfrow=c(1,1))
plot(lasso.mod)
lasso.cv=cv.glmnet(x=xmat, y=y)
plot(lasso.cv)
best.lambda = lasso.cv$lambda.min
best.lambda
predict(lasso.mod, type='coefficients', s=best.lambda)

#Here Lasso picks a 1 var model. Here CV testing against the other bestfit models or at least the 1var BIC model

### Q9
fix(College)
names(College)
set.seed(11)
train=sample(1:nrow(x), nrow(x)/2)
test = -train

College.train = College[train,]
College.test = College[test, ]

lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
College.test$Apps
summary(lm.fit)
summary(lm.pred)
mean((College.test[, "Apps"]-lm.pred)^2) # Total RSS 1,026,096

x.train = model.matrix(Apps~., data=College.train)[, -1]
x.test = model.matrix(Apps~., data=College.test)[, -1]
grid=10^seq(10, -2, length=100)
ridge.mod=cv.glmnet(x.train, College.train[, "Apps"], alpha=0, lambda=grid)
lambda.best = ridge.mod$lambda.min
lambda.best
plot(ridge.mod)
ridge.pred = predict(ridge.mod, newx=x.test, s=lambda.best)
mean((College.test[, "Apps"]-ridge.pred)^2) # Total RSS 1,025,799 Slightly better

lasso.mod=cv.glmnet(x.train, College.train[, "Apps"], lambda=grid)
plot(lasso.mod)
lambda.best = lasso.mod$lambda.min
lambda.best
lasso.pred=predict(lasso.mod, newx=x.test, s=lambda.best)
mean((College.test[, "Apps"]-lasso.pred)^2) # Total RSS 1,025,078 Slightly better again
lasso.mod=glmnet(model.matrix(Apps~., data=College), College[, "Apps"])
predict(lasso.mod, s=lambda.best, type='coefficients')

pcr.fit=pcr(Apps~., data=College.train, scale=T, validation='CV')
validationplot(pcr.fit, val.type="MSEP")
pcr.ncomp = pcr.fit$ncomp
pcr.pred = predict(pcr.fit, College.test, ncomp=pcr.ncomp)
mean((College.test[,"Apps"]-data.frame(pcr.pred)$Apps)^2) # Total RSS 1,026,096, same as lm since M = p

pls.fit=plsr(Apps~., data=College.train, scale=T, validation='CV')
validationplot(pls.fit, val.type="MSEP")
pls.ncomp = pls.fit$ncomp
pls.pred = predict(pls.fit, College.test, ncomp=pls.ncomp)
mean((College.test[,"Apps"]-data.frame(pls.pred)$Apps)^2) # Total RSS 1,026,096, same as lm since M = p

# Overall not a lot of difference between the models. They're all about the same. Regularized models did a bit better

### Q10
set.seed(2)
p=20
n=1000
x=matrix(rnorm(n*p), n, p) # Creates matrix with random n*p elements
eps = rnorm(p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
y = x%*%B + eps
train=sample(1:nrow(x), 900, replace=F)
x.train = x[train, ]
y.train = y[train, ]
x.test = x[-train, ]
y.test = y[-train, ]

data.train = data.frame(x=x.train, y=y.train)
mat.train=model.matrix(y~., data=data.train)

data.test = data.frame(x=x.test, y=y.test)
mat.test=model.matrix(y~., data=data.test)

regfit.full=regsubsets(y~., data=data.train, nvmax=p)
summary(regfit.full)
coef(regfit.full, 1)

val.errors=rep(NA, p)
for(i in 1:p){
  coefi=coef(regfit.full, id=i) #Get coefficient for every i
  pred=mat.train[, names(coefi)]%*%coefi
  val.errors[i] = mean((y.train-pred)^2)
}
plot(val.errors, pch=19, type='b')
which.min(val.errors) #Smallest MSE at 20 coef

val.errors=rep(NA, p)
for(i in 1:p){
  coefi=coef(regfit.full, id=i) #Get coefficient for every i
  pred=mat.test[, names(coefi)]%*%coefi
  val.errors[i] = mean((y.test-pred)^2)
}
plot(val.errors, pch=19, type='b')
which.min(val.errors) #Smallest MSE at 17 coef

coef = coef(regfit.full, id=17) # Coef 9 and 19 were not caught, but they were pretty close to 0
B # All the values were pretty close in fact

### Q11
set.seed(1)
fix(Boston)
sum(is.na(Boston))

# K folds CV best subset
predict.regsubsets = function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  mat[,names(coefi)]%*%coefi
}


p = ncol(Boston)-1
n = nrow(Boston)

k=10
folds=sample(1:k, n, replace=TRUE)
cv.error=matrix(NA, k, p)

for(j in 1:k){
  best.fit=regsubsets(crim~., data=Boston[folds!=j, ], nvmax=p)
  for(i in 1:p){
    pred=predict(best.fit, Boston[folds==j,], id=i)
    cv.error[j,i]=mean((Boston$crim[folds==j]-pred)^2)
  }
}
cv.error

cv.rmse=apply(cv.error, 2, mean)
which.min(cv.rmse) # Lowest error
plot(cv.rmse, pch=19, type='b')
cv.rmse[which.min(cv.rmse)]

# Lasso
test=sample(1:n, n/4)
train = -train

Boston.train = Boston[train,]
Boston.test = Boston[test, ]

x.train = model.matrix(crim~.-crim, data=Boston.train)[, -1]
x.test = model.matrix(crim~.-crim, data=Boston.test)[, -1]

grid=10^seq(10, -2, length=100)
lasso.mod=cv.glmnet(x.train, Boston.train$crim, lambda=grid)
plot(lasso.mod)
lambda.best = lasso.mod$lambda.min
coef(lasso.mod) # 1 coef
lasso.pred=predict(lasso.mod, newx=x.test, s=lambda.best)
mean((Boston.test$crim-lasso.pred)^2) # MSE 28.3 Better


# RR
ridge.mod=cv.glmnet(x.train, Boston.train$crim, lambda=grid, alpha=0)
lambda.best = ridge.mod$lambda.min
lambda.best
plot(ridge.mod)
coef(ridge.mod)
ridge.pred = predict(ridge.mod, newx=x.test, s=lambda.best)
mean((Boston.test$crim-ridge.pred)^2) # MSE 28.6 Close to lasso

# lm
lm.fit=lm(crim~., data=Boston.train)
lm.pred=predict(lm.fit, Boston.test)
mean((Boston.test$crim-lm.pred)^2) # MSE 28.3 Same as Lasso

# PCR
pcr.fit=pcr(crim~., data=Boston.train, scale=T, validation='CV')
validationplot(pcr.fit, val.type="MSEP")
pcr.ncomp = pcr.fit$ncomp
pcr.fit$ncomp
pcr.pred = predict(pcr.fit, Boston.test, ncomp=pcr.ncomp)
mean((Boston.test$crim-data.frame(pcr.pred)$crim)^2) # Exactly the same as lm since it used 13 components
