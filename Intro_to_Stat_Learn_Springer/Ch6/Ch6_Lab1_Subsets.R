### Best Subset

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

install.packages("leaps")
library(leaps)
?regsubsets
regfit.full=regsubsets(Salary~., data=Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab='Num Vars', ylab='RSS', type="b")
plot(reg.summary$adjr2, xlab='Num Vars', ylab='AR2', type="b")

which.max(reg.summary$adjr2) #Identify maximum point of adj r2
points(11, reg.summary$adjr2[11], col='red', cex=2, pch=20) # Plot max point

plot(reg.summary$cp, xlab='Num Vars', ylab='Cp', type="b")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col='red', cex=2, pch=20) # Plot min point

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab='Num Vars', ylab='BIC', type="b")
points(6, reg.summary$bic[6], col='red', cex=2, pch=20) # Plot min point

?plot.regsubsets
plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

coef(regfit.full, 6)

### Forward/Backward Stepwise Subset

regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method = 'forward') #Forward
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax=19, method = 'backward') #Backward
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

### Validation Set and Cross Validation

set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE) # Creates a vector as big as Hitters with all Trues and Falses. A True means that row is train
test=(!train)

regfit.best=regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~., data=Hitters[test,]) # Builds an X matrix from test data???
?model.matrix
test.mat

val.errors=rep(NA, 19)
val.errors

coefi = coef(regfit.best, id=2)
coefi
pred=test.mat[, names(coefi)]%*%coefi
pred

for(i in 1:19){
  coefi=coef(regfit.best, id=i) #Get coefficient for every i
  pred=test.mat[, names(coefi)]%*%coefi # Get predictions at every value I think???
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2) # Average out the errors of the test vs predictors MSE
}
val.errors
which.min(val.errors)
coef(regfit.best, 7)

predict.regsubsets = function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  mat[,names(coefi)]%*%coefi
}

regfit.best=regsubsets(Salary~., data=Hitters, nvmax=19)
k=10
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
folds
table(folds)
cv.error=matrix(NA, k ,19)
cv.error

for(j in 1:k){
  best.fit=regsubsets(Salary~., data=Hitters[folds!=j, ], nvmax=19, method='forward')
  for(i in 1:19){
    pred=predict(best.fit, Hitters[folds==j,], id=i)
    cv.error[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.error=apply(cv.error, 2, mean) #Average over the columns of this matrix
?apply
which.min(mean.cv.error) # 12 is lowest for me
par(mfrow=c(1,1))
plot(mean.cv.error, pch=19, type='b')

reg.best = regsubsets(Salary~., data=Hitters[folds!=j, ], nvmax=19, method='forward')
coef(reg.best, 12)