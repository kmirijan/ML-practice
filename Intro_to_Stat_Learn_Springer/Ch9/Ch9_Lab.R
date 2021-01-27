### Video 1

make.grid = function(x, n=75){
  grange = apply(x,2,range)
  x1=seq(from=grange[1,1], to=grange[2,1], length=n)
  x2=seq(from=grange[1,2], to=grange[2,2], length=n)
  expand.grid(X1=x1, X2=x2)
}

set.seed(10111)
x=matrix(rnorm(40), 20, 2)
x
y=rep(c(-1,1), c(10,10))
x[y==1,]=x[y==1,]+1
plot(x, col=y+3, pch=19)


library(e1071)
dat = data.frame(x,y=as.factor(y))
svmfit= svm(y~., data=dat, kernel='linear', cost=10, scale=F)
print(svmfit)
plot(svmfit, dat)

xgrid = make.grid(x)
xgrid
ygrid=predict(svmfit, xgrid)
plot(xgrid, col=c('red', 'blue')[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)


beta = drop(t(svmfit$coefs)%*%x[svmfit$index, ])
beta0 = svmfit$rho
plot(xgrid, col=c('red', 'blue')[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)

### Video 2

load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)

plot(x, col=y+1)
dat=data.frame(y=factor(y), x)
fit = svm(factor(y)~., data=dat, scale=F, kernel='radial', cost=5)

xgrid = expand.grid(X1=px1, X2=px2)
ygrid=predict(fit, xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20, cex=.2)
points(x, col=y+1, pch=19)

func=predict(fit, xgrid, decision.values = T)
func=attributes(func)$decision
contour(px1, px2, matrix(func, 69,99), level=0, add=T)
contour(px1, px2, matrix(func, 69,99), level=.5, add=T, col='blue', lwd=2)


### BOOK
### Suport Vector Classifier

set.seed=1
x=matrix(rnorm(40), ncol=2)
y=c(rep(-1, 10), rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x, col=3-y, pch=19)

dat=data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data=dat, kernel='linear', cost=10, scale=F)
plot(svmfit, dat)
svmfit$index # Index of support vectors
summary(svmfit)

svmfit = svm(y~., data=dat, kernel='linear', cost=.1, scale=F)
plot(svmfit, dat)
svmfit$index # Index of support vectors

set.seed(1)
?tune
costs = c(0.001, 0.01, 0.1, 1, 5, 10, 100)
tune.out = tune(svm, y~., data=dat, kernel='linear', ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1, 1), 20, rep=T)
xtest[ytest==1,] = xtest[ytest==1,] + 1             
testdat = data.frame(x=xtest, y = as.factor(ytest))
ypred = predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

svmfit = svm(y~., data=dat, kernel='linear', cost=5, scale=F) #Try different costs
ypred = predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)

### Support Vector Machine

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] +2
x[101:150, ] = x[101:150, ] -2
y=c(rep(1,150), rep(2,50))
dat= data.frame(x, y=as.factor(y))
plot(x, col=y)
train=sample(200, 100)
svmfit = svm(y~., data=dat[train, ], kernel='radial', gamma=1, cost=1)
plot(svmfit, dat[train, ])
summary(svmfit)

svmfit = svm(y~., data=dat[train, ], kernel='radial', gamma=1, cost=0.0001) #Try different costs
plot(svmfit, dat[train, ])

set.seed(1)
tune.out = tune(svm, y~., data=dat[train,], kernel='radial', 
                ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000) , gamma = c(0.5, 1, 2, 3, 4)))

summary(tune.out)

plot(tune.out$best.model, dat[train, ])

table(true=dat[-train, 'y'], pred=predict(tune.out$best.model, newdata=dat[-train, ]))

### ROC Curve: SKIP

### SVM with multiple classes
set.seed(1)
x=rbind(x, matrix(rnorm(100), ncol=2))
y = c(y, rep(0,50))
x[y==0, 2] = x[y==0, 2] + 2
dat = data.frame(x=x, y = as.factor(y))
plot(x, col=(y+1))
svmfit = svm(y~., data=dat, kernel='radial', gamma=1, cost=10)
plot(svmfit, dat) # This looks pretty bad

### Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)

table(Khan$ytrain)
table(Khan$ytest)

dat = data.frame(x=Khan$xtrain, y = as.factor(Khan$ytrain))
svmfit = svm(y~., data=dat, kernel='linear', cost=10)
summary(svmfit)
table(svmfit$fitted, dat$y) # Makes sense. Lots of predictors, Could easily overfit

dat.te = data.frame(x=Khan$xtest, y = as.factor(Khan$ytest))
predict.gene = predict(svmfit, newdata=dat.te)
table(predict.gene, dat.te$y) # Not bad on the test set actually
