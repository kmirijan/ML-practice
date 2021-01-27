### Principal Component Regression

install.packages("pls")
library(pls)

set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters, scale=TRUE, validation='CV') #Scale-> standardization, validation:CV->10 fold
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP") #Train everything

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=T, validation='CV')
validationplot(pcr.fit, val.type="MSEP") # Hold out, only use train, 5 PC
pcr.pred=predict(pcr.fit, x[test,], ncomp=5)
mean((pcr.pred-y.test)^2) # Comparable to RR and Lasso, but less interpretable
pcr.fit=pcr(y~x, scale=T, ncomp=5) # x and y from previous lab
summary(pcr.fit)

### Partial Least Squares
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters, scale=TRUE, validation='CV')
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred=predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters, scale=T, ncomp=2) # x and y from previous lab
summary(pls.fit)
