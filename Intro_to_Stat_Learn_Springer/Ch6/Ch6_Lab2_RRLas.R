### Ridge Regression

library(glmnet)
x=model.matrix(Salary~., data=Hitters)[, -1] # Transform predcitors into a matrix for glmnet
y=Hitters$Salary

grid=10^seq(10, -2, length=100) # Create a grid of lambda values to run through
grid
ridge.mod=glmnet(x,y,alpha=0, lambda = grid) # You can auto grid by not including lambda. alpha=0 mean RR
dim(coef(ridge.mod)) #20x10. (length_predictors + 1 for intercept) * number of lambda values
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod, s=50, type='coefficients')[1:20,] # Obtain RR coef for new value of labmda 50

# Split train and test
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0, lambda = grid, thresh=1e-12)
?glmnet
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2) # MSE with lambda=4
mean((mean(y[train])-y.test)^2) #MSE super high lambda

ridge.pred=predict(ridge.mod, s=0, newx=x[test,], exact=T, x=x[train,], y=y[train]) #Error, I think I did the x and y right
?predict.glmnet
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact = T, x=x[train,], y=y[train], type="coefficients")[1:20,] # Not sure if I did this right

set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2) # MSE with lambda=4
out=glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]

### Lasso

lasso.mod=glmnet(x=x[train,], y=y[train], lambda=grid)
plot(lasso.mod)

#Cross Validation
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train])
names(cv.out)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2) # MSE with lambda=4

out=glmnet(x,y, lambda=grid)
predict(out, type='coefficients', s=bestlam)[1:20,]
