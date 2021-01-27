library(MASS)
library(ISLR)

### Simple Linear Regression

fix(Boston) # View in spreadsheet
names(Boston) # See column names
lm.fit=lm(medv~lstat, data=Boston) # Create linear model lstat->medv
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit
summary(lm.fit) # Get coef info, SE, t-value, p-value, RSE, R^2, F-stat
names(lm.fit) # See other potential info stored in fit
coef(lm.fit) # Get coef only
confint(lm.fit) # Confidence Interval
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence") #Predicton with confidence intervals
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction") #Predicton with prediction intervals?

plot(lstat, medv)
abline(lm.fit, col=2)

# Fancy Plotting
abline(lm.fit, lwd=3) # lwd -> line width thickness
abline(lm.fit, lwd=3, col='red')
plot(lstat, medv, col=2)
plot(lstat, medv, pch=20) # pch is point characteristics
plot(lstat, medv, pch='+')
plot(1:20, 1:20, pch=1:20) # pch is point characteristics

par(mfrow=c(2,2)) # Split plot screen
plot(predict(lm.fit), residuals(lm.fit)) #Residuals plot
plot(predict(lm.fit), rstudent(lm.fit)) # Studentized residuals plot
plot(hatvalues(lm.fit), rstudent(lm.fit)) # Leverage Statistice
which.max(hatvalues(lm.fit))

### Multiple Linear Regression

lm.fit = lm(medv~lstat+age, Boston) # lstat + age
summary(lm.fit)
lm.fit = lm(medv~., Boston) # ~. means all other variables
summary(lm.fit)

?summary.lm
summary(lm.fit)$r.sq #R^2
summary(lm.fit)$sigma #RSE

#install.packages("car")
library('car')
vif(lm.fit) # Get VIFs
lm.fit1 = lm(medv~.-age, Boston) # Everything except age
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age) # Update

### Interaction Terms
fit.int = lm(medv~lstat*age, Boston) # Interaction can be * or :
summary(fit.int)

### Non-linear Transformations
lm.fit2 = lm(medv~lstat+I(lstat^2)) # Use I() so I can use ^ as a square
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2) # anova() to see comparison of linear vs poly fits. Look for high F-stat and low p-value
par(mfrow=c(2,2))
plot(lm.fit2) # Look at residuals. Less pattern
plot(lm.fit)
lm.fit5= lm(medv~poly(lstat, 5)) # Go all the way up to ^5
summary(lm.fit5)
lm.fit4= lm(medv~log(rm), Boston) # Log transform
summary(lm.fit4)

### Qualitative Predictor
fix(Carseats)
names(Carseats)
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats) 
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc) # Looks at encoding for the ShelveLoc Qualitative variable
