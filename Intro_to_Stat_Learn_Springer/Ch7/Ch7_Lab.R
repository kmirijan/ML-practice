### Polynomials and Step Functions
library(ISLR)
library(MASS)
attach(Wage)

fit = lm(wage~poly(age,4), data=Wage) # poly this way creates age poly values that are orthongonal to eachother. Basically Independent
summary(fit)
coef(summary(fit))

fit2 = lm(wage~poly(age,4, raw=T), data=Wage) # Model doesn't change much, but we see change in coefficients
summary(fit2)
coef(summary(fit2))

fit2a = lm(wage~age + I(age^2) + I(age^3) + I(age^4), data = Wage) # Exactly the same as fit 2
summary(fit2a)
coef(summary(fit2a))

fit2a = lm(wage~cbind(age, age^2, age^3, age^4), data = Wage) # Exactly the same as fit 2
summary(fit2a)
coef(summary(fit2a))

agelims = range(age) # Creates vector containing max and min values of age
age.grid = agelims[1]:agelims[2] # From low -> high sequence by 1s
preds = predict(fit, newdata=list(age=age.grid), se=T) # This predicts while maintaining standard errors
se.bands=cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit) # Standard error band

# Polynomial Regression
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, 1), oma=c(0, 0, 4, 0)) # mar and oma hadle margins
plot(age, wage, xlim=agelims, cex=.5, col='darkgrey')
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col='blue')
matlines(age.grid, se.bands, lwd=1, col='blue', lty=3)

preds2=predict(fit2, newdata=list(age=age.grid), se=T)
max(abs(preds$fit-preds2$fit)) # Virtually no difference in the fit 

fit.1=lm(wage~age, data=Wage)
fit.2=lm(wage~poly(age,2), data=Wage)
fit.3=lm(wage~poly(age,3), data=Wage)
fit.4=lm(wage~poly(age,4), data=Wage)
fit.5=lm(wage~poly(age,5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5) # Analysis of variance tells us that 3 degrees is enough

coef(summary(fit.5)) # Essentially does the same as anova since polynomials are orthogonal in this fit

fit.1=lm(wage~education+age, data=Wage)
fit.2=lm(wage~education+poly(age,2), data=Wage)
fit.3=lm(wage~education+poly(age,3), data=Wage)
anova(fit.1, fit.2, fit.3) # Anova works when there are extra predictors

# Logistic Regression
fit=glm(I(wage>250)~poly(age,4), data=Wage, family='binomial')
pred=predict(fit, newdata=list(age=age.grid), se=T)
pfit = exp(pred$fit)/ (1 + exp(pred$fit)) # We have to do this transformations since before we got our predictions in logit form
se.bands.logit = cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
se.bands = exp(se.bands.logit)/ (1 + exp(se.bands.logit))

plot(age, I(wage>250), xlim=agelims, type='n', ylim=c(0,.2))
points(jitter(age), I((wage>250)/5), cex=.5, pch='|', col='darkgrey') # Jitter moves the points a bit so they don't cover eachother
lines(age.grid, pfit, lwd=2, col='blue')
matlines(age.grid, se.bands, lwd=1, col='blue', lty=10)

# Step function
table(cut(age,4)) # Automatically picks cut points
fit=lm(wage~cut(age,4), data=Wage)
summary(fit)

### Splines
library(splines)

# Regression Spline
fit=lm(wage~bs(age, knots=c(25,40,60)), data=Wage) # fits wage to basis functions of age given knots
pred=predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col='grey')
lines(age.grid, pred$fit, lwd=2)
se.bands=cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
matlines(age.grid, se.bands, lty='dashed')

dim(bs(age, knots=c(25,40,60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots") #When distribution is uniform, the knots are chosen at 33.75, 42, and 51

# Natural Spline
fit2=lm(wage~ns(age, df=4), data=Wage)
pred=predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred$fit, lwd=2, col='red') # Linear continuation at ends. Similar fit, less degrees of freedom

# Smoothing Spline
plot(age, wage, xlim=agelims, cex=.5, col='darkgray')
title('Smoothing Splines')
?smooth.spline
fit=smooth.spline(age, wage, df=16) # Chose your degrees of freedom
fit2 = smooth.spline(age, wage, cv=T) # Use CV to find best df
fit2$df
lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c('red','blue'), lty=1, lwd=2, cex=.8)

# Local Regression (Loess)
plot(age, wage, xlim=agelims, cex=.5, col='darkgray')
title('Local Regression')
fit=loess(wage~age, span=.2, data=Wage)
fit2=loess(wage~age, span=.5, data=Wage)
pred1 = predict(fit, data.frame(age=age.grid))
pred2 = predict(fit2, data.frame(age=age.grid))
lines(age.grid, pred1, lwd=2, col='red')
lines(age.grid, pred2, lwd=2, col='blue')

### GAMs
gam1=lm(wage~ns(year,4) + ns(age,5) + education, data=Wage)

install.packages('gam')
library(gam)

gam.m3=gam(wage~s(year,4)+s(age,5) +education, data=Wage) # s() indicates smoothing spline
par(mfrow=c(1,3))
plot(gam.m3, se=T, col='blue') # Year doesn't really look linear or like the book
plot.Gam(gam1, se=T, col='red')

gam.m1 = gam(wage~s(age,5)+education, data = Wage)
gam.m2 = gam(wage~year+s(age,5)+education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test=F)

summary(gam.m3)
pred=predict(gam.m2, newdata=Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage)
plot.Gam(gam.lo, se=T, col='green')

gam.lo.i=gam(wage~lo(year,age,span=.5)+education, data=Wage)

install.packages("akima")
library(akima)
plot(gam.lo.i)
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education, data=Wage, family='binomial')
par(mfrow=c(1,3)) 
plot(gam.lr, se=T, col='green')
table(education, I(wage>250))

gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education, data=Wage, subset=(education!='1. < HS Grad'), family='binomial')
plot(gam.lr.s, se=T, col='green')
