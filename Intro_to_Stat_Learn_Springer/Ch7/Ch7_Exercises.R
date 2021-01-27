set.seed(1)
library(leaps)
library(boot)
library(MASS)
library(ISLR)

### Q6
par(mfrow=c(1,1))
all.errors = rep(NA, 10)
for (i in 1:10){
  glm.fit = glm(wage~poly(age, i), data=Wage)
  all.errors[i] = cv.glm(glm.fit, data=Wage, K=10)$delta[2] # Save adjusted prediction error
}
plot(x=1:10, y=all.errors, xlab='Dfs', ylab='CV Error', type='l', lwd=2,  pch=20, ylim=c(1590, 1700))
min.pt = min(all.errors)
sd.pts = sd(all.errors)
abline(h=min.pt + .2*sd.pts, col='red', lty='dashed')
abline(h=min.pt - .2*sd.pts, col='blue', lty='dashed') # Looks like 3 or 4 d would be good

fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10) # Degree 3 is as high as it should go

plot(wage~age, data=Wage, col='darkgrey')
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit = lm(wage~poly(age,3), data=Wage)
lm.pred=predict(lm.fit, newdata=list(age=age.grid), se=T)
lines(age.grid, lm.pred$fit, lwd=2, col='red')

all.errors = rep(NA, 10)
for(i in 2:10){
  Wage$age.cut = cut(Wage$age, i) # Don't know why. Can't just use cut() in lm.
  lm.fit=glm(wage~age.cut, data=Wage) # Maybe since it isn't originally in data Wage?
  all.errors[i] = cv.glm(lm.fit, data=Wage, K=10)$delta[2]
}
plot(2:10, all.errors[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2) # looks like 8 is best

plot(wage~age, data=Wage, col='darkgrey')
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit=glm(wage~cut(age,8), data=Wage)
lm.pred=predict(lm.fit, newdata=list(age=age.grid), se=T)
lines(age.grid, lm.pred$fit, lwd=2, col='red')

### Q7
attach(Wage)
summary(Wage$maritl)
summary(Wage$jobclass)

par(mfrow=c(1,2))
plot(maritl, wage)
plot(jobclass, wage)

### Q8
Auto
pairs(Auto)

errors = rep(NA, 10)
fits = list()
for(i in 1:10){
  fits[[i]] = lm(mpg~poly(horsepower, i), data=Auto)
}
anova(fits[[1]],fits[[2]],fits[[3]],fits[[4]],fits[[5]],fits[[6]],fits[[7]],fits[[8]],fits[[9]],fits[[10]]) # 5 seems good

library(glmnet)
library(boot)

cv.errs = rep(NA, 10)
for(c in 2:10){
  Auto$hp.cut = cut(Auto$horsepower, c)
  fit = glm(mpg~hp.cut, data=Auto)
  cv.errs[c] = cv.glm(fit, data=Auto, K=10)$delta[2]
}
which.min(cv.errs)
cv.errs

library(gam)
fit = gam(mpg~ns(displacement, 4) + ns(horsepower, 4), data=Auto)
summary(fit)

### Q9
attach(Boston)
lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)
plot(nox ~ dis, data=Boston, col='darkgrey')
dislims = range(Boston$dis)
dis.grid = seq(from=dislims[1], to=dislims[2], by=.1)
lm.pred = predict(lm.fit, newdata = list(dis=dis.grid))
lines(dis.grid, lm.pred, col='blue', lwd=2)

all.rss = rep(NA, 10)
for(i in 1:10){
  lm.fit = lm(nox ~ poly(dis, i), data = Boston)
  all.rss[i] = sum(lm.fit$residuals^2)
}
all.rss

all.deltas = rep(NA, 10)
for(i in 1:10){
  glm.fit=glm(nox ~ poly(dis, i), data = Boston)
  all.deltas[i] = cv.glm(Boston, glm.fit, K=10)$delta[2]
}
plot(1:10, all.deltas, pch=20, type='b') # Looks like 3 or 4 is good here

?bs
bs.fit = lm(nox~bs(dis, df=4), data=Boston) # Auto chose knots
summary(bs.fit)
attr(bs(dis, df=4), "knots")
bs.pred = predict(bs.fit, newdata=list(dis=dis.grid))
plot(nox~dis, data=Boston, col='darkgrey')
lines(dis.grid, bs.pred)

all.rss = rep(NA, 20)
for(i in 3:20){
  bs.fit = lm(nox~bs(dis, df=i), data=Boston)
  all.rss[i] = sum(bs.fit$residuals^2)
}
all.rss

all.deltas = rep(NA, 20)
for(i in 1:20){
  glm.fit=glm(nox ~ bs(dis, df=i), data = Boston)
  all.deltas[i] = cv.glm(Boston, glm.fit, K=10)$delta[2]
}
all.deltas
plot(1:20, all.deltas, pch=20, type='b') # 11 looks the best

### Q10

attach(College)
set.seed(1)
train = sample(nrow(College), nrow(College)/2)
College.train = College[train,]
College.test = College[-train,]
length(names(College))
?regsubsets
regfit.fwd = regsubsets(Outstate~., data=College, nvmax=length(names(College))-1, method='forward')
reg.summary = summary(regfit.fwd)
names(reg.summary)
par(mfrow = c(1, 3))
plot(reg.summary$adjr2, ylab='Adjusted R2', xlab='Num Vars', pch=19, type='b' )
plot(reg.summary$cp, ylab='Cp', xlab='Num Vars', pch=19, type='b' )
plot(reg.summary$bic, ylab='BIC', xlab='Num Vars', pch=19, type='b' )
which.max(reg.summary$adjr2)
reg.summary$adjr2
which.min(reg.summary$cp)
which.min(reg.summary$bic)

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

library(gam)
?summary
summary(College)
gam.fit=gam(Outstate~Private + s(Room.Board, df=3) + s(PhD, df=3) + s(perc.alumni, df=3) +
            s(Expend, df=3) + s(Grad.Rate, df=3), data=College)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")

gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err

gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss # R2 test of .779

summary(gam.fit) # If I use ns, I won't get non-parameric anova. Also expend and grad rate

### Q11
set.seed(1)
x1 = rnorm(100)
x2 = rnorm(100)
y = rnorm(100)
beta1 = 2
a = y -beta1*x1
beta2 = lm(a~x2)$coef[2]
beta2
y = 1 + x1 + x1^2
y
lm(y~x1)
