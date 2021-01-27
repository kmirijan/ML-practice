### Q8

library(ISLR)
auto=read.csv("data/Auto.csv", na.strings='?')
auto=na.omit(auto)
attach(auto)
fit1 = lm(mpg~horsepower, auto)
summary(fit1)
predict(fit1, data.frame(horsepower=c(98)), interval='confidence')
predict(fit1, data.frame(horsepower=c(98)), interval='prediction')
confint(fit1)
par(mfrow=c(1,1))
plot(horsepower, mpg)
abline(fit1, col=2)
par(mfrow=c(2,2))
plot(fit1)

### Q9

pairs(auto)
?cor
names(auto)
cor(auto[1:8])
fit2=lm(mpg~.-name, auto)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
fit3=lm(mpg~.+displacement:cylinders + displacement:weight + weight:cylinders + horsepower:displacement, auto[1:8])
summary(fit3)
plot(horsepower, mpg)
plot(weight, mpg)
plot(year, mpg)
plot(displacement*horsepower, mpg)
plot(log(horsepower), mpg); plot(sqrt(horsepower), mpg); plot((horsepower)^2, mpg)
plot(log(displacement*horsepower), mpg); plot(sqrt(displacement*horsepower), mpg); plot((displacement*horsepower)^2, mpg)
fit3=lm(mpg~.+log(horsepower) + log(displacement*horsepower) - cylinders, auto[1:8])
summary(fit3)
anova(fit2, fit3)

### Q10

carseats = data(Carseats)
fix(Carseats)
attach(Carseats)

fit4 = lm(Sales~Price+Urban+US, Carseats)
summary(fit4)
fit4=lm(Sales~Price+US, Carseats)
summary(fit4)
confint(fit4)
plot(fit4)

###Q11

set.seed(1) #Set random seed
x=rnorm(100) # create 100 values normalized
y = 2*x +rnorm(100) #create another 100 random normalized values but also based on an equattion that involves x
fit5 = lm(y~x+0) #lm without intercept
summary(fit5)
fit6=lm(x~y+0)
summary(fit6)

# I don't really understand this part. Not only in terms of how the math works, but why it is significant

n <- length(x)
t <- sqrt(n - 1)*(x %*% y)/sqrt(sum(x^2) * sum(y^2) - (x %*% y)^2)
as.numeric(t)

fit7 = lm(y~x)
summary(fit7)
fit8 = lm(x~y)
summary(fit8)

### Q12
set.seed(1) #Set random seed
x=rnorm(100) # create 100 values normalized
y = 2*x +rnorm(100) #create another 100 random normalized values but also based on an equattion that involves x
fit.Y = lm(y~x+0) #lm without intercept
summary(fit.Y)
fit.X=lm(x~y+0)
summary(fit.X)

x = 1:100
y = 100:1
fit.Y = lm(y~x+0) #lm without intercept
summary(fit.Y)
fit.X=lm(x~y+0)
summary(fit.X)

### Q13: How variance can effect your system
x = rnorm(100)
eps = rnorm(100, sd=sqrt(.25))
y = -1 + .5*x + eps
length(y)
plot(x,y)
fit.13.1 = lm(y~x)
summary(fit.13.1)
abline(-1, 0.5, col='3')
abline(fit.13.1, col='2')
legend("topleft", c("Least square", "Regression"), col = c("3", "2"), lty = c(1, 1))

eps = rnorm(100, sd=sqrt(.1))
y = -1 + .5*x + eps
length(y)
plot(x,y)
fit.13.2 = lm(y~x)
summary(fit.13.2)
abline(-1, 0.5, col='3')
abline(fit.13.2, col='2')
legend("topleft", c("Least square", "Regression"), col = c("3", "2"), lty = c(1, 1))

eps = rnorm(100, sd=sqrt(.5))
y = -1 + .5*x + eps
length(y)
plot(x,y)
fit.13.3 = lm(y~x)
summary(fit.13.3)
abline(-1, 0.5, col='3')
abline(fit.13.3, col='2')
legend("topleft", c("Least square", "Regression"), col = c("3", "2"), lty = c(1, 1))

confint(fit.13.1)
confint(fit.13.2)
confint(fit.13.3)

### Q14: How collinearity effects your system
set.seed(1)
x1=runif(100)
x2=.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)
plot(x1, x2)
cor(x1, x2)

fit.14.1 = lm(y~x1+x2)
summary(fit.14.1)

fit.14.2 = lm(y~x1)
summary(fit.14.2)

fit.14.3 = lm(y~x2)
summary(fit.14.3)

x1=c(x1, .1)
x2=c(x2, .8)
y=c(y,6)

fit.14.4 = lm(y~x1+x2)
summary(fit.14.4)

fit.14.6 = lm(y~x1)
summary(fit.14.6)

fit.14.7 = lm(y~x2)
summary(fit.14.7)

par(mfrow=c(2,2))

plot(fit.14.4)
plot(fit.14.6)
plot(fit.14.7)

### Q15
names(Boston)
fit.zn = lm(crim~zn, Boston)
fit.indus = lm(crim~indus, Boston)
fit.chas = lm(crim~chas, Boston)
fit.nox = lm(crim~nox, Boston)
fit.rm = lm(crim~rm, Boston)
fit.age = lm(crim~age, Boston)
fit.dis = lm(crim~dis, Boston)
fit.rad = lm(crim~rad, Boston)
fit.tax = lm(crim~tax, Boston)
fit.ptratio = lm(crim~ptratio, Boston)
fit.black = lm(crim~black, Boston)
fit.lstat = lm(crim~lstat, Boston)
fit.medv = lm(crim~medv, Boston)

summary(fit.zn)
plot(zn, crim)
abline(fit.zn, col=2)

summary(fit.indus)
plot(indus, crim)
abline(fit.indus, col=2)

summary(fit.chas)
plot(chas, crim)
abline(fit.chas, col=2)

summary(fit.nox)
plot(nox, crim)
abline(fit.nox, col=2)

summary(fit.rm)
plot(rm, crim)
abline(fit.rm, col=2)

summary(fit.age)
plot(age, crim)
abline(fit.age, col=2)

summary(fit.dis)
plot(dis, crim)
abline(fit.dis, col=2)

summary(fit.rad)
plot(rad, crim)
abline(fit.rad, col=2)

summary(fit.tax)
plot(tax, crim)
abline(fit.tax, col=2)

summary(fit.ptratio)
plot(ptratio, crim)
abline(fit.ptratio, col=2)

summary(fit.black)
plot(black, crim)
abline(fit.black, col=2)

summary(fit.lstat)
plot(lstat, crim)
abline(fit.lstat, col=2)

summary(fit.medv)
plot(medv, crim)
abline(fit.medv, col=2)

fit.all = lm(crim~., Boston)
summary(fit.all)

simple.reg <- vector("numeric",0)
simple.reg <- c(simple.reg, fit.zn$coefficient[2])
simple.reg <- c(simple.reg, fit.indus$coefficient[2])
simple.reg <- c(simple.reg, fit.chas$coefficient[2])
simple.reg <- c(simple.reg, fit.nox$coefficient[2])
simple.reg <- c(simple.reg, fit.rm$coefficient[2])
simple.reg <- c(simple.reg, fit.age$coefficient[2])
simple.reg <- c(simple.reg, fit.dis$coefficient[2])
simple.reg <- c(simple.reg, fit.rad$coefficient[2])
simple.reg <- c(simple.reg, fit.tax$coefficient[2])
simple.reg <- c(simple.reg, fit.ptratio$coefficient[2])
simple.reg <- c(simple.reg, fit.black$coefficient[2])
simple.reg <- c(simple.reg, fit.lstat$coefficient[2])
simple.reg <- c(simple.reg, fit.medv$coefficient[2])
mult.reg <- vector("numeric", 0)
mult.reg <- c(mult.reg, fit.all$coefficients)
mult.reg <- mult.reg[-1]
plot(simple.reg, mult.reg, col = "red")

names(Boston)

fit.zn2 <- lm(crim ~ poly(zn, 3))
fit.indus2 <- lm(crim ~ poly(indus, 3))
# fit.chas2 <- lm(crim ~ poly(chas, 3)) Can't do this
fit.nox2 <- lm(crim ~ poly(nox, 3))
fit.rm2 <- lm(crim ~ poly(rm, 3))
fit.age2 <- lm(crim ~ poly(age, 3))
fit.dis2 <- lm(crim ~ poly(dis, 3))
fit.rad2 <- lm(crim ~ poly(rad, 3))
fit.tax2 <- lm(crim ~ poly(tax, 3))
fit.ptratio2 <- lm(crim ~ poly(ptratio, 3))
fit.black2 <- lm(crim ~ poly(black, 3))
fit.lstat2 <- lm(crim ~ poly(lstat, 3))
fit.medv2 <- lm(crim ~ poly(medv, 3))

summary(fit.zn2) # NO
summary(fit.indus2) # Yes
summary(fit.nox2) # Yes
summary(fit.rm2) # NO
summary(fit.age2) # Yes
summary(fit.dis2) # Yes
summary(fit.rad2) # NO
summary(fit.tax2) # NO
summary(fit.ptratio2) # Yes
summary(fit.black2) # NO
summary(fit.lstat2) # NO
summary(fit.medv2) # Yes
