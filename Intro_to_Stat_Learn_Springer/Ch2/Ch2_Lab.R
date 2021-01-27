print("hello")
x=c(2,7,5)
x
y=seq(from=4, length=3, by=3)
y
x+y
x[2]
y[2]
x[2:3]
x[-2]
x[c(1,2)]
x[-c(1,2)]
z=matrix(seq(1,12),4,3)
z
z[3:4, 2:3]
z[,2:3]
z[,1]
z[,1, drop=FALSE]
dim(z)
ls()
rm(y)
ls()
x=runif(50)
y=rnorm(50)
plot(x,y)
x
y
plot(x,y,xlab = "Random Uniform", ylab = "Random Normal", pch="x", col="blue")
plot(x,y,xlab = "Random Uniform", ylab = "Random Normal", col="blue")
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))
setwd("C:/Users/Khachatur/ML-practice/Intro_to_Stat_Learn_Springer/")
Auto=read.csv("data/Auto.csv")
names(Auto)
dim(Auto)
summary(Auto)
plot(Auto$cylinders, Auto$mpg)
plot(Auto$cyl, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)

