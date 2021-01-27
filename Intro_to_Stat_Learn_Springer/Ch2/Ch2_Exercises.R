### Q8
college = read.csv("data/College.csv")
fix(college)
rownames(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
attach(college)
plot(Outstate, Private)
Private=as.factor(Private)
plot(Private, Outstate)
Elite = rep("No", nrow(college))
Elite[Top10perc > 50] = "Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)
summary(college)
Elite = as.factor(Elite)
plot(Elite, Outstate)
par(mfrow=c(2,2))
hist(Outstate)
hist(mpg)
hist(mpg, breaks=5)
hist(Expend, breaks = 25)

### Q9

Auto = read.csv("data/Auto.csv", header=T, na.strings="?")
Auto= na.omit(Auto)
summary(Auto[, -c(4, 9)])
sapply(Auto[, -c(4, 9)], sd)
sub_auto <- Auto[-c(10:85), -c(4,9)]
sapply(sub_auto, range)
sapply(sub_auto, sd)
sapply(sub_auto, mean)
pairs(Auto)

### Q10
library(MASS)
Boston
?Boston
pairs(Boston)
sapply(Boston, range)
summary(Boston)
Boston$chas = as.factor(Boston$chas)
summary(Boston)
nrow(Boston[Boston$crim >20,])
Boston[min(Boston$medv),]
dim(Boston[Boston$rm > 8, ])
dim(Boston[Boston$rm > 7, ])
summary(Boston[Boston$rm > 7, ])
summary(Boston[Boston$rm > 8, ])
