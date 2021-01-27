### Q10
library(ISLR)
library(MASS)
dim(Weekly)
names(Weekly)
summary(Weekly)
cor(Weekly[,-9])
cor(Weekly[,-9]) > .6
pairs(Weekly, col= Weekly$Direction)
attach(Weekly)
glm.10 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = 'binomial', data=Weekly)
summary(glm.10) # Thinks Lag2 is a bit significant
glm.prob=predict(glm.10, type='response')
glm.pred=rep("Down", 1089)
glm.pred[glm.prob>.5]="Up"
table(glm.pred, Direction)
mean(glm.pred==Direction) #Correct 56% of the time. Not very good at predicting the downs

train=Year<2009
Weekly.train = Weekly[train,]
Weekly.test = Weekly[!train,]
glm.10 = glm(Direction~Lag2, family='binomial', data=Weekly, subset=train)
summary(glm.10)
glm.prob = predict(glm.10, Weekly.test, type='response')
glm.pred=rep("Down", 104)
glm.pred[glm.prob>.5]="Up"
table(glm.pred, Direction[!train]) # 62% accuracy, but does very poorly on Downs
mean(glm.pred == Direction[!train]) 

lda.10 = lda(Direction~Lag2, data=Weekly, subset=train)
lda.10
plot(lda.10)
lda.pred=predict(lda.10, Weekly.test)
table(lda.pred$class, Direction[!train])
mean(lda.pred$class==Direction[!train]) #Exact same results as logistic regression

qda.10 = qda(Direction~Lag2, data=Weekly, subset=train)
qda.10
qda.pred=predict(qda.10, Weekly.test)
table(qda.pred$class, Direction[!train])
mean(qda.pred$class==Direction[!train]) # 58% accuracy, only predicted Up

library(class)
train.x.10 = as.matrix(Lag2[train])
test.x.10 = as.matrix(Lag2[!train])
train.Direction = Direction[train]
test.Direction = Direction[!train]
set.seed(1)
knn.pred = knn(train.x.10, test.x.10, train.Direction, k=1)
table(knn.pred, test.Direction)
mean(knn.pred==test.Direction) # 50% correct

# Try my own thing
glm.exp = glm(Direction~Lag2+Lag4+Lag1:Volume, family='binomial', data=Weekly, subset=train)
summary(glm.exp)
glm.prob = predict(glm.exp, Weekly.test, type='response')
glm.pred=rep("Down", 104)
glm.pred[glm.prob>.5]="Up"
table(glm.pred, Direction[!train]) # 55% accuracy, still does very poorly on Downs
mean(glm.pred == Direction[!train])

# Now without Lag4
glm.exp = glm(Direction~Lag2+Lag1:Volume, family='binomial', data=Weekly, subset=train)
summary(glm.exp)
glm.prob = predict(glm.exp, Weekly.test, type='response')
glm.pred=rep("Down", 104)
glm.pred[glm.prob>.5]="Up"
table(glm.pred, Direction[!train]) # 57% accuracy, but didn't get worse on downs, so it is better unilaterally
mean(glm.pred == Direction[!train])

lda.exp = lda(Direction~Lag2+Lag1:Volume, data=Weekly, subset=train)
lda.exp
plot(lda.exp)
lda.pred=predict(lda.exp, Weekly.test)
table(lda.pred$class, Direction[!train])
mean(lda.pred$class==Direction[!train]) # Almost Exact same results as logistic regression, does worse on downs

qda.exp = qda(Direction~Lag2+Lag1:Volume, data=Weekly, subset=train)
qda.exp
qda.pred=predict(qda.exp, Weekly.test)
table(qda.pred$class, Direction[!train])
mean(qda.pred$class==Direction[!train]) # 48% accuracy, Downs got worse

train.x.10 = as.matrix(Lag2[train])
test.x.10 = as.matrix(Lag2[!train])
train.Direction = Direction[train]
test.Direction = Direction[!train]
set.seed(1)
knn.pred = knn(train.x.10, test.x.10, train.Direction, k=3) # Change k to 3
table(knn.pred, test.Direction)
mean(knn.pred==test.Direction) # Better Accuracy, but got worse on down

train.x.10 = cbind(Lag2, Lag1*Volume)[train,]
test.x.10 = cbind(Lag2, Lag1*Volume)[!train,]
train.Direction = Direction[train]
test.Direction = Direction[!train]
set.seed(1)
knn.pred = knn(train.x.10, test.x.10, train.Direction, k=1)
table(knn.pred, test.Direction)
mean(knn.pred==test.Direction) # 56% did a lot better on down, struggled more with ups

train.x.10 = cbind(Lag2, Lag1*Volume)[train,]
test.x.10 = cbind(Lag2, Lag1*Volume)[!train,]
train.Direction = Direction[train]
test.Direction = Direction[!train]
set.seed(1)
knn.pred = knn(train.x.10, test.x.10, train.Direction, k=3) # Change to 3 again
table(knn.pred, test.Direction)
mean(knn.pred==test.Direction) # 50%,  worse overall

### Q11

dim(Auto)
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg>median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

pairs(Auto)
cor(Auto[,-9]) > 0.5
cor(Auto[,-9]) < -0.5
boxplot(cylinders~mpg01, data=Auto)

#mpg01 seems most correlated with origin, cylinders, displacement, horsepower, and weight
#Split into train and test
train = (year%%2 == 0) # Train is 1 if year mod 2 == 0
Auto.train = Auto[train,]
Auto.test = Auto[!train,]
mpg01.test = mpg01[!train]

#LDA 
lda.11 = lda(mpg01~cylinders+displacement+horsepower+weight, data=Auto, subset=train)
lda.11
plot(lda.11) # Not normally distributed
lda.pred=predict(lda.11, Auto.test)
table(lda.pred$class, mpg01.test)
mean(lda.pred$class==mpg01.test) #87% accurate. Does well on both 1 and 0

#logistic regression
glm.11 = glm(mpg01~cylinders+displacement+horsepower+weight, family='binomial', data=Auto, subset=train)
summary(glm.11) #Says only horespower and weight are significant
glm.prob = predict(glm.11, Auto.test, type='response')
glm.pred=rep(0, length(glm.prob))
glm.pred[glm.prob>.5]=1
table(glm.pred, mpg01.test) 
mean(glm.pred == mpg01.test) # 88% accurate, did a bit better on 0s than 1s

#QDA
qda.11 = lda(mpg01~cylinders+displacement+horsepower+weight, data=Auto, subset=train)
qda.11
qda.pred=predict(qda.11, Auto.test)
table(qda.pred$class, mpg01.test)
mean(qda.pred$class==mpg01.test) #87% accurate. Does well on both 1 and 0

#KNN
train.x.11 = cbind(cylinders, displacement, horsepower, weight)[train,]
test.x.11 = cbind(cylinders, displacement, horsepower, weight)[!train,]
train.mpg01 = mpg01[train]
test.mpg01 = mpg01[!train]
set.seed(1)
knn.pred = knn(train.x.11, test.x.11, train.mpg01, k=1)
table(knn.pred, test.mpg01)
mean(knn.pred==test.mpg01) # 85% on K=1

knn.pred = knn(train.x.11, test.x.11, train.mpg01, k=2)
table(knn.pred, test.mpg01)
mean(knn.pred==test.mpg01) # 85% on K=2, but better 1s, worse 0s

knn.pred = knn(train.x.11, test.x.11, train.mpg01, k=3)
table(knn.pred, test.mpg01)
mean(knn.pred==test.mpg01) # 86% on K=3, but better 1s, better 0s

### Q12
Power = function(){
  print(2^3)
  
}
Power()

Power2 = function(x,a){
  print(x^a)
  
}
Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)

Power3=function(x,a){
  result = x^a
  return(result)
}
x = 1:10
plot(x, Power3(x, 2), log='xy', xlab='Log of x', ylab='Log of y')

PlotPower=function(x,a){
  plot(x, Power3(x, a), main=paste('x vs y power', a))
}
PlotPower(x, 3)


### Q13
Boston

# Make median and split data
crim01 = rep('Low', length(crim))
crim01[crim > median(crim)] = 'High'
Boston = data.frame(Boston, crim01)
Boston
train = 1:(length(crim) / 2)
test = (length(crim) / 2 + 1):length(crim)
Boston.train = Boston[train,]
Boston.test = Boston[test,]
crim01.test = crim01[test]

# Look for significant predictors in glm
glm.13 = glm(crim01~.-crim01.1-crim, family='binomial', data=Boston, subset=train)
summary(glm.13)
glm.13 = glm(crim01~zn+nox+rm+dis+rad+ptratio+black+medv, family='binomial', data=Boston, subset=train)
summary(glm.13)
glm.13 = glm(crim01~zn+nox+rm+dis+rad+ptratio+medv, family='binomial', data=Boston, subset=train)
summary(glm.13)

#predict with glm
glm.prob = predict(glm.13, Boston.test, type='response')
glm.pred=rep('Low', length(glm.prob))
glm.pred[glm.prob>.5]='High'
table(glm.pred, crim01.test) 
mean(glm.pred == crim01.test) # 80% accuracy but only gets 70% of the Lows

#Explore Data
cor(Boston[, c(2,5,6,8,9,11,14)]) > 0.5
cor(Boston[, c(2,5,6,8,9,11,14)]) < -0.5
par(mfrow=c(2,3))
plot(zn, dis)
plot(nox, rad)
plot(rm, medv)
plot(zn, nox)
plot(nox, dis) # Log interaction
plot(ptratio, medv) # Multiplicative Interaction
par(mfrow=c(1,1))
plot(log(nox), log(dis))

#Try new 
glm.13 = glm(crim01~zn+nox+rm+dis+rad+ptratio+rm:medv, family='binomial', data=Boston, subset=train)
summary(glm.13)
glm.pred=rep('Low', length(glm.prob))
glm.pred[glm.prob>.5]='High'
table(glm.pred, crim01.test) # Exact same results
mean(glm.pred == crim01.test) # 

#LDA
lda.13 = lda(crim01~zn+nox+rm+dis+rad+ptratio+rm:medv, data=Boston, subset=train)
lda.13
plot(lda.13) # Not normally distributed
lda.pred=predict(lda.13, Boston.test)
table(lda.pred$class, crim01.test)
mean(lda.pred$class!=crim01.test) #88% accurate. Does much better than logistic regression

#KNN
train.X = cbind(zn, nox, rm, dis, rad, ptratio, rm*medv)[train, ]
test.X = cbind(zn, nox, rm, dis, rad, ptratio, rm*medv)[test, ]
train.crim01 = crim01[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.crim01, k = 1)
table(pred.knn, crim01.test)
mean(pred.knn == crim01.test) # About 73% accurate on K=1. Drops the ball on Highs

pred.knn <- knn(train.X, test.X, train.crim01, k = 4)
table(pred.knn, crim01.test)
mean(pred.knn == crim01.test) # K=4 still bad

pred.knn <- knn(train.X, test.X, train.crim01, k = 10)
table(pred.knn, crim01.test)
mean(pred.knn == crim01.test) # K=10 still bad

# I'd go with LDA here
