install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8, 'No', 'Yes')
Carseats = data.frame(Carseats, High)

### Classification Trees

tree.carseats = tree(High~. - Sales, data=Carseats) # Make tree
summary(tree.carseats) # You can see # of term nodes, residual mean deviance, nad training error rate
plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats

set.seed(2) # Make train and test and predict on test 
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree.carseats = tree(High~. - Sales, data=Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
(104+50) / 200

set.seed(3) # Cross validate for pruning size of tree
?cv.tree
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b') # 8 is the best size
plot(cv.carseats$k, cv.carseats$dev, type='b')

prune.carseats = prune.misclass(tree.carseats, best=8)
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred = predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
(89+62)/200 # Did a tiny bit worse overall but evened out the miscalssification a bit

### Regression Trees
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b') #Unpruned is best

yhat=predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2) # On average I'm off my $6000 which is pretty bad

### Bagging and random Forest
install.packages("randomForest")
library(randomForest)
set.seed(1)

?randomForest # importance is ??? importance of predecessors ???
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, importance=T) #Bagging since m=p
yhat.bag = predict(bag.boston, newdata=Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) # Now im only off by an average of $5000, still bad

rf.boston=randomForest(medv~., data=Boston, subset=train, importance=T) # default m = sqrt(p)
yhat.rf = predict(rf.boston, newdata=Boston[-train, ])
plot(yhat.rf, boston.test)
abline(0,1)
mean((yhat.rf-boston.test)^2) # Now im only off by an average of $4200, still bad

importance(rf.boston)
varImpPlot(rf.boston) # Looks like rm and lstat are the most important variables

### Boosting
install.packages('gbm')
library(gbm)
set.seed(1)
?gbm

boost.boston = gbm(medv~., Boston[train,], distribution = 'gaussian', n.trees=5000, interaction.depth=4)
# Theres a lot of hyperparams up there. You probably want to cross validate to get the best ones
# default shrinkage is 0.001
summary(boost.boston) # Importance plot. Once again, rm and lstat are more important

yhat.boost = predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
mean((yhat.boost-boston.test)^2) # About the same as before

boost.boston = gbm(medv~., Boston[train,], distribution = 'gaussian', n.trees=5000, 
                   interaction.depth=4, shrinkage=0.01)
yhat.boost = predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
mean((yhat.boost-boston.test)^2) # Slightly better at lambda=0.01

boost.boston = gbm(medv~., Boston[train,], distribution = 'gaussian', n.trees=5000, 
                   interaction.depth=4, shrinkage=0.2)
yhat.boost = predict(boost.boston, newdata=Boston[-train, ], n.trees=5000)
mean((yhat.boost-boston.test)^2) # Worse better at lambda=0.2
