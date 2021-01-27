### 4.6.1
library(ISLR)
names(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket) # Produces error since Direction is not numeric
Smarket[1:5,-1]
Smarket[1:5,-2]
Smarket[1:5,-9]
cor(Smarket[,-9]) > abs(.6) # Doing a correlation check
plot(Year, Volume)

### 4.6.2
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=Smarket)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4] # Only p-value
glm.prob = predict(glm.fit, type = 'response') # response outputs probability of the form P(Y=1 | X)
glm.prob[1:10]
contrasts(Direction) # See the dummy variable associated with each classification
glm.pred=rep("Down", 1250) # Creates 1250 Down elements
?rep()
glm.pred
glm.pred[glm.prob>.5]="Up" # Transforms everything in glm.pred to Up if the probability > .5
table(glm.pred, Direction) # Get the confusion matrix
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
dim(Smarket)
dim(Smarket[train,])
Direction.2005=Direction[!train]
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=Smarket, subset=train) #Make model on training set
summary(glm.fit)
glm.prob=predict(glm.fit, Smarket.2005, type='response') #Predict on !train set
glm.pred=rep("Down", 252)
glm.pred[glm.prob>.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

#Make model on training set using less predictors
glm.fit = glm(Direction~Lag1+Lag2, family=binomial, data=Smarket, subset=train) 
summary(glm.fit)
glm.prob=predict(glm.fit, Smarket.2005, type='response') #Predict on !train set
glm.pred=rep("Down", 252)
glm.pred[glm.prob>.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1, -0.8)), "response")

#4.6.3
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data = Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.pred$x
lda.pred$class
lda.pred$posterior
lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)
lda.pred$posterior[,1]>=.5
sum(lda.pred$posterior[,1]>=.5) # How many Down predictions >=.5
sum(lda.pred$posterior[,1]<.5) # How many Down Prediction <.5
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>=.9) # How many Down predictions >=.5

#4.6.4
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.pred = predict(qda.fit, Smarket.2005)
qda.class=qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

#4.6.5
library(class)
train.X=cbind(Lag1, Lag2)[train,] # Binds the Lag1 and Lag2 vectors where train==TRUE into a single matrix
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]
set.seed(1) #In knn ties, R will randomly break the tie. Set a seed to ensure reproduibility 
knn.pred=knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)
knn.pred=knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

#4.6.6
dim(Caravan)
attach(Caravan)
?Caravan
summary(Caravan)
summary(Purchase)
standardized.X=scale(Caravan[,-86]) # Scale all predictor variables
var(Caravan[,1]) # Variance of variable 1
var(Caravan[,2])
?var()
var(standardized.X[,1])
var(standardized.X[,2]) # Every column has sd of 1 and mean of 0
standardized.X
#Make train and test sets
test = 1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
?knn
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred) #Our prediction error rate
mean(test.Y!="No") # Binary Classification of always saying No
table(knn.pred, test.Y)
# With K=3
knn.pred=knn(train.X, test.X, train.Y, k=3)
mean(test.Y!=knn.pred) #Our prediction error rate
mean(test.Y!="No") # Binary Classification of always saying No
table(knn.pred, test.Y)
# With K=5
knn.pred=knn(train.X, test.X, train.Y, k=5)
mean(test.Y!=knn.pred) #Our prediction error rate
mean(test.Y!="No") # Binary Classification of always saying No
table(knn.pred, test.Y)

# Check against logistic regression
glm.fit=glm(Purchase~., Caravan, family = binomial, subset=-test)
glm.prob=predict(glm.fit, Caravan[test,], type = 'response')
glm.pred=rep("No", 1000)
glm.pred[glm.prob>.5]="Yes"
table(glm.pred, test.Y)
glm.pred[glm.prob>.25]='Yes'
table(glm.pred, test.Y)
