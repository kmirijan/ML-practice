### Video 1: PC
dimnames(USArrests)
?apply
apply(USArrests, 2, mean)
apply(USArrests, 2, var) # PC is about Variance

pca.out = prcomp(USArrests, scale=T) # Makes PCs and scales values
pca.out
names(pca.out) # Principal Components don't care about signs
?biplot.prcomp
biplot(pca.out, scale=0, cex=.6)

### BOOK: PCA
states=row.names(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean) # 2nd input is line axis. 1 is by rows, 2 is by columns
apply(USArrests, 2, var)

pr.out = prcomp(USArrests, scale=T)
names(pr.out)
pr.out$center # means of the variables used during scaling
pr.out$scale # standard deviation of variables used during scaling

pr.out$rotation # loadings
pr.out$x # PC score vectors

biplot(pca.out, scale=0, cex=.6) # Plot first 2 PCs

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0, cex=.6) # Mirror image by inversing loadings and score vectors

pr.out$sdev #sd of each PC
pr.var = pr.out$sdev^2
pr.var #Varaince explained by each PC
pve = pr.var/sum(pr.var)
pve # Percent of variance explained by each PC

barplot(pve, ylim=c(0,1), col = 'blue')
cumsum(pve)
plot(cumsum(pve), type='b', ylim=c(0,1))

### Video 2: K-means Clustering
set.seed(101)

x=matrix(rnorm(100*2), 100, 2) # R norm 100 rows 2 columns
xmean = matrix(rnorm(8, sd=4), 4, 2) # R norm 4 rows, 2 cols of means
which=sample(1:4, 100, replace=TRUE) # Assigns a mean value to a row
x = x+xmean[which, ] # Add each mean to the original x value
plot(x, col=which, pch=19) # Now the data is artificaially clustered

km.out=kmeans(x,4,nstart=15) # Do kmeans on x with k=4 and 15 random starts
km.out
plot(x, col=km.out$cluster, cex=2, pch=1, lwd=2)
points(x, col=which, pch=19)
points(x, col=c(1,3,4,2)[which], pch=19) # Visualize cluster mismatch

### BOOK: K-Means Clustering
set.seed(1)
x = matrix(rnorm(50*2), ncol=2)
x[1:25, 1] = x[1:25, 1]+3
x[1:25, 2] = x[1:25, 2]-4
km.out=kmeans(x,2,nstart=20)
km.out$cluster
km.out
plot(x, col=km.out$cluster, pch=20) # With K=2

set.seed(4)
km.out=kmeans(x,3,nstart=20) # Say we don't know k as we normally wouldn't. Let's try K=3
km.out
plot(x, col=km.out$cluster, pch=20)

### Video 3: Heirarchical Clustering
hc.complete=hclust(dist(x), method='complete')
?hclust  
?dist
plot(hc.complete)
hc.single=hclust(dist(x), method='single')
plot(hc.single)
hc.average=hclust(dist(x), method='average')
plot(hc.average)

hc.cut=cutree(hc.complete, 4)
table(hc.cut, which)
table(hc.cut, km.out$cluster)
plot(hc.complete, labels=which)

### BOOK: Heirarchical Clustering : SKIP, Same as Video
### Video Q1
?rbind
X = rbind(x, x.test)
pca.out=prcomp(X, scale=T)
sum((pca.out$sdev[1:5])^2) # Answers in video are wrong. I double checked with their answer code

### BOOK: NC160 Data Lab
library(ISLR)
nci.labs = NCI60$labs #Cancer type labels
nci.labs
nci.data = NCI60$data
nci.data
dim(nci.data)
table(nci.labs)

pr.out = prcomp(nci.data, scale=T)
Cols = function(vec){
  cols=rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab='Z1', ylab='Z2')
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab='Z1', ylab='Z3') # Neither of these plots looks great to me
summary(pr.out)
plot(pr.out)

pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type='o', col='blue') # Look for the elbow
barplot(pve, col='blue', ylim=c(0, max(pve)+1))
plot(cumsum(pve), type='o', col='red', ylim=c(0,100)) # Scree

sd.data= scale(nci.data)
par(mfrow= c(3,1))
data.dist=dist(sd.data)
plot(hclust(data.dist, method='complete'), main='Complete')
plot(hclust(data.dist, method='average'), main='Average')
plot(hclust(data.dist, method='single'), main='Single')

hc.out=hclust(data.dist, method='complete')
hc.clusters= cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col='red')
hc.out

set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters) # Clusters found are different

hc.out = hclust(dist(pr.out$x[, 1:5]))
plot(hc.out) # Clustering on principal components
