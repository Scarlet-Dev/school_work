####Chapter 12#####
library(tidyverse)

#Clear workspace
rm(list=ls())
#Clear plots
dev.off()

#Option to format numbers as decimals rather than scientific notation
options(scipen = 999)

#Slide 9
#Generate and plot random datapoints
set.seed(1234)
x <- rnorm(12, rep(1:3,each=4), 0.2)
y <- rnorm(12, rep(c(1,2,1),each=4), 0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

#Slide 10
#Create distance matrix
dataFrame <- data.frame(x=x, y=y)
dist(dataFrame)
rdistxy <- as.matrix(dist(dataFrame))
rdistxy <- round(rdistxy,digits=2)

#Slide 14
#Generate dendogram
hClustering <- data.frame(x=x,y=y) %>% dist %>% hclust
plot(hClustering)

####Chapter 13#####

#Slide 23
set.seed(1234)
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

#Slide 29
dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame,centers=3)
names(kmeansObj)
kmeansObj$cluster

#Slide 30
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)

#Slide 31
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj <- kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n",main = "Original Data")
image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n", main = "Clustered Data")
