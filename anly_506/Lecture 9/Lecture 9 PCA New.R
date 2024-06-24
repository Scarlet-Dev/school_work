####Slide 10####

  #Clear workspace
  rm(list=ls())
  #Clear plots
  dev.off()
  
  #Set working directory
  setwd("~/source/class_assignments/anly_506/Week 9")
  #Read in data
  wine=read.csv("wine.csv")
  
  #Take a glimpse
  download.packages(dplyr)
  library(dplyr)
  glimpse(wine)

####Slide 11####
  
  #Pre-Processing
    #Drop the Y variable
    X=wine[,-1]
    #Check for missing values
    summary(X)
    #Scale the X variable
    X_scaled=scale(X)
    
  #Perform PCA
    winePCA <- prcomp(X_scaled)
    summary(winePCA)

####Slide 13####
    
  #Keep only the first 5 PCs
  X_new=winePCA$x[,(1:5)]
  #Take a look at the data which is now transformed into principal components.
  head(X_new)

####Slide 15####
  
  #Take a look at the weights used to generate the Principal Components
  weights=winePCA$rotation[,(1:5)]
  head(weights)
  #Quickly verify that the sum of the squared weights is equal to 1
  weight_totals=data.frame(colSums(weights^2))

####Slide 16####  
  
  #Read in data
  EU = read.csv('EU.csv')
  
  #Take a glimpse
  # library(dplyr)
  glimpse(EU)
  
####Slide 17####
####Exercise####
  rownames(EU) <- EU$Country
  EU_scaled <- scale(EU[,-1])
  glimpse(EU_scaled)
  
####Slide 19####
  #Imports vs. GDP
  library(ggplot2)
  p = ggplot(EU, aes(Imports, GDP, label = rownames(EU)))
  p + geom_text(check_overlap = T)

####Slide 20####  
  
  #Labor force vs. Population
  p = ggplot(EU, aes(Labor.force, Population, label = rownames(EU)))
  p + geom_text(check_overlap = T)

####Slide 22####  
  
  #Missing values
  summary(EU)
  #Replace missing values with mean for all columns
  for(i in 1:ncol(EU)){EU[is.na(EU[,i]), i] <- mean(EU[,i], na.rm = TRUE) }
  summary(EU)
  
  #Scale data
  EU_scaled=scale(EU[,-1])
  
####Slide 23####
####Exercise####
  EUPCA <- prcomp(EU_scaled)
  summary(EUPCA)
  EU_new = EUPCA$x[,(1:5)]
  EUAfterPCA <- data.frame(EUPCA$x)

  summary(EUAfterPCA)
    
####Slide 24####
  
  #Plot first 2 principal components
  p = ggplot(EUAfterPCA[,1:2], aes(PC1, PC2, label = rownames(EUAfterPCA)))
  p + geom_text(check_overlap = T)
  
####Slide 25####
####Exercise####
set.seed(123)
kMeansOriginal = kmeans(EU_scaled, centers = 5, nstart = 20)  
kMeansOriginal$cluster

kMeansPCA = kmeans(EUAfterPCA[,1:3], centers = 5, nstart = 20)
kMeansPCA$cluster
  
####Slide 26####  
  compareClusterResults=data.frame(Original=kMeansOriginal$cluster,PCA=kMeansPCA$cluster, Index=1:28)
  clusterPlot=ggplot(compareClusterResults, aes(x=Original,y=Index, label=row.names(compareClusterResults)))
  
  OriginalClusteringPlot=
    clusterPlot + geom_text(aes(color=factor(kMeansOriginal$cluster)))+ labs(title = "Original Data",x="Cluster Assigned",y="", color="Cluster Assigned")
  OriginalClusteringPlot
  
  PCAClusteringPlot=
    clusterPlot + geom_text(aes(x=PCA, color=factor(kMeansPCA$cluster)))+ labs(title = "First 3 Principal Components",x="Cluster Assigned",y="", color="Cluster Assigned")
  PCAClusteringPlot
