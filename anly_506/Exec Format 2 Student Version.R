
#####Slide 30#####
  #Generate random number
  runif(1)
  #Set seed
  set.seed(345)
  runif(1)
  runif(1)
    

#####Slide 36#####
  #Setup
    
    #Clear workspace
    rm(list=ls())
    #Clear plots
    dev.off()
    #Set working directory
    setwd('~/source/class_assignments/anly_506')
    #Read in data
    bikes = read.csv('/media/scarlet-base/database_space/raw_data/anly506/bikeVendors.csv')
    
  j#Pre-Processing
    
    #Separate cluster data and overlay data
    overlayData=bikes[ , 1:5]
    clusterData=bikes[ , -(1:5)]
    #Transpose so vendors are observations, models are columns
    clusterData=t(clusterData)
    #Set column names to make data more interpretable
    colnames(clusterData)=bikes$model
    #Validate proportions sum to 1
    rowSums(clusterData)
    
    
#####Slide 40#####    
    #Determine optimal number of clusters
    set.seed(1234)
    if(!require('factoextra')){install.packages('factoextra')}
    #Elbow method
    fviz_nbclust(clusterData, kmeans, method = "wss", nstart=20)

    
#####Slide 41#####    
    #Silhoutte method
    fviz_nbclust(clusterData, kmeans, method = "silhouette", nstart=20)
    
    
#####Slide 42#####
    #Gap stat
    gap_stat <- clusGap(clusterData, FUN = kmeans, K.max=29, nstart=20)
    fviz_gap_stat(gap_stat)
  
    
#####Slide 43#####    
    #Calculate with final number of clusters
    set.seed(1234)
    kMeansResult = kmeans(clusterData, centers = 9)
    
    
#####Slide 44#####        
    #Pull out locations of final centroids
    library(dplyr)
    centers = data.frame(t(kMeansResult$centers))
    #Combine with overlay data
    results = bind_cols(overlayData,centers)
    #Redefine price as factor variable
    results$price = ifelse(results$price>mean(results$price), "High", "Low")
    results$price = as.factor(results$price)
    #Top 10 models for bike vendors in Cluster 1
    cluster1=arrange(results,by=desc(X1))[1:10,1:5]
    
    