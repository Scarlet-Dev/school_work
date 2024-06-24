library(NbClust)
library(cluster)

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }

wine <- read.csv('wine.csv')

df <- scale(wine[-1])

#Examine the data frame and plot the within sum of squares
head(df)
wssplot(df)

#Start the k-Means analysis using the variable "nc" for the number of clusters

set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc = 15, method = "kmeans")
print(table(nc$Best.n[1,]))
barplot(table(nc$Best.n[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria", main =
          "Number of Clusters Chosen by 26 Criteria")

#Enter the best number of clusters based on the information in the table and barplot
n <- readline(prompt = "Enter the best number of clusters: ")
n <- as.integer(n)

#Conduct the k-Means analysis using the best number of clusters

set.seed(1234)
fit.km <- kmeans(df, n, nstart=25)
print(fit.km$size)
print(fit.km$centers)
print(aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean))

#Use a confusion or truth table to evaluate how well the k-Means analysis performed
ct.km <- table(wine$Wine, fit.km$cluster)
print(ct.km)

#Generate a plot of the clusters using cluster package
clusplot(df, fit.km$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#Set-up to train a model for classification of wines using rpart
df <- data.frame(k=fit.km$cluster, df)
print(str(df))

#Randomize the dataset
rdf <- df[sample(1:nrow(df)), ]
print(head(rdf))
train <- rdf[1:(as.integer(.8*nrow(rdf))-1), ]
test <- rdf[(as.integer(.8*nrow(rdf))):nrow(rdf), ]

#Train the classifier and plot the results
fit <- rpart(k ~ ., data=train, method="class")

# Create plot using rpart.Plot and RColorBrewer and rattle
fancyRpartPlot(fit)

#Now use the predict() function to see how well the model works
pred <- predict(fit, test, type="class")
(p_2 <- prop.table(table(pred, test$k)))
(Accuracy_2 <- sum(diag(p_2))/sum(p_2) * 100)
