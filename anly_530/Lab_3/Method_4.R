library(caret)
library(dplyr)
library(BBmisc)
library(NbClust)
library(class)

# Q4- Now let’s get back to our problem of news popularity and see if we can apply KNN (K-                                                                                        nearest neighbors) to improve the accuracy of the model. Use the same strategy of training and
# testing that we did on first 2 labs, and don’t forget that whenever it is required you should use:
# set.seed(12345). Use PCA to reduce the number of features.
news <- read.csv("OnlineNewsPopularity_for_R.csv")

# Only selecting the integer/numbers and the response variable
newsShort <- data.frame(news$n_tokens_title, news$n_tokens_content, 
                        news$n_unique_tokens, news$n_non_stop_words, 
                        news$num_hrefs, news$num_imgs, news$num_videos, 
                        news$average_token_length, news$num_keywords, 
                        news$kw_max_max, news$global_sentiment_polarity, 
                        news$avg_positive_polarity, news$title_subjectivity, 
                        news$title_sentiment_polarity, news$abs_title_subjectivity, 
                        news$abs_title_sentiment_polarity, news$shares)

colnames(newsShort) <- c("n_tokens_title", "n_tokens_content", "n_unique_tokens", 
                         "n_non_stop_words", "num_hrefs", "num_imgs", "num_videos", 
                         "average_token_length", "num_keywords", "kw_max_max", 
                         "global_sentiment_polarity", "avg_positive_polarity", 
                         "title_subjectivity", "title_sentiment_polarity", 
                         "abs_title_subjectivity", "abs_title_sentiment_polarity", 
                         "shares")

newsShort$shares <- as.factor(ifelse(newsShort$shares > 1400, 'yes', 'no'))

prop.table(table(newsShort$shares)) * 100

# First normalize using BBmisc.normalize
news.normal <- as.data.frame(lapply(newsShort[-17], normalize))

# PCA Section
news.pca <- preProcess(x = news.normal, method = c("pca"))
news.normal.pca <- predict(news.pca, news.normal %>% na.omit())
print(str(news.normal.pca))

set.seed(12345)

# Create a random 1000 sample from the PCA and normalized data
newsRand <- news.normal.pca[order(runif(2500)), ]

# Do a 80/20 split
news.train <- newsRand[1:2000,]
news.test <- newsRand[2001:2500,]

# Using the same index get the shares response variable, Split with 80/20
news.train.labels <- newsShort[row.names(newsRand[1:2000,]), 17]
news.test.labels <- newsShort[row.names(-newsRand[2001:2500,]), 17]

# Check proportions. Should expect some difference with certain variables being dropped
prop.table(table(news.train.labels)) * 100
prop.table(table(news.test.labels)) * 100

# Next, find the number of clusters
news.sample.normal <- news.normal[row.names(newsRand),]
news.kCluster <- NbClust(news.sample.normal, min.nc = 2,
                         max.nc = 10, method = 'kmeans')

# Perform KNN
news.knn.predict <- knn(train = news.train, test = news.test, 
                        cl = news.train.labels, k = 2)

# Evaluate the model
(p4 <- table(news.knn.predict, news.test.labels))
(NewsAccuracy <- sum(diag(p4))/sum(p4) * 100)
