news <- read.csv('OnlineNewsPopularity_for_R.csv')
news_short <- data.frame(news$n_tokens_title, news$n_tokens_content, 
                         news$n_unique_tokens, news$n_non_stop_words, news$num_hrefs, 
                         news$num_imgs, news$num_videos, news$average_token_length, 
                         news$num_keywords, news$kw_max_max, news$global_sentiment_polarity, 
                         news$avg_positive_polarity, news$title_subjectivity, news$title_sentiment_polarity, 
                         news$abs_title_subjectivity, news$abs_title_sentiment_polarity, news$shares)
colnames(news_short) <- c("n_tokens_title", "n_tokens_content", "n_unique_tokens",
                          "n_non_stop_words", "num_hrefs", "num_imgs", "num_videos",
                          "average_token_length", "num_keywords", "kw_max_max",
                          "global_sentiment_polarity", "avg_positive_polarity",
                          "title_subjectivity", "title_sentiment_polarity", 
                          "abs_title_subjectivity", "abs_title_sentiment_polarity", "shares")

str(news_short)
summary(news_short)


news_short$shares <- as.factor(ifelse(news_short$shares > 1400, 'yes', 'no'))


set.seed(11323)

news_rand <- news_short[order(runif(10000)),]

news.train <- news_rand[1:9000,]
news.test <- news_rand[9001:10000,]

prop.table(table(news.train$shares))
prop.table(table(news.test$shares))

# Naive Bayes 1
news.nb1 <- naive_bayes(shares ~ ., data = news.train)
news.nb1.predict <- predict(news.nb1, news.test)

(p1 <- table(news.nb1.predict, news.test$shares))
(NewsAccuracy_1 <- sum(diag(p1))/sum(p1) * 100)

# Naive Bayes 2
newsDataScaled <- scale(news_rand[, -17], center = TRUE, scale = TRUE)
news.cor <- cor(newsDataScaled)
(newsHighlyCor <- findCorrelation(news.cor, 0.3))

filteredNews <- news_rand[, -(newsHighlyCor)+1]

filteredNews.Train <- filteredNews[1:9000,]
filteredNews.Test <- filteredNews[9000:10000,]

news.nb2 <- naive_bayes(shares ~ ., data = filteredNews.Train)
news.nb2.predict <- predict(news.nb2, filteredNews.Test)

(p2 <- table(news.nb2.predict, filteredNews.Test$shares))
(NewsAccuracy_2 <- sum(diag(p2))/sum(p2) * 100)

# SVM
news_classifier <- ksvm(shares ~ ., news.train)
summary(news_classifier)

news.ksvm.predict <- predict(news_classifier, news.test)

(p3 <- table(news.ksvm.predict, news.test$shares))
(NewsAccuracy_3 <- sum(diag(p3))/sum(p3) * 100)

# Q5- Do you see any improvement compared to last three techniques? Please completely explain
# your results and analysis.