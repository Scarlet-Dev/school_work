# Method 4 - Decision Trees and Random Forest
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

# Decision Tree
news_model <- C5.0(news.train[-17], news.train$shares)
summary(news_model)
CrossTable(news.test$shares, news_predict, prop.chsq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual Shares', 'Predicted Shares'))

# Prediction
news_predict <- predict(news_model, news.test)
(p <- table(news_predict, news.test$shares))
(Accuracy <- sum(diag(p))/sum(p) * 100)


### Random Forest
news_predict_model <- randomForest::randomForest(shares ~ ., data = news.train)
summary(news_predict_model)

# Prediction
news_predict <- predict(news_predict_model, news.test)
(p <- table(news_predict, news.test$shares))
(Accuracy <- sum(diag(p))/sum(p) * 100)

### Random Forest + Regression/Classification
news.part <- train(shares ~ ., data = news.train, method='rpart', metric="Accuracy", trControl=trainControl(method="cv", number = 10))

summary(news.part$finalModel)

plot(news.part, uniform=TRUE, main="Classification Tree")
text(news.part$finalModel, use.n = TRUE, all= TRUE, cex=.8)
