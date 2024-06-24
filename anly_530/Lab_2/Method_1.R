require(naivebayes)

credit <- read.csv('creditData.csv')

str(credit)
summary(credit)

sum(is.na(credit))

set.seed(13347)

index <- createDataPartition(credit[,1], p = 0.90, list=FALSE)

credit.train <- credit[index,]
credit.test <- credit[-index,]

prop.table(table(credit.train$Creditability))
prop.table(table(credit.test$Creditability))

sum(is.na(credit.train))
sum(is.na(credit.test))

# Q1- (one thing to make sure you do!): Remember that the class variable needs to be a categorical
# data type in order to build a Naïve Bayes Classifier. This means that you’ll need to convert your
# class variable

credit.train$Creditability <- factor(credit.train$Creditability)
credit.test$Creditability <- factor(credit.test$Creditability)

credit.nb1 <- naive_bayes(Creditability ~ ., credit.train)
credit.nb1.predict <- predict(credit.nb1, credit.test)

(conf_table <- table(credit.nb1.predict, credit.test$Creditability))

(Accuracy_1 <- sum(diag(conf_table))/sum(conf_table) * 100)

# Next, use a 75%/25% split for training and test data, i.e. use 75% of the records for the training
# set and 25% of the records for the test set. Report the number of missing values you find in the
# data in your results report. Use the randomization seed of 12345.

index_two <- createDataPartition(credit[,1], p = 0.75, list=FALSE)

ctest2 <- credit[index_two,]
ctrain2 <- credit[-index_two,]

prop.table(table(ctrain2$Creditability))
prop.table(table(ctest2$Creditability))

sum(is.na(ctrain2))
sum(is.na(ctest2))

set.seed(12345)

ctrain2$Creditability <- factor(ctrain2$Creditability)
ctest2$Creditability <- factor(ctest2$Creditability)


credit.nb2 <- naive_bayes(Creditability ~ ., ctrain2)
credit.nb2.predict <- predict(credit.nb2, ctest2)

(conf_table <- table(credit.nb2.predict, ctest2$Creditability))

(Accuracy_2 <- sum(diag(conf_table))/sum(conf_table) * 100)


# Q2- Compute the percentage of both classes similar to what you did in lab 1 and see if the
# distribution of both classes preserved for both training and testing data.

