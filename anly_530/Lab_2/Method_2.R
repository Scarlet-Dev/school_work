library(caret)
set.seed(88912)

credit_rand <- credit[order(runif(1000)),]

creditDataScaled <- scale( credit_rand[, 2:ncol(credit_rand)], 
                           center = TRUE, scale = TRUE )
credit.cor <- cor(creditDataScaled)
(highlyCor <- findCorrelation(credit.cor, 0.30))

filteredCredit <- credit_rand[, -(highlyCor[4]+1)]
filteredCredit$Creditability <- factor(filteredCredit$Creditability)

filteredTrain <- filteredCredit[1:750,]
filteredTest <- filteredCredit[751:1000,]

filteredTrain$Creditability <- factor(filteredTrain$Creditability)
filteredTest$Creditability <- factor(filteredTest$Creditability)


credit.nb2.1 <- naive_bayes(Creditability ~ ., data = filteredTrain)

filteredTestPredict <- predict(credit.nb2.1, newdata = filteredTest)
(conf_table2 <- table(filteredTestPredict, filteredTest$Creditability))
 
(Accuracy_2_1 <- sum(diag(conf_table2))/sum(conf_table2) * 100)

# Q3- What is the accuracy this time? Be sure to include in your results report 
# whether or not, after all this work, the performance of your 
# Naïve Bayes Classifier was improved.