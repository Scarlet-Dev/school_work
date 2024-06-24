# Method 1 - Decision Tree

# library(C50)
# library(gmodels)
# library(ggplot2)
# library(rpart)
# library(rpart.plot)

credit <- read.csv('credit.csv')

str(credit)
summary(credit$Credit.Amount)
table(credit$Creditability)

set.seed(12345)

credit_rand <- credit[order(runif(1000)), ]

# Using a 90-20 split
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$Creditability))
prop.table(table(credit_test$Creditability))

credit_test$Creditability <- factor(credit_test$Creditability)
credit_train$Creditability <- factor(credit_train$Creditability)

credit_model <- C5.0(x = credit_train[-1], y = credit_train$Creditability)

# Predict - Model vs Test
credit_predict <- predict(credit_model, credit_test)
summary(credit_predict)


# Confusion Table
CrossTable(credit_test$Creditability, credit_predict, prop.chsq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn=c('Actual Credability', 'Predicted Credability'))

# Evaluation
(p_1 <- table(credit_predict, credit_test$Creditability))
(Accuracy_1 <- sum(diag(p_1))/sum(p_1) * 100)
credit.mpart <- rpart(Creditability ~ ., data = credit_train)

summary(credit.mpart)
summary(credit.mpart$Creditability)

# Visualize
rpart.plot(credit.mpart, type = 1, extra = 102)

# Q1- If you see an accuracy of 100%, what does it mean? Does this mean that we design a perfect
# model? This is some thing that needs more discussion. Write a few sentences about accuracy of
# 100%.