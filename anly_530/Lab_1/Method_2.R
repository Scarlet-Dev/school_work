# Method 2 - Random Forest
credit <- read.csv('credit.csv')

str(credit)
summary(credit$Credit.Amount)
table(credit$Creditability)

set.seed(6789)

credit_rand <- credit[order(runif(1000)), ]

# Using a 90-10 split
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$Creditability))
prop.table(table(credit_test$Creditability))

credit_test$Creditability <- factor(credit_test$Creditability)
credit_train$Creditability <- factor(credit_train$Creditability)

# Predict
random_credit_model <- randomForest::randomForest(Creditability ~ ., data = credit_train)
summary(random_credit_model)

cred_predict_rand <- predict(random_credit_model, credit_test)

# Evaluation
(p_2 <- table(cred_predict_rand, credit_test$Creditability))
(Accuracy_2 <- sum(diag(p))/sum(p) * 100)
credit.mpart <- rpart(Creditability ~ ., data = credit_train)

summary(credit.mpart)
summary(credit.mpart$quality)

# Visualize
rpart.plot(credit.mpart, type = 1, extra = 102)

# Q2- What are the three most important features in this model.
# Now, Change the random seed to 23458 and find the new accuracy of random forest.