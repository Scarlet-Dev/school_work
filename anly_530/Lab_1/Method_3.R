# Method 3 - Regression on Decision Trees
whitewines <- read.csv('whitewines.csv')

str(whitewines)
summary(whitewines)
hist(whitewines$quality)

table(whitewines$quality)
# Data is normally distributed

set.seed(8899)

wine_train <- whitewines[1:3674,]
wine_test <- whitewines[3675:4898,]

prop.table(table(wine_train$quality))
prop.table(table(wine_test$quality))


wine.part <- rpart(quality ~ ., data=wine_train)
wine.part

wine.predict <- predict(wine.part, wine_test)

rpart.plot(wine.part, digits=3)
rpart.plot(wine.part, digits=4, fallen.leaves = TRUE, type = 3, extra = 101)

# Evaluation
summary(wine.predict)
summary(wine_test$quality)

cor(wine.predict, wine_test$quality)

# Q3- What is your interpretation about this amount of RMSE?