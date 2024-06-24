library(kernlab)
letters <- read.csv('letterdata.csv')

str(letters)
sum(is.na(letters$letter))

set.seed(09981)

letters_train <- letters[1:18000,]
letters_train$letter <- factor(letters_train$letter)

letters_test <- letters[18001:20000,]
letters_test$letter <- factor(letters_test$letter)


letter_classifier <- ksvm(letter ~ ., data = letters_train)
summary(letter_classifier)

letters_prediction <- predict(letter_classifier, letters_test)

(p <- table(letters_prediction, letters_test$letter))
agreement <- letters_prediction == letters_test$letter
table(agreement)

( Accuracy_3 <- sum(diag(p))/sum(p) * 100 )

# Q4- We may be able to do better than this by changing the Kernels. Try Polynomial and RBF
# kernels to improve the result.