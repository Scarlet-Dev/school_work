# setwd('./Project')

library(tidyverse)
library(caret)
library(MLeval)


train_evaluate <- function(trainSet, validSet){
  
  control <- trainControl(method = 'cv', number = 10, classProbs = T, 
                          savePredictions = T)
  metric <- "Accuracy"
  
  fit.dt <- train(HasAvailability ~ ., data = trainSet, method = "rpart", 
                  metric = metric, trControl = control)
  
  fit.rf <- train(HasAvailability ~ ., data = trainSet, method = "rf", 
                  metric = metric, trControl = control)
  
  fit.svm <- train(HasAvailability ~ ., data = trainSet, method = "svmLinear",
                   metric = metric, trControl = control)
  
  results.smp <- resamples(list(dt = fit.dt, rf = fit.rf, svm = fit.svm))
  
  summary(results.smp)
  # lattice::dotplot(results.smp)
  
  dt.pred <- predict(fit.dt, newdata = validSet)
  rf.pred <- predict(fit.rf, newdata = validSet)
  svm.pred <- predict(fit.svm, newdata = validSet)
  
  cm.dt <- confusionMatrix(dt.pred, reference = validSet$HasAvailability,
                           mode = 'prec_recall')
  cm.rf <- confusionMatrix(rf.pred, reference = validSet$HasAvailability,
                           mode = 'prec_recall')
  cm.svm <- confusionMatrix(svm.pred, reference = validSet$HasAvailability,
                            mode = 'prec_recall')
  results.cms <- list(DecisionTree = cm.dt, 
                      RandomForest = cm.rf,
                      SVM = cm.svm)

  # 
  # print("------------------------------")
  # print("Decision Tree")
  # print(cm.dt)
  # 
  # print("------------------------------")
  # print("Random Forest")
  # print(cm.rf)
  # 
  # print("------------------------------")
  # print("Support Vector Machine")
  # print(cm.svm)
  
  res.dt <- MLeval::evalm(fit.dt, silent = T)
  res.rf <- MLeval::evalm(fit.rf, silent = T)
  res.svm <- MLeval::evalm(fit.svm, silent = T)
  
  results.eval <- list(DecisionTree = res.dt, 
                       RandomForest = res.rf,
                       SVM = res.svm)
  
  output <- list(resamples = results.smp, 
                 confusionMatrix = results.cms,
                 evaluation = results.eval)
  return(output)
}
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Training/Validation Set 1 - Scaling & Normalization only
trainSet1 <- read_csv('./train_test/training_1.csv') %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
trainSet1 <- trainSet1[-1]

validSet1 <- read_csv('./train_test/testing_1.csv') %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
validSet1 <- validSet1[-1]
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Training/Validation Set 2 - Feature Selection (Normalization & Correlation)
trainSet2 <- read_csv('./train_test/training_2.csv') %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
trainSet2 <- trainSet2[-1]

validSet2 <- read_csv('./train_test/testing_2.csv') %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
validSet2 <- validSet2[-1]
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# Train/Validation Set 3 - Feature Engineering (Normalization & PCA)
trainSet3 <- read_csv('./train_test/training_3.csv') %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
trainSet3 <- trainSet3[-1]

validSet3 <- read_csv('./train_test/testing_3.csv') %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
validSet3 <- validSet3[-1]
# ---------------------------------------------------------------------------- #


set.seed(8898)
# Algorithm 1
algo_1 <- train_evaluate(trainSet1, validSet1)

# Algorithm 2
algo_2 <- train_evaluate(trainSet2, validSet2)

# Algorithm 3
algo_3 <- train_evaluate(trainSet3, validSet3)

results_1 <- data.frame(`Decision Tree` = c(algo_1$confusionMatrix$DecisionTree$overall, algo_1$confusionMatrix$DecisionTree$byClass) * 100,
                        `Random Forest` = c(algo_1$confusionMatrix$RandomForest$overall, algo_1$confusionMatrix$DecisionTree$byClass) * 100,
                        `SVM` = c(algo_1$confusionMatrix$SVM$overall, algo_1$confusionMatrix$SVM$byClass) * 100) %>% 
  t() %>% 
  as.data.frame()

results_2 <- data.frame(`Decision Tree` = c(algo_2$confusionMatrix$DecisionTree$overall, algo_2$confusionMatrix$DecisionTree$byClass) * 100,
                        `Random Forest` = c(algo_2$confusionMatrix$RandomForest$overall, algo_2$confusionMatrix$DecisionTree$byClass) * 100,
                        `SVM` = c(algo_2$confusionMatrix$SVM$overall, algo_2$confusionMatrix$SVM$byClass) * 100) %>% 
  t() %>% 
  as.data.frame()

results_3 <- data.frame(`Decision Tree` = c(algo_3$confusionMatrix$DecisionTree$overall, algo_3$confusionMatrix$DecisionTree$byClass) * 100,
                        `Random Forest` = c(algo_3$confusionMatrix$RandomForest$overall, algo_3$confusionMatrix$DecisionTree$byClass) * 100,
                        `SVM` = c(algo_3$confusionMatrix$SVM$overall, algo_3$confusionMatrix$SVM$byClass) * 100) %>% 
  t() %>% 
  as.data.frame()
