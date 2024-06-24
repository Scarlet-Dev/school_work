library(dplyr)
library(readr)
library(caret)
library(corrplot)

# setwd('./Project/')
pa_availability <- read_csv('./processed/pa_availability_cleaned.csv') %>% 
  mutate(Zip.Code = as.numeric(factor(Zip.Code))) %>% 
  mutate(HasAvailability = factor(as.character(HasAvailability)))
pa_availability <- pa_availability[-c(1,3)]

prop.table(table(pa_availability$HasAvailability)) * 100

set.seed(876)
# ---------------------------------------------------------------------------- #
# Algo 1 Train/Split 80/20
index_1 <- createDataPartition(pa_availability$HasAvailability, 
                                          p = 0.8, list = FALSE)

train_1 <- pa_availability[index_1,]
test_1 <- pa_availability[-index_1,]

prop.table(table(train_1$HasAvailability)) * 100
prop.table(table(test_1$HasAvailability)) * 100

write.csv(train_1, './train_test/training_1.csv')
write.csv(test_1, './train_test/testing_1.csv')
#  -------------------------------------------------------------------------- #
# Algo 2 with Feature Selection (Correlation Analysis)
pa_cor <- cor(pa_availability[-c(13)])
# corrplot(corr = pa_cor, type = 'upper')

pa_highly_cor <- findCorrelation(pa_cor, 0.4)
pa_filtered <- pa_availability[,-(pa_highly_cor)+1]

index_2 <- createDataPartition(pa_filtered$HasAvailability, 
                               p = 0.8, list = FALSE)

train_2 <- pa_filtered[index_2,]
test_2 <- pa_filtered[-index_2,]

prop.table(table(train_2$HasAvailability)) * 100
prop.table(table(test_2$HasAvailability)) * 100

write.csv(train_2, './train_test/training_2.csv')
write.csv(test_2, './train_test/testing_2.csv')
# ---------------------------------------------------------------------------- #
# Algo 3 with Feature Engineering (Variable Imnportance & PCA)
pa_pca <- preProcess(pa_availability, method = 'pca')
pa_predict <- predict(pa_pca, pa_availability %>%
                                     na.omit())
pa_predict$Zip.Code <- pa_availability$Zip.Code

index_3 <- createDataPartition(pa_predict$HasAvailability,
                               p = 0.8, list = FALSE)

train_3 <- pa_predict[index_3,]
test_3 <- pa_predict[-index_3,]

prop.table(table(train_3$HasAvailability)) * 100
prop.table(table(test_3$HasAvailability)) * 100

write.csv(train_3, './train_test/training_3.csv')
write.csv(test_3, './train_test/testing_3.csv')

# rm(list=ls())
