library(dplyr)
library(NbClust)
setwd('~/hu_lectures/anly_506/Quizes')

raw_data <- read.delim2('./DiabetesData.txt', header = FALSE, sep = ",")
str(raw_data)


colnames(raw_data) <- c("NumPregnancy",
                        "Plasma_Glucose_Concentration",
                        "Diastolic_BP",
                        "TricepsThickness",
                        "Insulin",
                        "BMI",
                        "Diabetes_Pedigree",
                        "Age",
                        "Class")
str(raw_data)

pima <- raw_data

pima$BMI <- as.numeric(pima$BMI)
pima$Diabetes_Pedigree <- as.numeric(pima$Diabetes_Pedigree)
pima$Class <- factor(pima$Class)
glimpse(pima)
summary(pima)

df <- scale(pima[,-9])
glimpse(df)

source('wssplot.R')

nc1 <- wssplot(df)
nc2 <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")
nc2
