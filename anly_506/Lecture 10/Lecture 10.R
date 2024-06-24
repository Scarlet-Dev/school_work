#Clear workspace
rm(list=ls())
#Clear plots
dev.off()
#Set working directory


setwd("~/source/class_assignments/anly_506/Lecture 10")


#Slide 22
titanic=read.csv("titanic.csv")
str(titanic)

#Slide 23
#Determine unique values
sapply(titanic, function(x) length(unique(x)))
#Determine missing values
sapply(titanic, function(x) sum(is.na(x)))
#Subset the titanic data in order to eliminate variables with too many missing values, etc.
#and check the structure of the new data frame, "data" 
data=subset(titanic, select=c(2,3,5,6,7,8,10,12))
str(data)

#Slide 24
#Take care of missing values for age by replacing them with the average value.
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)
#Check data type
class(data$Sex)
data$Sex <- factor(data$Sex)
contrasts(data$Sex)
#Take care of missing values for Embarked by removing them
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL
#Create training dataset
train <- data[1:800,]
#Create test dataset
test <- data[801:889,]
model <- glm(Survived~., family=binomial(link='logit'), data=train)

#Slide 25
summary(model)

#Slide 26
anova(model,test="Chisq")
fitted.results=predict(model, newdata=subset(test, select=c(2,3,4,5,6,7,8)),type='response')
fitted.results=ifelse(fitted.results>0.5,1,0)
misClasificError=mean(fitted.results!=test$Survived)
print(paste("Accuracy",1-misClasificError))

#Slide 31
scaledata=titanic[c("Age","Fare")]
scaledata2=scale(scaledata)
