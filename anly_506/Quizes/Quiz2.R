setwd("~/hu_lectures/anly_506")

master <- read.csv("./College used for Quiz1-1.csv")

college <- master

# Question 5
college$Private <- factor(college$Private)
summary(college)

# Question 6
boxplot(Apps ~ Private, ylim=c(0,25000), data = college)

# Question 7
private_apps = college[college$Private == "Yes", ]  

summary(private_apps)

# Question 8
plot(college$Apps, college$Accept, col=college$Private, ylim=c(0,10000), xlim=c(0,15000))
legend("topleft", legend=c("Public", "Private"), fill=1:2)

# Question 9
plot(college$Enroll, college$Accept, col=college$Private, ylim=c(0,15000), xlim=c(0,5000))
legend("topleft", legend=c("Public", "Private"), fill=1:2)

# Question 10
subcollege<- data.frame(college$Apps, college$Accept, college$Enroll, college$Top10perc, college$Outstate)
str(subcollege)

cor(subcollege)
