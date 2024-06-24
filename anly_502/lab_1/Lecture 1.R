source("http://www.openintro.org/stat/data/arbuthnot.R")
#Assignment 1
class(arbuthnot)
str(arbuthnot)

dim(arbuthnot)
head(arbuthnot)
tail(arbuthnot)
head(arbuthnot, 10)

str(arbuthnot$boys)

class(boys)
class(arbuthnot$boys)
boys = "Blue"


boys = "male"
girls = "female"

class(boys)

number = 8
vect = c(1,2,3,4,6,8,0)
(vect)

names(arbuthnot)

str(arbuthnot)

summary(arbuthnot)

arbuthnot$boys

str(arbuthnot)
summary(arbuthnot$boys)
name = "srikar"
class(name)
class(TRUE)

arbuthnot$girls
length(arbuthnot$girls)
sum(arbuthnot$girls)
summary(arbuthnot$girls)

arbuthnot[8,2]
1:3
arbuthnot[c(5,8),1:2]
arbuthnot[c(5,8),]
arbuthnot[ 2:5 , 2:3]

plot(x = arbuthnot$year, y = arbuthnot$girls)
plot(x = arbuthnot$year, y = arbuthnot$boys)
plot(x = arbuthnot$year, y = arbuthnot$girls, type = "l", col = "red")

arbuthnot$total = arbuthnot$girls + arbuthnot$boys
arbuthnot$test = NA

plot(x = arbuthnot$year, y = arbuthnot$total, type="l", xlab ="Year", ylab="count")

