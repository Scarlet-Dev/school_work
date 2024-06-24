#Clear environment
rm(list=ls())

#Clear plots
dev.off()

#Set working directory
setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lec#3")
#Slide 8
X <- 3			#assign X the value 3
X = 3			#same as previous line
vector = c(1, 2, 3, 4)	#define a vector
vector2 = vector*2	#scale the vector
Y = max(vector2)	#assign m to the max of vector 2 (8)
Z = "This is a string"	#assign Z to a string
Z == Y

#Slide 9
#View all built in datasets in R
data()
#Read in data from built in dataset
data(mtcars)
#The dataset will appear as a dataframe once it's used
summary(mtcars)

#Slide 11
#Read csv file
houses=read.csv("houses.csv")
#Only read in first 100 rows
houses2=read.csv("houses.csv",nrows=100)
#Read txt file
music=read.table("music.txt",sep="")
#Only read in first 100 rows
music2=read.table("music.txt",sep="",nrows=100)

#Slide 12
#Install xlsx package if not already installed
if(!require("xlsx")){install.packages("xlsx")}
#Load package
library(xlsx)
#Read xlsx file, 2nd argument is sheet number
housesFromExcel=read.xlsx2("houses.xlsx",1)

#Slide 13
#Read csv from url
fires=read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"))
#Only read in the first 100 rows
fires2=read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"),nrows=100)

#Slide 20
#Install sqldf package if not already installed
if(!require("sqldf")){install.packages("sqldf")}
#Load package
library(sqldf)
sqldf("SELECT * FROM mtcars WHERE mpg > 20")

#Slide 21
#Subset on the read in statement
expensiveHouses = read.csv.sql("houses.csv",
                               sql="select * from file where SalePrice < 200000",
                               eol="\n")
