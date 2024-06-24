#Clear environment
rm(list=ls())

#Clear plots
dev.off()

#Set working directory
setwd("C:\\Users\\Owner\\Documents\\ANLY\\EDA\\Lectures\\Draft 1st Executive Fall18")

###Setup###

#Slide 9
#Check which version of R you have
version

#Slide 10
#Install rJava and load. If there are issues and errors, you probably need to update Java.
if(!require("rJava")){install.packages("rJava")}
library("rJava")

###Getting Started###

#Slide 14
X <- 3			#assign X the value 3
X = 3			#same as previous line
vector = c(1, 2, 3, 4)	#define a vector
vector2 = vector*2	#scale the vector
Y = max(vector2)	#assign m to the max of vector 2 (8)
Z = "This is a string"	#assign Z to a string
Z == Y

###Importing Data###

#Slide 15
#View all built in datasets in R
data()
#Read in data from built in dataset
data(mtcars)
#The dataset will appear as a dataframe once it's used
#Summary provides the min, 1st quartile, mean, 3rd quartile, and max for every variable in the dataset
summary(mtcars)
#Get the names of all variables in the dataset
names(mtcars)
#Get a summary for just one variable
summary(mtcars$mpg)

#Slide 16
#First row
mtcars [ 1, ]
#Second column
mtcars [ , 2]
#Subset data into 2 new datasets based on mpg
EnvironmentalCars=mtcars[mtcars$mpg>20,]
notEnvironmentalCars=mtcars[mtcars$mpg<=20,]
#Compare horsepower for 2 subsets
summary(EnvironmentalCars$hp)
summary(notEnvironmentalCars$hp)

#Slide 19
#Read csv file
houses=read.csv("houses.csv")
#Only read in first 100 rows
houses2=read.csv("houses.csv",nrows=100)
#Read txt file
music=read.table("music.txt",sep="")
#Only read in first 100 rows
music2=read.table("music.txt",sep="",nrows=100)

#Slide 20
#########Exercise##########

#Slide 21
#Install xlsx package if not already installed
if(!require("xlsx")){install.packages("xlsx")}
#Load package
library(xlsx)
#Read xlsx file, 2nd argument is sheet number
housesFromExcel=read.xlsx2("housesExcel.xlsx",1)

#Slide 22
#Read csv from url
fires=read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"))
#Only read in the first 100 rows
fires2=read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"),nrows=100)

#Slide 30
#Install sqldf package if not already installed
if(!require("sqldf")){install.packages("sqldf")}
#Load package
library(sqldf)
sqldf("SELECT * FROM mtcars WHERE mpg > 20")

#Slide 31
#Subset on the read in statement
expensiveHouses = read.csv.sql("houses.csv",
                               sql="select * from file where SalePrice < 200000",
                               eol="\n")


#Slide 35
#Install packages
if(!require("Quandl")){install.packages("Quandl")}
library(Quandl)
#to use an API key
Quandl.api_key("P4H2wmsg4VzKvTLfUHzU")
#Pull Apple financial data
AppleData = Quandl("WIKI/AAPL")
#Pull Amazon financial data
AmazonData = Quandl("WIKI/AMZN")

#Slide 38
#Install packages and load into memory
if(!require('tidyr')){install.packages('tidyr')}
if(!require('stringr')){install.packages('stringr')}
if(!require('rvest')){install.packages('rvest')}
library('tidyr')
library('stringr')
library('rvest')
#Download html
url = 'http://espn.go.com/nfl/superbowl/history/winners'
webpage = read_html(url)
#Extract all of the tables from the html
sb_table = html_nodes(webpage, 'table')
#Convert to dataframe
sb = html_table(sb_table)[[1]]

