#Clear workspace
rm(list=ls())

#Remove plots
dev.off()

#Set working directory
setwd("C:\\Users\\Owner\\Documents\\Tommy\\Lectures\\Lec#11 BigData")

#Install required packages
#tm requires R version 3.3.1 or later to run
if(!require("tm")){install.packages("tm")}
if(!require("slam")){install.packages("slam")}
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("syuzhet")){install.packages("syuzhet")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("SnowballC")){install.packages("SnowballC")}
library(SnowballC)

#Slide 9
#Read in data and take a glimpse
emailsFull=read.csv("emails.csv")
glimpse(emailsFull)

#-----Pre-processing-----#
#Slide 10
#Convert variable types
emailsFull$MetadataSubject=as.character(emailsFull$MetadataSubject)
emailsFull$ExtractedBodyText=as.character(emailsFull$ExtractedBodyText)
emailsFull$MetadataDateSent=as.Date(emailsFull$MetadataDateSent)
emailsFull$MetadataDateReleased=as.Date(emailsFull$MetadataDateReleased)
glimpse(emailsFull)

#Slide 11
#Replace all "/n" in the data with spaces, effectively removing them
emailsFull$ExtractedBodyText=sapply(emailsFull$ExtractedBodyText,function(x)gsub("\n"," ",x))
glimpse(emailsFull)

#Slide 12
#Remove emails which are blank
emailsFull=emailsFull[emailsFull$ExtractedBodyText!="",]

#--------Data Summary-------#
#Slide 13
#Summary
summary(emailsFull)

#Slide 14
#Create histogram
hist(emailsFull$MetadataDateSent,"quarters")

#Sample Data
#Slide 16
#Set seed so that results stay the same.
#If you use the same seed, you will get the same selection every time.
set.seed(1234)
#Create 4,000 random numbers between 1 and 6,742 (number of rows in the data).
#Samples without replacement by default.
randomRowNumbers= sample(nrow(emailsFull), 4000)
#Subset the full dataset to just the rows which were selected randomly
emails=emailsFull [ randomRowNumbers,  ]

#Slide 17
#Compare the distribution in the sample with that in the population.
hist(emails$MetadataDateSent,"quarters")

#------NLP Pre-Processing-------#
#Slide 20
#In NLP, a Corpus is a collection of documents, where documents are observations in the data.
#In this example the Corpus is the set of all emails. Each email is a document.
#In R, we need to first convert to Corpus in order to do most NLP analysis.
library(tm)
myCorpus = Corpus(VectorSource(emails$ExtractedBodyText))

#Slide 22
# Convert the text to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# Remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# Remove english common stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
# Remove punctuations
myCorpus <- tm_map(myCorpus, removePunctuation)
# Eliminate extra white spaces
myCorpus <- tm_map(myCorpus, stripWhitespace)
# Text stemming (reduces words to their root form)
myCorpus <- tm_map(myCorpus, stemDocument)

#-----Term Document Matrix-----#
#Slide 24
#Create term document matrix
tdm <- TermDocumentMatrix(myCorpus)
#Convert to matrix
m <- as.data.frame(as.matrix(tdm))
#Calculate total for each term
m$total=rowSums(m)
#Create new dataframe with words and total frequencies
wordFreq=data.frame(word=rownames(m),freq=m$total)
#Sort by descending frequency
wordFreq=arrange(wordFreq,by=desc(freq))

#-----Word Cloud----------#
#Slide 25
library("wordcloud")
library("RColorBrewer")
#To save time, will only create the word cloud for the top 500 words.
wordFreq2=wordFreq[(1:500),]
#par sets default graph colors. use to set background color
par(bg="grey30")
#Create wordcloud plot
wordcloud(wordFreq2$word, wordFreq2$freq, col=terrain.colors(length(wordFreq2$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )

#----Sentiment Analysis----#
library('syuzhet')
#syuzhet package Uses NRC Emotion Lexicon
#The NRC emotion lexicon is a list of words and their associations with
#8 emotions-anger, fear, anticipation, trust, surprise, sadness, joy, and disgust
#and 2 sentiments-negative and positive 

#Slide 28
#The get_nrc_sentiment function returns a data frame
#in which each row represents a sentence from the original file.
#columns are emotions/sentiments, cells are freq of words with those emotions
#Convert Corpus back into character vector
cleanedText=unlist(myCorpus)
#Get sentiment
df=get_nrc_sentiment(cleanedText)
#Total sentiment
sentimentTotal=data.frame(sentiment=names(df),count=colSums(df))
library(ggplot2)
#Create plot of sentiments
qplot(sentiment, data=sentimentTotal[(1:8),], weight=count, geom="bar",fill=sentiment)+ggtitle("Email sentiments")

#-----Search-----#
#Slide 29
#Transpose TDM using t() function. Now documents are rows and terms are columns.
#This is now a document term matrix (DTM)
dtm=data.frame(t(m))
#Drop last row which is the totals
dtm=dtm[-4001,]
#Combine the original dataset with this document term matrix
search=cbind(emails,dtm)

#Slide 30
#Subset the dataset to just emails which contain the word "danger"
danger=search[search$danger>0,]
#Export data
danger2=danger$ExtractedBodyText
write.csv(danger2,"danger.csv")

#Slide 31
#Subset the dataset to just emails which contain the word "war"
#and were also sent by Hillary (represented as "H" in the data)
war=search[search$war>0&search$MetadataFrom=="H",]
