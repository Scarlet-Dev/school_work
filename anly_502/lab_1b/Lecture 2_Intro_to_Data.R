rm(list=ls())

# We begin by loading the data set of 20,000 observations into the R workspace.
#After launching RStudio, enter the following command.

source("http://www.openintro.org/stat/data/cdc.R")

names(cdc)
summary(cdc$hlthplan)
summary(cdc)
head(cdc)
class(cdc)
str(cdc)
class(cdc$gender)
str(cdc$gender)
# The data set cdc that shows up in your workspace is a data matrix, 
# with each row representing a case and each column representing a variable.
# R calls this data format a data frame, which is a term that will be used 
# throughout the labs.

#To view the names of the variables, type the command
# Each of these variables corresponds to questions asked in the survey
# will see the possible responses also later
names(cdc)

#########################################################
#  Some Questions here we want to answer first:
# 	How many cases are there in this data set?
#   How many variables?
#   For each variable, identify its data type (e.g. categorical, discrete)
### 
dim(cdc)
nrow(cdc)
ncol(cdc)
l
# START at Looking at a few rows at top and bottom
head(cdc)
#and similarly we can look at the last few by typing
tail(cdc,10)

## Look at the variables as Summaries and Tables 
###############################################
#  Summary and table functions
#################################################
# A good first step in any analysis is to distill all of that information into a few summary statistics and graphics.
# As a simple example, the function summary returns a numerical summary: minimum, first quartile, median, mean, second quartile, and maximum. For weight this is
str(cdc)
summary(cdc)
class(cdc)
## LOOK AT A SPECIFIC VARIABLE

summary(cdc$weight)

########################################################
#  R also has built-in functions to compute summary statistics one by one. 
#  For instance, to calculate the mean, median, and variance of weight, type
# NUMERIC VARIABLES STATS
mean(cdc$weight) 
var(cdc$weight)
median(cdc$weight)
sd(cdc$weight)

################################################################
#  While it makes sense to describe a quantitative variable like weight in terms of these statistics,
#  what about categorical data? We would instead consider the sample frequency or relative frequency distribution.
#  The function table does this for you by counting the number of times each kind of response was given.
#  For example, to see the number of people who have smoked 100 cigarettes in their lifetime, type

## CATEGORICAL/Integral Numbers VARIABLE  STATS
table(cdc$smoke100)
table(cdc$gender)
table(cdc$genhlth)
## or instead look at the relative frequency distribution by typing (  Why 20000 ?)
numRows <- nrow(cdc)
table(cdc$smoke100)/numRows
# table(cdc$smoke100)/20000

# Notice how R automatically divides all entries in the table by 20,000 in the command above. 
# This is similar to something we observed in the Introduction to R; 
# when we multiplied or divided a vector with a number, R applied that action across entries in the vectors. 
# As we see above, this also works for tables. 

#############################################################
##   Next, we make a bar plot of the entries in the table by
#putting the table inside the barplot command.
##  We saw plot() command last time
barplot(table(cdc$smoke100))
barplot(cdc$smoke100)

#  can we directly write: table(cdc$smoke100)/nrows(cdc)
barplot(table(cdc$smoke100)/nrow(cdc))
#
#Notice what we've done here! We've computed the table of cdc$smoke100 and then immediately applied
#the graphical function, barplot. This is an important idea: R commands can be nested. 
#You could also break this into two steps by typing the following:
smoke <- table(cdc$smoke100)
barplot(smoke)

# Here, we've made a new object, a table, called smoke 
# (the contents of which we can see by typing smoke into the console)
# and then used it in as the input for barplot.
# The special symbol <- or = performs an assignment, taking the output of one line of code and saving it 
# into an object in your workspace. This is another important idea that we'll return to later.

################### EXERCISE ########################################
## Exercise: Let us see some other numerical calulations.
## Create a numerical summary for height and age, and compute the interquartile range for each. 
## Compute the relative frequency distribution for gender and exerany. 
## How many males are in the sample? What proportion of the sample reports being in excellent health?
##
#####################################################################

# Now The table command can be used to tabulate any number of variables that you provide.
## For example, to examine which participants have smoked across each gender, we could use the following.
table(cdc$gender,cdc$smoke100)  ## Have we seen this before???

table(cdc$genhlth,cdc$smoke100)

barplot(table(cdc$genhlth,cdc$smoke100))

table(cdc$genhlth,cdc$gender,cdc$smoke100)
 
## Here, we see column labels of 0 and 1. Recall that 1 indicates a respondent has smoked at least 100 cigarettes.
## The rows refer to gender. 
## To create a mosaic plot of this table, we would enter the following command.
mosaicplot(table(cdc$gender,cdc$smoke100))

#  We could have accomplished this in two steps by saving the table in one line and applying mosaicplot in the next
## (see the table/barplot example above).

##   What does the mosaic plot reveal about smoking habits and gender?

########################################################################################
#  Interlude: How R thinks about data & Accessing data in a DataFrame

###############################################################################
#  We mentioned that R stores data in data frames, which you might think
# of as a type of spreadsheet. Each row is a different observation 
#(a different respondent) and each column is a different variable 
#(the first is genhlth, the second exerany and so on).
#We can see the size of the data frame next to the object name in the 
#workspace or we can type
dim(cdc)

##  which will return the number of rows and columns. 
##  Now, if we want to access a subset of the full data frame, we can use row-and-column notation.
##  For example, to see the sixth variable of the 567th respondent (row=567, col = 6), use the format 
cdc[567,1]

## which means we want the element of our data set that is in the 567th row (meaning the 567th person or observation)
## and the 6th column (in this case, weight). We know that weight is the 6th variable because it is the 6th entry 
##  in the list of variable names
names(cdc)

## To see the weights for the first 10 respondents we can type
cdc[5:10,6]  ## what are we doing here??

cdc[c(5,10,15,20),7:8]

## In this expression, we have asked just for rows in the range 1 through 10.
## R uses the : to create a range of values, so 1:10 expands to 1, 2, 3, 4, 5, 6, 7, 8, 9, 10. 
## You can see this by entering
1:10

## Finally, if we want all of the data for the first 10 respondents, type
cdc[1:10,6]

## By leaving out an index or a range (we didn't type anything between the comma and the square bracket),
## we get all the columns. When starting out in R, this is a bit counterintuitive.
## As a rule, we omit the column number to see all columns in a data frame.
## Similarly, if we leave out an index or range for the rows, we would access all the observations,
## not just the 567th, or rows 1 through 10.
## Try the following to see the weights for all 20,000 respondents fly by on your screen Z(But will stop reaching the display limit)
yy = cdc[,6]
##  REMEMBER THE COMMA IS VERY IMPORTAN IN THE DATAFRAME data selection
## Now Recall that column 6 represents respondents weight, so the command above reported all of the weights in the data set. An alternative method to access the weight data is by referring to the name. Previously, we typed names(cdc) to see all the variables contained in the cdc data set. We can use any of the variable names to select items in our data set.
xxx = cdc$weight   ## Looking at only one specific column.  Same as the above statement
                  ## Notice the $ symbol
##  The dollar-sign tells R to look in data frame cdc for the column called weight. Since that's a single vector,
##  Now we can subset it with just a single index inside square brackets. We see the weight for the 567th respondent by typing
cdc$weight[567]  ## WHY DO WE NOT HAVE ANY COMMA HERE??????
#same as
cdc[567,6]  #  Similarly, for just the first 10 respondents
cdc$weight[1:10]  ## WHY DO WE NOT HAVE ANY COMMA HERE??????

## The command above returns the same result as the cdc[1:10,6] command.
##  Both row-and-column notation and dollar-sign notation are widely used,
#  which one you choose to use depends on your personal preference.

##
##  A little more on subsetting
##  It's often useful to extract all individuals (cases) in a data set that have specific characteristics.
##  We accomplish this through conditioning commands. First, consider expressions like

xxx = cdc$gender == "m"
## or
yy = cdc$age > 30

##  REMEMBER These commands produce a series of TRUE and FALSE values. 
##There is one value for each respondent, where TRUE indicates that the person was male (via the first command)
## or older than 30 (second command).

##  NOW - Suppose we want to extract just the data for the men in the sample, or just for those over 30. 
##  We can use the R function --  subset() -- to do that for us. For example, the command

mdata <- subset(cdc, cdc$gender == "m")
Over30Data = subset(cdc, cdc$age > 30)

barplot(table(cdc$smoke100))
xxx<- subset(cdc, cdc$gender == "m" & cdc$age > 30)

##  will create a new data set called mdata that contains only the men from the cdc data set.
##  In addition to finding it in your workspace alongside its dimensions, you can take a peek at the first several rows as usual
head(mdata)
dim(mdata)

## This new data set contains all the same variables but just under half the rows.
#  It is also possible to tell R to keep only specific variables, which is a topic we'll discuss in a future lab.
##
##  For now, the important thing is that we can carve up the data based on values of one or more variables.

## Subsetting a dataframe with multiple conditions.
##  As an aside, you can use several of these conditions together with (and) & and (or) |. The & is read "and" so that
m_and_over30 <- subset(cdc, gender == "m" & age > 30)  ##  Note the == to check for equality.  (= is an assignmnt . same as <-  )

head(m_and_over30)

##  The above will give you the data for men over the age of 30. The | character is read "or" so that
m_or_over30 <- subset(cdc, gender == "m" | age > 30)
head(m_or_over30, 25)

 
##  will take people who are female or less than the age of 30 (why that's an interesting group is hard to say, but right now the mechanics of this are the important thing). In principle, you may use as many "and" and "or" clauses as you like when forming a subset.

##################   EXERCISE  TRY this Out.  ###################################
###  HOW DO WE Create a new object called under23_and_smoke that contains all observations of respondents under the age of 23 
##   that have smoked 100 cigarettes in their lifetime. Write the command you used to create the new object as the answer to this exercise.

##########################################################################################################################

##  Quantitative data
##  With our subsetting tools in hand, Let us make basic summaries of the BRFSS questionnaire. 
## We've already looked at categorical data such as smoke and gender so now let's turn our attention
#  to quantitative data.

##  Two common ways to visualize quantitative data are with box plots and histograms.
##  We can construct a box plot for a single variable with the following command.

boxplot(cdc$height)
##  You can compare the locations of the components of the box by examining the summary statistics.
summary(cdc$height)

##  Confirm that the median and upper and lower quartiles reported in the numerical summary 
# match those in the graph.
##  The purpose of a boxplot is to provide a thumbnail sketch of a variable for the purpose of
##  comparing across several categories.
##  So we can, for example, compare the heights of men and women with

boxplot(cdc$height ~ cdc$gender)  ## The notation ~ here is new.
boxplot(cdc$height ~ cdc$smoke100 )  ## The notation ~ here is new.
boxplot(cdc$weight ~ cdc$smoke100 )  ## The notation ~ here is new.

## The ~ character can be read versus or as a function of. 
## So we're asking R to give us a box plots of heights where the groups are defined by gender.
##############################################################################
##  Next let's consider a new variable that doesn't show up directly in this data set: Body Mass Index (BMI) (http://en.wikipedia.org/wiki/Body_mass_index). BMI is a weight to height ratio and can be calculated as:
##  All You Workout addicts will know it
##
##  [ BMI = ( {weight~(lb)} divided by {height~(in)^2})  * 703 ]
##   703 is the approximate conversion factor to change units from metric (meters and kilograms) to imperial (inches and pounds).
##  The following two lines first make a new object called bmi and then creates box plots of these values, defining groups by the variable cdc$genhlth.
bmi <- (cdc$weight / cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)

##  Notice that the first line above is just some arithmetic, 
# but it's applied to all 20,000 numbers in the cdc data set.
##  That is, for each of the 20,000 participants, we take their weight,
#  divide by their height-squared and then multiply by 703.##  
##  The result is 20,000 BMI values, one for each respondent. 
#  This is one reason why we like R:
##  it lets us perform computations like this using very simple 
#   expressions.
##  4.	What does this box plot show? Pick another categorical variable
## from the data set and see how it relates to BMI.
##  List the variable you chose, why you might think it would have a 
#  relationship to BMI, and indicate what the figure seems to suggest.

boxplot(bmi ~ cdc$gender)
boxplot(bmi ~ cdc$smoke100)

#####################################################################################################################################

##  Finally, DATA VISUALIZATION-- let's make some histograms.
# We can look at the histogram for the age of our respondents 
#  with command

hist(cdc$age)

##  Histograms are generally a very good way to see the shape of a single distribution, 
#   but that shape can change depending
##  on how the data is split between the different bins. You can control the number of bins by 
##  adding an argument to the command.
##  In the next two lines, we first make a default histogram of bmi and then one with 50 breaks.
hist(bmi)
hist(bmi, breaks = 50, las=1)

##  Note that you can flip between plots that you've created by clicking the forward and backward arrows in the 
##  lower right region of RStudio, just above the plots. How do these two histograms compare?

##  At this point, we've done a good first pass at analyzing the information in the BRFSS questionnaire.
##  We've found an interesting association between smoking and gender, and we can say something about the relationship between 
##  people's assessment of their general health and their own BMI.
##  We've also picked up essential computing tools -- summary statistics, subsetting, and plots -- that will serve us well 
##  throughout this course.

###############################################################################################################

###  NOW ON YOUR OWN....
#.	Make a scatterplot of weight versus desired weight. Describe the relationship between these two variables.
plot( cdc$weight , cdc$wtdesire  )  
#.	Let's consider a new variable: the difference between desired weight (wtdesire) and current weight (weight).
#   Create this new variable by subtracting the two columns in the data frame and assigning them to a new object called wdiff.
wdiff = cdc$wtdesire - cdc$weight 
#.	What type of data is wdiff? If an observation wdiff is 0, what does this mean about the person's weight and desired weight.
#   What if wdiff is positive or negative?

#.	Describe the distribution of wdiff in terms of its center, shape, and spread, including any plots you use.
#   What does this tell us about how people feel about their current weight?

#.	Usingd  mumerical summaries and a side-by-side box plot, determine if men tend to view their weight differently than women.
#.	Now it's time to get creative. Find the mean and standard deviation of weight and determine 
#   what proportion of the weights are within one standard deviation of the mean.
