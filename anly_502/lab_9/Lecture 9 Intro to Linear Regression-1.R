
#Introduction To Linear Regression

# In this lab we'll be looking at data from all 30 Major League
# Baseball teams and examining the linear relationship between runs scored in a season and 
# a many of other player statistics.

# Our aim will be to summarize these relationships both graphically and numerically in order to find 
# which variable, if any, helps us best predict a team's runs scored in a season.


#The data

#Let's load up the data for the 2011 season.
download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")

load("mlb11.RData")

#In addition to runs scored, there are seven traditionally used variables in the data set: at-bats, hits, 
#home runs, batting average, strikeouts, stolen bases, and wins.
#There are also three newer variables: on-base percentage, slugging percentage, and on-base plus slugging. 
#For the first portion of the analysis we'll consider the seven traditional variables.
#At the end of the lab, you'll work with the newer variables on your own.

# Exercise 1: 
# What type of plot would you use to display the relationship between runs and one of the
# other numerical variables?
print("Scatter")
# Plot this relationship using the variable at_bats as the predictor.
# Does the relationship look linear? If you knew a team's at_bats, would you be comfortable using a # 
# linear model to predict the number of runs?

############################################
plot(mlb11$at_bats, mlb11$runs)

m1 = (lm( mlb11$runs ~ mlb11$at_bats))
summary(m1)
abline(m1)

print("There is a moderate but positive relationship between the at_bats and runs made. It is not likely the 
      prediction will be correct for most bats or runs.")

#If the relationship looks linear, we can quantify the strength of the relationship with the correlation coefficient.
cor(mlb11$runs, mlb11$at_bats)
######################################################################

# Sum of squared residuals

# Think back to the way that we described the distribution of a single variable.
# Recall that we discussed characteristics such as center, spread, and shape.
# It's also useful to be able to describe the relationship of two numerical variables, 
# such as runs and at_bats above.

# Exercise 2:
# Looking at your plot from the previous exercise, describe the relationship between 
# these two variables. Make sure to discuss the form, direction, and strength of the relationship
# as well as any unusual observations.
print("The two variables have a moderate and positive relationship with each other. The points have a relatively small range with most
      cluster near the intercept of the graph.")
# Just as we used the mean and standard deviation to summarize a single variable, we can summarize 
# the relationship between these two variables by finding the line that best follows
# their association. Use the following interactive function to select the line that you think
#  does the best job of going through the cloud of points.

plot_ss(x = mlb11$at_bats, y = mlb11$runs)

# After running this command, you'll be prompted to click two points on the plot to define a line.
# Once you've done that, the line you specified will be shown in black and the residuals in blue.
# Note that there are 30 residuals, one for each of the 30 observations. 
# Recall that the residuals are the difference between the observed values and the values 
# predicted by the line:
  
#  ei=yi-y^i


# The most common way to do linear regression is to select the line that minimizes the
# sum of squared residuals. To visualize the squared residuals, you can rerun the plot command 
# and add the argument showSquares = TRUE.

plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

# Note that the output from the plot_ss function provides you with the slope and intercept of your 
# line as well as the sum of squares.

#Exercise 3
#Using plot_ss, choose a line that does a good job of minimizing the sum of squares.
#Run the function several times. What was the smallest sum of squares that you got?
#How does it compare to your neighbors?


##  The linear model Function lm()

#It is rather cumbersome to try to get the correct least squares line
#i.e. the line that minimizes the sum of squared residuals, through trial and error. 
#Instead we can use the lm function in R to fit the linear model (a.k.a. regression line).

m2 <- lm( runs ~ at_bats,data = mlb11)

# The first argument in the function lm is a formula that takes the form y ~ x. 
# Here it can be read that we want to make a linear model of runs as a function of at_bats.
# The second argument specifies that R should look in the mlb11 data frame to find the runs 
# and at_bats variables.

# The output of lm is an object that contains all of the information we need about the linear model that was just fit. We can access this information using the summary function.
summary(m2)

# Let's consider this output piece by piece. 
# First, the formula used to describe the model is shown at the top. 
# After the formula you find the five-number summary of the residuals. 
# The "Coefficients" table shown next is key; 
# its first column displays the linear model's y-intercept and the coefficient of at_bats. 

#With this table, we can write down the least squares regression line for the linear model:
  
##  runs = 2789.2 +0.6305*atbats


# One last piece of information we will discuss from the summary output is the
# Multiple R-squared, or more simply, R 2. The R 2 value represents 
# the proportion of variability in the response variable that is explained
# by the explanatory variable. For this model, 37.3% of the variability in
# runs is explained by at-bats.

# Exercise 4

# Fit a new model that uses homeruns to predict runs. 
lmHR = lm(mlb11$runs ~ mlb11$homeruns )
summary(lmHR)

# Using the estimates from the R output, write the equation of the
# regression line. 
# runs = 415.24 + 1.84 * homeruns

# how does the plot look like??
plot(mlb11$runs ~ mlb11$homeruns)
abline((lmHR))

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

print("The homeruns relationship with runs is mdoerately positive.")

#What does the slope tell us in the context of the relationship between 
#success of a team and its home runs?
summary(lmHR)
print("By achieving more homeruns the team will have an imporve runs for the season.")

# 

#########################################################################
# Prediction and prediction errors

# Let's create a scatterplot with the least squares line laid on top.
plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

# The function abline plots a line based on its slope and intercept. Here, we used a shortcut
# by providing the model m1, which contains both parameter estimates. 
# This line can be used to predict y at any value of x 

# NOTE!!!! -- When predictions are made for values of x that are beyond the
# range of the observed data, it is referred to as extrapolation and is
## not usually recommended. 

# However, predictions made within the range of the data are more reliable.
# They're also used to compute the residuals.

#Exercise 5

# If a team manager saw the least squares regression line and not the actual data,
# how many runs would he or she predict for a team with 5,578 at-bats?
##  runs = -2789.2 +0.6305*atbats
## 5578
##  runs = -2789.2 +0.6305*5578

predict(at_bats, data.frame(at_bats = 5578), se.fit = TRUE)

mlb11$runs[mlb11$at_bats==5579]
# 875

# Is this an overestimate or an underestimate, and by how much?
# In other words, what is the residual for this prediction?
print('')

#  Model diagnostics

#To assess whether the linear model is reliable, we need to check for
# (1) linearity, (2) nearly normal residuals, and (3) constant variability.

#Linearity: You already checked if the relationship between runs and at-bats is linear 
#using a scatterplot. We should also verify this condition with a plot of the residuals vs. at-bats.
#Recall that any code following a # is intended to be a comment that helps understand the code but is ignored by R.

plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3)  # adds a horizontal line at y = 0  and the line type = 3 for dashed line

#Exercise 6

#Is there any apparent pattern in the residuals plot?
#What does this indicate about the linearity of the relationship between runs and at-bats?

#Nearly normal residuals: To check this condition, we can look at a histogram
hist(m1$residuals)

# or a normal probability plot of the residuals.
qqnorm(m1$residuals)
qqline(m1$residuals)  # adds diagonal line to the normal prob plot

# Exercise 7

# Based on the histogram and the normal probability plot, 
# does the nearly normal residuals condition appear to be met?

# Constant variability:
#  Exercise 8

# Based on the plot in (1), does the constant variability condition appear to be met?
##############################################################################################
#On Your Own

#1.Choose another traditional variable from mlb11 that you think might be a good predictor of runs. Produce a scatterplot of the two variables and fit a linear model. At a glance, does there seem to be a linear relationship?


#2.How does this relationship compare to the relationship between runs and at_bats? Use the R2  

#values from the two model summaries to compare. Does your variable seem to predict runs better than at_bats? How can you tell?


#3.Now that you can summarize the linear relationship between two variables, investigate the relationships between runs and each of the other five traditional variables. Which variable best predicts runs? Support your conclusion using the graphical and numerical methods we've discussed (for the sake of conciseness, only include output for the best variable, not all five).


#4.Now examine the three newer variables. These are the statistics used by the author of Moneyball to predict a teams success. In general, are they more or less effective at predicting runs that the old variables? Explain using appropriate graphical and numerical evidence. Of all ten variables we've analyzed, which seems to be the best predictor of runs? Using the limited (or not so limited) information you know about these baseball statistics, does your result make sense?


#5.Check the model diagnostics for the regression model with the variable you decided was the best predictor for runs.

