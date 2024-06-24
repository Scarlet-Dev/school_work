# Sampling from Ames, Iowa

#If you have access to data on an entire population, say the size of 
# every house in Ames, Iowa, it's straight forward to answer questions 
# like, "How big is the typical house in Ames?" and "How much variation is
# there in sizes of houses?". If you have access to only a sample of the
#population, as is often the case, the task becomes more complicated.

# Q: What is your best guess for the typical size if you only know the sizes 
# of several dozen houses? This sort of situation requires that you use
#your sample to make inference on what your population looks like.

# Q: How much variation is # there in sizes of houses?"

# Q What is your confidence in your estimate?  WHat is the uncertainty of the estimate??
# What is you confidence that it good sample estimate of the true population mean!!! 

#The data

#In the previous lab, ``Sampling Distributions'', we looked at the population
# data of houses from Ames, Iowa. Let's start by loading that data set.

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "./lab_6/ames.RData")
load("./lab_6/ames.RData")
names(ames)


#In this lab we'll start with a simple random sample of size 60 from the 
#population. Specifically, this is a simple random sample of size 60.
#Note that the data set has information on many housing variables, but for 
#the first portion of the lab we'll focus on the size of the house, 
#represented by the variable  Gr.Liv.Area .

population <- ames$Gr.Liv.Area
samp <- sample(population, 60)
mean(samp)

# 1.Describe the distribution of your sample. 
print("The distribution of the graph is not normal in this sample. It has two peaks making it binomial and having a very fat right tail.")

# What would you say is the "typical" size within your sample? 
print("The typical size in this sample is around 1500.")

# Also state precisely what you interpreted "typical" to mean.
print("In this case typical would mean the observation with the highest frequency.")

summary(population)
summary(samp)
xlimits <- range(samp) # limiting the values of the x axis
hist(samp, breaks = 20, xlim = xlimits)

# 2.Would you expect another student's distribution to be identical to yours?
print("The distribution of sample's is influenced by the randomness of selecting the area values. It is very unlikely that another students sample distribution would be identical. Only share some similarities.")

# Would you expect it to be similar? Why or why not?
print("Sample distribution between students may be similar in regards to distribution, outliers and having the tail be skewed to the right.")

#  Confidence intervals
# One of the most common ways to describe the typical or central value of a
# distribution is to use the mean. In this case we can calculate the mean of 
# the sample using,
sample_mean <- mean(samp)

# Return for a moment to the question that first motivated this lab: 
# based on this sample, what can we infer about the population?
print("The average living area of people who live within Ames is around 1500.")

# Based only on this single sample, the best estimate of the average living
# area of houses sold in Ames would be the sample mean, usually denoted
# as X-bar (here we're calling it  sample_mean ). That serves as a 
# good point estimate but it would be useful to also communicate how 
# uncertain we are of that estimate.
# This can be captured by using a CONFIDENCE INTERVAL.

# We can calculate a 95% confidence interval for a sample mean by adding and
# subtracting 1.96 standard errors to the point estimate 
# (See Section 4.2.3 if you are unfamiliar with this formula).
# Also look at Pg 175


se <- sd(samp) / sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se
c(lower, upper)


# This is an important inference that we've just made: 
# even though we don't know what the full population looks like,
# we're 95% confident that the true average size of houses in Ames 
# lies between the values lower and upper.
# There are a few conditions that must be met for this interval to be valid.

# 3.For the confidence interval to be valid, 
# a) the sample mean must be normally distributed and
# b) have standard error std/sqrt{n}.
## ***********************************************************
# What conditions must be met for this to be true?
#` obs must be independent -> if sampling is random & <10% of population
#  Sample Size & Skewness: The population distribution is normal possible 
#  If not, the sample size should be large enough (>30) to CLT to apply and assume normality.
#******************************************************************

#  Confidence levels
#  4.What does "95% confidence" mean? If you're not sure, see Section 4.2.2.
print("The confidence interval means the area under the line that a possible value will fall under.")

# In this case we have the luxury of knowing the true population mean since
# we have data on the entire population. 
# This value can be calculated using the following command:

  mean(population)

# 5.Does your confidence interval capture the true average size of houses in Ames?
  print("The mean population of Ames does not capture the current sample's confidence interval.")
  # If you are working on this lab in a classroom, does your neighbor's 
  # interval capture this value?


# 6.Each student in your class should have gotten a slightly different
# confidence interval.
# What proportion of those intervals would you expect to capture the true 
# population mean? Why? If you are working in this lab in a classroom,
# collect data on the intervals created by other students in the class and
# calculate the proportion of intervals that capture the true population 
# mean.


# Using R, we're going to recreate many samples to learn more about how sample means and confidence intervals vary from one sample to another. Loops come in handy here (If you are unfamiliar with loops, review the Sampling Distribution Lab).

#Here is the rough outline:
#  .Obtain a random sample.
#  .Calculate and store the sample's mean and standard deviation.
#  .Repeat steps (1) and (2) 50 times.
#  .Use these stored statistics to calculate many confidence intervals.

# But before we do all of this, we need to first create empty vectors where we can save the means and standard deviations that will be calculated from each sample. And while we're at it, let's also store the desired sample size as  n .
 
  samp_mean <- rep(NA, 50)
  samp_sd <- rep(NA, 50)
  n <- 60

# Now we're ready for the loop where we calculate the means and standard deviations of 50 random samples.
for(i in 1:50){
  samp <- sample(population, n) # obtain a sample of size n = 60 from the population
  samp_mean[i] <- mean(samp)    # save sample mean in ith element of samp_mean
  samp_sd[i] <- sd(samp)        # save sample sd in ith element of samp_sd
}

#Lastly, we construct the confidence intervals.

lower_vector <- samp_mean - 1.96 * samp_sd / sqrt(n) 
upper_vector <- samp_mean + 1.96 * samp_sd / sqrt(n)


# Lower bounds of these 50 confidence intervals are stored in  lower_vector,
# and the upper bounds are in  upper_vector . Let's view the first interval.
c(lower_vector[15], upper_vector[15])
par(mfrow = c(1, 1))
plot_ci(lower_vector, upper_vector, mean(population))


#On your own

# .Using the following function (which was downloaded with the data set),
# plot all intervals. What proportion of your confidence intervals include
# the true population mean? Is this proportion exactly equal to the
# confidence level? If not, explain why.
par(mfrow = c(1, 1))
plot_ci(lower_vector, upper_vector, mean(population))

# .Pick a confidence level of your choosing, provided it is not 95%.
#  What is the appropriate critical value?

# .Calculate 50 confidence intervals at the confidence level you chose
# in the previous question. You do not need to obtain new samples, 
# simply calculate new intervals based on the sample means and
# standard deviations you have already collected. Using the  plot_ci  function, plot all intervals and calculate the proportion of intervals that include the true population mean. How does this percentage compare to the confidence level selected for the intervals?


#This is a product of OpenIntro that is released under a [Creative Commons Attribution-ShareAlike 3.0 Unported](http://creativecommons.org/licenses/by-sa/3.0). This lab was written for OpenIntro by Andrew Bray and Mine ?etinkaya-Rundel. 