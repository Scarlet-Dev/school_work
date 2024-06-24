# download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")

# load("kobe.RData")  # Kobe's shooting stats in few games
head(kobe)
tail(kobe)
dim(kobe)

#  For example, in Game 1 Kobe had the following sequence of hits and misses from his nine shot attempts in the first quarter:
# To verify this use the following command:

  kobe$basket[1:9] ## what is another way to get the same data??? 
  kobe[1:9,6]
  
##  Within the nine shot attempts, there are six streaks, which are separated by a "|" above.
##  Their lengths are one, zero, two, zero, zero, zero (in order of occurrence).
  
##  1.What does a streak length of 1 mean, i.e. how many hits and misses are in a streak of 1? What about a streak length of 0?
  # What is a streak length? A streak lenght is the measure of consecutive shots a player makes without missing. 
##   A streak length of 1 would mean Kobe was has the chance of making 1 consecutive shot. A length of zero would mean he has a consecutive shots that miss.
  
##  The custom function  calc_streak , which was loaded in with the data, may be used to calculate the lengths of all shooting streaks and then look at the distribution.

  kobe_streak <- calc_streak(kobe$basket)
##  Lets see how this looks...
  kobe_streak  
  
    barplot(table(kobe_streak))
    
    table(kobe$basket)/133
    
##  Note that instead of making a histogram, we chose to make a bar plot from a table of the streak data.
##  The bar plot is preferable here since our variable is discrete -- counts -- instead of continuous.

##    2.Describe the distribution of Kobe's streak lengths from the 2009 NBA finals.
##    The distribution is right skewed where     
    
##    What was his typical streak length? How long was his longest streak of baskets?
    
##    Compared to What?
    
##    We've shown that Kobe had some long shooting streaks, but are they long enough to support the belief
##    that he had hot hands? What can we compare them to?
#    Pr(shot1 =H) =.45
#    Pr(shot2= h given Shot1 = H)  > .45 ??  eg. 0.60  if Kobe has a hot hand.
    
## So Now that we've phrased the situation in terms of independent shots, the question is:
##    how do we tell if Kobe's shooting streaks are long enough to indicate that he has hot hands?
##    We can compare his streak lengths to someone without hot hands: an independent shooter.
##
##  Simulations in R
    
##  We don't have any data from a shooter we know to have independent shots.
##  But it is very easy to simulate such data in R. 
##  In a simulation, you set the ground rules of a random process and then the computer uses 
#   random numbers to generate an outcome that adheres to those rules.
#    As a simple example, you can simulate flipping a fair coin with the following.

    #define the outcomes
    outcomes <- c("heads", "tails")
    sample(outcomes, size = 1, replace = TRUE)

##   The vector  outcomes  can be thought of as a hat with two slips of paper in it: one slip says  heads
#    and the other says  tails . The function  sample  draws one slip from the hat and tells us if it was
#    a head or a tail.
    
##   Run the second command listed above several times. Just like when flipping a coin, sometimes you'll 
#    get a heads, sometimes you'll get a tails, but in the long run, you'd expect to get roughly 
#    equal numbers of each.
    
##    If you wanted to simulate flipping a fair coin 100 times, you could either run the function 100 
#    times or, more simply, adjust the  size  argument, which governs how many samples to draw
#    (the  replace = TRUE  argument indicates we put the slip of paper back in the hat before
#    drawing again). Save the resulting vector of heads and tails in a new object called  sim_fair_coin .
   
## SET A SEED
    set.seed(5)
  
    sim_fair_coin <- sample(outcomes, size = 1000, replace = TRUE)
    
    
##   To view the results of this simulation, type the name of the object and then use  table  to 
##   count up the number of heads and tails.
    sim_fair_coin
    table(sim_fair_coin)
    table(sim_fair_coin)/1000
    
    
##  Since there are only two elements in  outcomes , the probability that we "flip" a coin and it lands
#    heads is 0.5. Say we're trying to simulate an unfair coin that we know only lands heads 20% of the 
#    time. We can adjust for this by adding an argument called  prob , which provides a vector of 
#    two probability weights.
 
       sim_unfair_coin <- sample(outcomes, size = 10000, replace = TRUE, prob = c(0.2, 0.8))
       table(sim_unfair_coin)/1000
    
##    Notice prob=c(0.2, 0.8)  indicates that for the two elements in the  outcomes  vector, 
##    we want to select the first one,  heads , with probability 0.2 and the second one,  tails  with
##    probability 0.8. 
##    Another way of thinking about this is to think of the outcome space as a bag of 10 chips, 
##    where 2 chips are labeled "head" and 8 chips "tail". Therefore at each draw, the probability of 
##    drawing a chip that says "head"" is 20%, and "tail" is 80%.
    
##   3.In the simulation of flipping the unfair coin 100 times, how many flips came up heads?
##   read more on the sample function in help.  ?sample()
    
##     Simulating the Independent Shooter
   
##   Simulating a basketball player who has independent shots uses the same mechanism that we use to 
##    simulate a coin flip. To simulate a single shot from an independent shooter with a shooting percentage
##   of 50% we type,
  
      outcomes <- c("H", "M")
      sim_basket <- sample(outcomes, size = 1, replace = TRUE, prob = c(.5, 0.5))
      
#   To make a valid comparison between Kobe and our simulated independent shooter, we need to 
#    align both their shooting percentage and the number of attempted shots.
     
##   4.What change needs to be made to the  sample  function so that it reflects a shooting percentage 
#      of 45%? Make this adjustment, then run a simulation to sample 133 shots.
#      Assign the output of this simulation to a new object called  sim_basket .
      
      outcomes <- c("H", "M")
      sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(.436, 0.564))
      
##  Note that we've named the new vector  sim_basket , the same name that we gave to the 
##  previous vector reflecting a shooting percentage of 50%. 
##  In this situation, R overwrites the old object with the new one, so always make sure that 
##  you don't need the information in an old vector before reassigning its name.
  
##  With the results of the simulation saved as  sim_basket , we have the data necessary to compare 
##  Kobe to our independent shooter. We can look at Kobe's data alongside our simulated data.

      kobe$basket
      sim_basket

##  Both data sets represent the results of 133 shot attempts, each with the same shooting percentage of
##  45%. We know that our simulated data is from a shooter that has independent shots. 
##  That is, we know the simulated shooter does not have a hot hand. Each Shot in an independent computer 
##  generated random number following our Hit probability weight
  
##  COMPARE & ANALYZE THE TWO..  What is you conclusion????
    
      
            
##      Using  calc_streak , compute the streak lengths of  sim_basket .   
      
      calc_streak(sim_basket)
      
       barplot(table(calc_streak(sim_basket)))
       
#############################################################################       
       
##.Describe the distribution of streak lengths. What is the typical streak length for this 
## simulated independent shooter with a 45% shooting percentage? How long is the player's longest streak
## of baskets in 133 shots?
       
       
#       .If you were to run the simulation of the independent shooter a second time,
#       how would you expect its streak distribution to compare to the distribution from the question 
#       above? Exactly the same? Somewhat similar? ?Totally different Explain your reasoning.
#       (SEEDING THE SIMULATION????)
       
#       .How does Kobe Bryant's distribution of streak lengths compare to the distribution of streak 
#       lengths for the simulated shooter? Using this comparison, 
#       do you have evidence that the hot hand model fits Kobe's shooting patterns? Explain
       
       
