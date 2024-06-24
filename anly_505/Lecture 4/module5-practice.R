library(rethinking)
 data("WaffleDivorce")

 d<- WaffleDivorce
 
 # Standardize
 d$D <- standardize(d$Divorce)
 d$M <- standardize(d$Marriage)
 d$A <- standardize(d$MedianAgeMarriage)

# D1 ~ Normal(mu_i, sigma)
#  mu_i <- alpha + beta_alpha(alpha_i)
#   
#  
 
 sd(d$MedianAgeMarriage) 

 m5.1 <- quap(
   alist(
     
   )
 ) 