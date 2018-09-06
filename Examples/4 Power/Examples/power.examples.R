#*********************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\power.examples.R    
# PURPOSE: Code for implementing the examples in Chapter 4 Power Calculations 
#          and Sample Size Determination
# DATE:    8/18/09                                                                
# AUTHOR:  R. Valliant                                                            
# REVISED: 01/08/10 Added 2-sample log-odds sample size calculation
#*********************************************************************************

###################################################################################
#   Example on power to detect alternative value of mu + delta in class notes
###################################################################################
power.t.test(n = 250,
    power = NULL,
    delta = 5000, 
    sd = 3300 * sqrt(250), 
    type = "one.sample",
    alt = "one.sided",
    sig.level = 0.05
    )

###################################################################################
#   Example 1 on difference in means in class notes
###################################################################################
power.t.test(power = 0.8,
    delta = 5, 
    sd = sqrt(200), 
    type = "two.sample",
    alt = "one.sided",
    sig.level = 0.05
    )
    
     Two-sample t test power calculation 

              n = 99.60428
          delta = 5
             sd = 14.14214
      sig.level = 0.05
          power = 0.8
    alternative = one.sided

 NOTE: n is number in *each* group 

power.t.test(power = 0.9,
    delta = 5, 
    sd = sqrt(200), 
    type = "two.sample",
    alt = "one.sided",
    sig.level = 0.05
    )
    
     Two-sample t test power calculation 

              n = 137.7033
          delta = 5
             sd = 14.14214
      sig.level = 0.05
          power = 0.9
    alternative = one.sided

 NOTE: n is number in *each* group 

##################################################################################
#   Example 2 on difference in proportions in class notes
###################################################################################
power.prop.test(power = 0.8,
    p1 = 0.15, 
    p2 = 0.18,
    alt = "one.sided",
    sig.level = 0.05
    )
    
power.prop.test(n = 1000,
    p1 = 0.15, 
    p2 = 0.18,
    alt = "one.sided",
    sig.level = 0.05
    )

##################################################################################
#   Example 3 on difference in proportions in class notes
###################################################################################
power.prop.test(n = 1000,
    p1 = 0.50, 
    p2 = 0.53,
    alt = "one.sided",
    sig.level = 0.05
    )

power.prop.test(n = 1000,
    p1 = 0.50, 
    p2 = 0.53,
    alt = "two.sided",
    sig.level = 0.05,
    strict = TRUE
    )

##################################################################################
#   Example 4.7 Two-sample test on means with overlapping samples
###################################################################################
n.dep.2sam(S2x=200, S2y=200, 
            g=0.75, r=1, rho=0.9, 
            alt="one.sided", del=5, 
            sig.level=0.05, pow=0.80)
            
n.dep.2sam(S2x=200, S2y=200, 
            g=0.75, r=1, rho= 0.9, 
            alt="two.sided", del=5, 
            sig.level=0.05, pow=0.80)
            
n.dep.2sam(S2x=200, S2y=200, 
            g=1, r=1, rho= 0.9, 
            alt="one.sided", del=5, 
            sig.level=0.05, pow=0.80)
            
##################################################################################
#   Example 4.11 Repeat example 4.8 with arcsin transformation
###################################################################################

p1 <- 0.15
p2 <- 0.18
alpha <- 0.05
power <- 0.80

phi1 <- asin(sqrt(p1))
phi2 <- asin(sqrt(p2))
d.phi <- phi1 - phi2
n <- ( (qnorm(1-alpha) - qnorm(1-power)) / sqrt(2) / d.phi)^2
n

### Same value of p1-p2 but smaller p's
p1 <- 0.01
p2 <- 0.04
alpha <- 0.05
power <- 0.80

phi1 <- asin(sqrt(p1))
phi2 <- asin(sqrt(p2))
d.phi <- phi1 - phi2
n <- ( (qnorm(1-alpha) - qnorm(1-power)) / sqrt(2) / d.phi)^2
n

power.prop.test(power = 0.8,
    p1 = 0.01, 
    p2 = 0.04,
    alt = "one.sided",
    sig.level = 0.05
    )
    

p1 <- 0.99
p2 <- 0.96
alpha <- 0.05
power <- 0.80

phi1 <- asin(sqrt(p1))
phi2 <- asin(sqrt(p2))
d.phi <- phi1 - phi2
n <- ( (qnorm(1-alpha) - qnorm(1-power)) / sqrt(2) / d.phi)^2
n
    
##################################################################################
#   Example 4.11 Repeat example 4.8 with log-odds transformation
###################################################################################

p1 <- 0.15
p2 <- 0.18
alpha <- 0.05
power <- 0.80

phi1 <- log(p1/(1-p1))
phi2 <- log(p2/(1-p2))
d.phi <- phi1 - phi2
p.bar <- mean(c(p1,p2))
V0 <- 1/p.bar/(1-p.bar)
VA <- 1/p1/(1-p1) + 1/p2/(1-p2)

n <- ( (qnorm(1-alpha)*sqrt(2*V0) - qnorm(1-power)*sqrt(VA)) / d.phi)^2
n
