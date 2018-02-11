#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.11.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--difference in proportions with arcsine and log-odds
#*********************************************************************************************************

p1 <- 0.15
p2 <- 0.18
alpha <- 0.05

    # arcsince sqrt
power <- 0.80
phi1 <- asin(sqrt(p1))
phi2 <- asin(sqrt(p2))
d.phi <- phi1 - phi2
n <- ((qnorm(1-alpha) - qnorm(1-power)) / sqrt(2) / d.phi)^2
n
[1] 1889.337

    # log-odds
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
[1] 1888.571
