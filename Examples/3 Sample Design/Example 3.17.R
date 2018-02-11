#*********************************************************************************************************
# FILE: C:\Projects\Practical Tools Book\Book Chapters\3 Sample Design\Examples\Example 3.17.R            
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                                     
# DATE:    08/20/2012
# AUTHOR:  R. Valliant                                                                                    
# PURPOSE: Estimating unit variance for srswor sampling
#*********************************************************************************************************


attach("C:/Projects/Populations/hospital.RData", pos=2)

N <- nrow(hospital)

sam <- c(76,155,192,200,228,243,253,289,297,315,320,321,329,354,360,369,373,376,378,381)
n <- length(sam)

y <- hospital[sam, "y"]
x <- hospital[sam, "x"]
pop.x <- hospital[, "x"]

	# calculate S^2 based on pps(x) sample
pi <- pop.x/sum(pop.x)
wi <- 1/(n*pi)

t.hat <- sum(y/pi[sam]/n)
ybar.pwr <- t.hat/N
ybar.pwr
[1] 813.0917

N.hat <- sum(wi[sam])
N.hat
[1] 342.478


S2 <- n/(n-1) * sum(wi[sam]* (y - ybar.pwr)^2) / (N.hat - 1)
S2
[1] 414145.8

	# Anticipated CV
CV.t <- sqrt( (1 - n/N) * S2/n) / ybar.pwr
CV.t
[1] 0.1724171

