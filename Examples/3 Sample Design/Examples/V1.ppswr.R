#****************************************************************************
# FILE:    V1.ppswr.R                                                        
# PROJECT: Practical Tools book                                              
# DATE:    12/14/09                                                          
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Compute unit variance estimate V1 for ppswr sampling. Account for 
#          change in MOS for a new sample. Select pps sample from the        
#          hospitals population and estimate V1 under pp(beds) and           
#          under pp(sqrt(beds)).
# REVISED: 
#****************************************************************************

attach("C:/Projects/Populations/hospital.RData", pos=2)
attach("C:/JPSM/SURV 699S-Prediction Theory/R/.RData", pos=3)

                # store random nos. do once only
#   save.seed <- .Random.se
#   set.seed(save.seed[100]
#   save.seed[100]
#   [1] -1933515768    

#   set.seed(-1933515768)  This one gives negative V1 for sqrt(x) sampling.
#   So, estimator does not work generally
#   Seed below gives sam <- c(190, 207, 272, 303, 308, 324, 345, 354, 373, 379)
#       used in book.

#___________________________________________________________________________________
#   pps samples of size n=10
#___________________________________________________________________________________
   set.seed(1492168107)
   n <- 10
   N <- nrow(hospital)
   sam <- pps.random.fcn(x = hospital[,"x"], n = n)

#n <- 10
#sam <- c(190, 207, 272, 303, 308, 324, 345, 354, 373, 379)

y <- hospital[sam, "y"]
x <- hospital[sam, "x"]
pop.x <- hospital[, "x"]

                # calculate V1 based on pp(x) sample
pi <- pop.x/sum(pop.x)
wi <- 1/(n*pi)
t.hat <- sum(y/pi[sam]/n)
ybar.pwr <- t.hat/N
V1 <- sum((y/pi[sam] - t.hat)^2)/(n-1)
V1

v.ybar.pwr <- V1/N^2/n

                # alternative ... (same answer as V1)
V1.alt <- n^2/(n-1)*sum((wi[sam]*y - t.hat/n)^2)
V1.alt

CV.t <- sqrt(V1/n)/t.hat
CV.t

                # calculate new V1 based on pp(rtx) sample
qi <- sqrt(pop.x)/sum(sqrt(pop.x))
V1 <- sum(y^2/pi[sam]/qi[sam]/n) - t.hat^2 + v.ybar.pwr
V1
CV.t <- sqrt(V1/n)/t.hat
CV.t

                # table values
cbind(sam, y, x, n*pi[sam], wi[sam], (wi[sam]*y - t.hat/n)^2)

#___________________________________________________________________________________
#   pps samples of size n=20
#   This version used for book, 12/14/09
#___________________________________________________________________________________

    set.seed(-428765274)
   n <- 20
   N <- nrow(hospital)
   sam <- pps.random.fcn(x = hospital[,"x"], n = n)

#   > sam
#    [1] 109 126 186 221 233 296 301 323 334 347 364 365 368 372 376 382 383 385 387 391

y <- hospital[sam, "y"]
x <- hospital[sam, "x"]
pop.x <- hospital[, "x"]

                # calculate V1 based on pp(x) sample
pi <- pop.x/sum(pop.x)
wi <- 1/(n*pi)
t.hat <- sum(y/pi[sam]/n)
ybar.pwr <- t.hat/N
ybar.pwr 

V1 <- sum((y/pi[sam] - t.hat)^2)/(n-1)
V1

v.ybar.pwr <- V1/N^2/n

CV.t <- sqrt(V1/n)/t.hat
CV.t

                # calculate new V1 based on pp(rtx) sample
qi <- sqrt(pop.x)/sum(sqrt(pop.x))
V1 <- sum(y^2/pi[sam]/qi[sam]/n) - t.hat^2 + v.ybar.pwr
V1
CV.t <- sqrt(V1/n)/t.hat
CV.t

            # pieces of V1
A <- sum(sqrt(pop.x))
B <- sum(y^2/sqrt(x)/pi[sam])
A/20*B - t.hat^2
                # table values
cbind(sam, y, x, n*pi[sam], wi[sam], (wi[sam]*y - t.hat/n)^2)

#___________________________________________________________________________________
#   srs samples of size n=20 using the same sample above for pps.
#   This version used for book, 12/14/09
#___________________________________________________________________________________

S2 <- sum(wi[sam]*(y - ybar.pwr)^2)
S2 <- n / (n-1) * S2 / (sum(wi[sam])-1)
S2

cv.srs <- sqrt((1-n/N) * S2/n) / ybar.pwr
cv.srs
