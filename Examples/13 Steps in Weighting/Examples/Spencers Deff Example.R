##################################################################
## Program: Spencers Deff Example.R
## Name:    J.Dever
## Project: SURV699E - A Practical Course in Sampling and Weighting
## Date:    11/09/07
## Purpose: Calculate Spencer's Deff and compare with 1 + relvar(wts),
##          Kish's approximation using Hansen, Madow, & Tepping data.
## Ref:     Spencer, Bruce D. (2000). An Approximate Design Effect 
##          for Unequal Weighting when Measurements May Correlate 
##          with Selection Probabilities.  Survey Methodology, 26, 
##          2, pp. 137-138.
## Revised: 
##################################################################
                                                  #Set working directory
rm(list=ls(all=TRUE))
setwd("//Jpsmnds/Users/Share/Surv 699-wting case studies/Lecture Notes/6 Steps in Weighting/Examples/")

                                                  #Load R libraries 
require(MASS)
require(rpart) 
require(foreign)
                                                  #Load R functions
source("//Jpsmnds/Users/Share/Surv 699-wting case studies/R/pps.functions.txt")
source("//Jpsmnds/Users/Share/Surv 699-wting case studies/Lecture Notes/6 Steps in Weighting/Examples/HMT.fcn")

                                                  #Random seed for sample selection
set.seed(-500398777)

##################################################################
## Generate HMT population
##################################################################
pop.dat <- as.data.frame(HMT.fcn())
                                                  #Examine population
dim(pop.dat)
pop.dat[1:5,]
table(pop.dat$strat)
                                                  #Population size
N <- nrow(pop.dat)

#############################################################################
## Calculate selection probabilities - pps to sqrt(x)
#############################################################################

                                                  #MOS = sqrt(x)
pop.dat$x.sqrt <- sqrt(pop.dat$x)
                                                  #Calculate 1-draw selection probabilities
pop.dat$prbs.1d <- pop.dat$x.sqrt / sum(pop.dat$x.sqrt)
summary(pop.dat$prbs.1d)

#############################################################################
## Select sample - pps to sqrt(x)
#############################################################################

                                                  #Define size of sample 
n.smp <- 80
                                                  #PPS sample
sam <- pps.random.fcn(pop.dat$x.sqrt, n.smp)
sam.dat <- pop.dat[sam, ]
                                                  #Examine sample
dim(sam.dat)
table(sam.dat$strat)
                                                  #Design weights
dsgn.wts <- sum(pop.dat$x.sqrt) / (n.smp * pop.dat[sam,"x.sqrt"])
sum(dsgn.wts)
summary(dsgn.wts)

#############################################################################
## Spencer's Deff (equation 6)
#############################################################################

                                                  #Calculate OLS values
sam.ols <- lm(y ~ prbs.1d, data = sam.dat)
summary(sam.ols)

                                                  #DEFF component - var of y
sam.mean.y <- sum(sam.dat$y * dsgn.wts) / sum(dsgn.wts)
sam.mean.y
sam.var.y  <- sum(dsgn.wts * (sam.dat$y - sam.mean.y)^2) / sum(dsgn.wts)
sam.var.y
                                                  #DEFF component - alpha squared
sam.alpha2 <- sam.ols$coefficients[1] ^2
sam.alpha2
                                                  #DEFF component - squared correlation
sam.rho2.yP <- cor(sam.dat$y, sam.dat$prbs.1d)^2
sam.rho2.yP
                                                  #DEFF component - unequal wting effect
sam.uwe <- (sum(dsgn.wts^2) / n.smp) / (mean(dsgn.wts)^2)
sam.uwe

                                                  #Spencer's DEFF
spencers.deff <- as.numeric((1 - sam.rho2.yP) * sam.uwe + 
                            (sam.alpha2 / sam.var.y) * (sam.uwe - 1))
spencers.deff

#############################################################################
## Spencer's Theoretical Deff (equation 4)
#############################################################################

                                                  #Calculate OLS values
pop.ols <- lm(y ~ prbs.1d, data = pop.dat)
summary(pop.ols)

pop.alpha2 <- pop.ols$coefficients[1] ^2
pop.alpha2

pop.rho2.yP <- cor(pop.dat$y, pop.dat$prbs.1d)^2
pop.rho2.yP

W.bar <- mean(1/pop.dat$prbs.1d) / n.smp

sigma2.y <- var(pop.dat$y)

spencers.tdeff <- as.numeric((1 - pop.rho2.yP) * n.smp * W.bar / N + 
                            (pop.alpha2 / sigma2.y) * (n.smp * W.bar / N - 1))
spencers.tdeff

#############################################################################
## Weight Check
#############################################################################

                                                  #Ratio of pop variances
Var.srs <- var(pop.dat$y) * N^2 / n.smp

Var.pps <- sum(pop.dat$prbs.1d*((pop.dat$y / pop.dat$prbs.1d) - 
               sum(pop.dat$y))^2) / n.smp
Var.pps / Var.srs

                                                  #Ratio of sample variances
vhat.srs <- sam.var.y  * N^2 / n.smp

vhat.pps <- sum((sam.dat$y / sam.dat$prbs.1d - 
                sum(sam.dat$y * dsgn.wts))^2) / (n.smp * (n.smp - 1))
vhat.pps / vhat.srs

##################################################################
