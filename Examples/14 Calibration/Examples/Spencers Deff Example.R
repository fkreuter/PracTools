#**************************************************************************
# Program: Spencers Deff Example.R
# Name:    J.Dever
# Topic:   Practical Tools for Designing and Weighting Sample Surveys
# Project: SURV699E - A Practical Course in Sampling and Weighting
# Date:    11/09/07
# Purpose: Calculate Spencer's Deff and compare with 1 + relvar(wts),
#          Kish's approximation using Hansen, Madow, & Tepping data.
# Ref:     Spencer, Bruce D. (2000). An Approximate Design Effect 
#          for Unequal Weighting when Measurements May Correlate 
#          with Selection Probabilities.  Survey Methodology, 26, 
#          2, pp. 137-138.
# Revised: 07/26/2010 R. Valliant
#          Select sample using the sampling package.
#**************************************************************************

            # load sampling package
require(sampling)
            # Load function to generate HMT pop
source("C:\\Projects\\Practical Tools Book\\Book Chapters\\15 Calibration\\Examples\\HMT.fcn.R")

            #Random seed for sample selection
set.seed(-500398777)

#############################################################################
## Generate HMT population
#############################################################################
pop.dat <- as.data.frame(HMT.fcn())
            #Examine population
dim(pop.dat)
pop.dat[1:5,]
table(pop.dat$strat)
            #Population size
N <- nrow(pop.dat)
subsam <- sample(1:N, 500)
with(pop.dat, 
     plot(x[subsam], y[subsam], 
     xlab = "x", ylab = "y",
     pch=16))

#############################################################################
## Calculate 1-draw selection probabilities - pps 
#############################################################################

            #MOS = x
mos <- pop.dat$x
            #Calculate 1-draw selection probabilities
pop.dat$prbs.1d <- mos / sum(mos)
summary(pop.dat$prbs.1d)

#############################################################################
## Select sample - pps 
#############################################################################

            #Define size of sample 
n <- 80
            # probabilities for selecting a sample of n
pk <- n * pop.dat$prbs.1d
            #PPS sample
sam <- UPrandomsystematic(pk)
sam <- sam==1
sam.dat <- pop.dat[sam, ]
            #Design weights
dsgn.wts <- 1/pk[sam]
sum(dsgn.wts)
summary(dsgn.wts)

#############################################################################
## Spencer's Deff
#############################################################################

            #Calculate WLS values
sam.wls <- lm(y ~ prbs.1d, data = sam.dat, weights = dsgn.wts)
summary(sam.wls)
            #DEFF component - var of y
sam.mean.y <- sum(sam.dat$y * dsgn.wts) / sum(dsgn.wts)
sam.mean.y
sam.var.y  <- sum(dsgn.wts * (sam.dat$y - sam.mean.y)^2) / sum(dsgn.wts)
sam.var.y
            #DEFF component - alpha squared
sam.alpha2 <- sam.wls$coefficients[1] ^2
sam.alpha2
            #DEFF component - squared correlation
sam.rho2.yP <- summary(sam.wls)$r.squared
sam.rho2.yP
            #DEFF component - Kish
kish.deff <- n*sum(dsgn.wts^2) / (sum(dsgn.wts)^2)
kish.deff
            #Spencer's DEFF
spencers.deff <- as.numeric((1 - sam.rho2.yP) * kish.deff + 
                            (sam.alpha2 / sam.var.y) * (kish.deff - 1))
spencers.deff

#############################################################################
## Spencer's Theoretical Deff (equation 4 in paper)
#############################################################################

            #Calculate OLS values
pop.ols <- lm(y ~ prbs.1d, data = pop.dat)
summary(pop.ols)

pop.alpha2 <- pop.ols$coefficients[1] ^2
pop.alpha2

pop.rho2.yP <- cor(pop.dat$y, pop.dat$prbs.1d)^2
pop.rho2.yP

W.bar <- mean(1/pop.dat$prbs.1d) / n

sigma2.y <- var(pop.dat$y)

spencers.tdeff <- as.numeric((1 - pop.rho2.yP) * n * W.bar / N + 
                            (pop.alpha2 / sigma2.y) * (n * W.bar / N - 1))
spencers.tdeff

#############################################################################
## Weight Check
#############################################################################

            #Ratio of pop variances
Var.srs <- var(pop.dat$y) * N^2 / n

Var.pps <- sum(pop.dat$prbs.1d*((pop.dat$y / pop.dat$prbs.1d) - 
               sum(pop.dat$y))^2) / n
Var.pps / Var.srs

            #Ratio of sample variances
vhat.srs <- sam.var.y  * N^2 / n

vhat.pps <- sum((sam.dat$y / sam.dat$prbs.1d - 
                sum(sam.dat$y * dsgn.wts))^2) / (n * (n - 1))
vhat.pps / vhat.srs
