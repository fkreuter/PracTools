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
#          07/06/2017 R. Valliant
#          Generate pop with HMT() function in PracTools. Compute deff's with deffH and deffS
#           in PracTools using deff wrapper function.
#**************************************************************************

            # load sampling package
require(sampling)
require(PracTools)

            #Random seed for sample selection
set.seed(-500398777)

#--------------------------------------------------------------------------------------------------
## Generate HMT population
#--------------------------------------------------------------------------------------------------
pop.dat <- as.data.frame(HMT())
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

#--------------------------------------------------------------------------------------------------
## Calculate 1-draw selection probabilities - pps
#--------------------------------------------------------------------------------------------------

            #MOS = x
mos <- pop.dat$x
            #Calculate 1-draw selection probabilities
pop.dat$prbs.1d <- mos / sum(mos)
summary(pop.dat$prbs.1d)

#--------------------------------------------------------------------------------------------------
## Select sample - pps
#--------------------------------------------------------------------------------------------------

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

#--------------------------------------------------------------------------------------------------
## Spencer's Deff
#--------------------------------------------------------------------------------------------------

            #DEFF component - Kish
kish.deff <- deffK(dsgn.wts)
kish.deff
#[1] 1.882999
            #Spencer's DEFF
spencers.deff <- deffS(p=sam.dat$prbs.1d, w=dsgn.wts, y=sam.dat$y)
spencers.deff
#[1] 0.2333836

    # alternatively, call the wrapper fcn deff()
deff(w=dsgn.wts, type="kish")
deff(w=dsgn.wts, y=sam.dat$y, p=sam.dat$prbs.1d, type="spencer")

#--------------------------------------------------------------------------------------------------
## Henry Deff
#--------------------------------------------------------------------------------------------------
deff(w=dsgn.wts, x=sam.dat$x, y=sam.dat$y, type="henry")
#[1] 0.5557366
