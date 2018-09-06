######################################################################
# Program: Example 3.3.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    07/09/10
# Purpose: Produce results for Example 3.3
# Revised: 01/06/2018 by R. Valliant to use nCont in PracTools
#           Correct pop and sample counts
######################################################################
                                                #Set working directory
rm(list=ls(all=TRUE))
#setwd("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/")

                                            #Load sample size function
#source("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/n.cont.fcn")
require(PracTools)

Nh <- c(74, 359, 121)
Npop <- sum(Nh)
nh.old <- c(21, 105, 36)
n.old <- sum(nh.old)
cv.old <- c(0.061, 0.096, 0.045)
cv.SR <- 0.054
        # estimate unit CV from last survey
(CVpoph <- cv.old/sqrt((1/nh.old - 1/Nh)))
#[1] 0.3303067 1.1694893 0.3221417
(CVpop_ <- cv.SR/sqrt(1/n.old - 1/Npop))
#[1] 0.8170772


        # salary grade samples
nCont(CV0=0.05, CVpop = CVpoph, N=Nh)
        # SR business unit sample
nCont(CV0=0.05, CVpop = CVpop_, N=Npop)


                                                 #Common population CV
CVpop_=0.5

                           #Sample size for Business Unit=SR (overall)
SR.smp    <- nCont(CV0=0.05, CVpop=CVpop_, N=N)
SR.smp

                     #Sample size for Business Unit=SR by Salary Grade
SR.As.smp <- nCont(CV0=0.05, CVpop=CVpop_, N= Nh[1])
SR.Rs.smp <- nCont(CV0=0.05, CVpop=CVpop_, N=Nh[2])
SR.Ms.smp <- nCont(CV0=0.05, CVpop=CVpop_, N=Nh[3])

SR.SG.smp <- sum(SR.As.smp, SR.Rs.smp, SR.Ms.smp)
c(SR.SG.smp, SR.As.smp, SR.Rs.smp, SR.Ms.smp)

                                 #Compare overall and by-grade results
c(SR.smp, SR.SG.smp, SR.SG.smp - SR.smp)

                                                      #Decrease pop CV
CVpop_=0.25
nCont(CV0=0.05, CVpop=CVpop_, N=N)

                                                      #Increase pop CV
CVpop_=0.75
nCont(CV0=0.05, CVpop=CVpop_, N=N)

                                # Use IRS bound
nCont(CV0=0.10/1.645, CVpop=1)
#[1] 270.6025
