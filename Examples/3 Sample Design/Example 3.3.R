######################################################################
# Program: Example 3.3.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    07/09/10
# Purpose: Produce results for Example 3.3
# Revised: 
######################################################################
                                                #Set working directory
rm(list=ls(all=TRUE))
setwd("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/")

                                            #Load sample size function
source("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/n.cont.fcn")

Nh <- c(68, 326, 110)
Npop <- sum(Nh)
nh.old <- c(22, 106, 36)
n.old <- sum(nh.old)
cv.old <- c(0.061, 0.096, 0.045)
cv.SR <- 0.054
        # estimate unit CV from last survey
CVpoph <- cv.old/sqrt((1/nh.old - 1/Nh))
CVpop_ <- cv.SR/sqrt(1/n.old - 1/N)

        # salary grade samples    
n.cont(CV0=0.05, CVpop = CVpoph, N=Nh)
        # SR business unit sample
n.cont(CV0=0.05, CVpop = CVpop_, N=Npop)


                                                 #Common population CV
CVpop_=0.5

                           #Sample size for Business Unit=SR (overall)
SR.smp    <- n.cont(CV0=0.05, CVpop=CVpop_, N=504)
SR.smp

                     #Sample size for Business Unit=SR by Salary Grade
SR.As.smp <- n.cont(CV0=0.05, CVpop=CVpop_, N= 68)
SR.Rs.smp <- n.cont(CV0=0.05, CVpop=CVpop_, N=326)
SR.Ms.smp <- n.cont(CV0=0.05, CVpop=CVpop_, N=110)

SR.SG.smp <- sum(SR.As.smp, SR.Rs.smp, SR.Ms.smp)
c(SR.SG.smp, SR.As.smp, SR.Rs.smp, SR.Ms.smp)

                                 #Compare overall and by-grade results
c(SR.smp, SR.SG.smp, SR.SG.smp - SR.smp)

                                                      #Decrease pop CV
CVpop_=0.25
n.cont(CV0=0.05, CVpop=CVpop_, N=504)

                                                      #Increase pop CV
CVpop_=0.75
n.cont(CV0=0.05, CVpop=CVpop_, N=504)
