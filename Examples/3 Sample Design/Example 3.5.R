######################################################################
# Program: Example 3.5.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    07/10/10
# Purpose: Produce results for Example 3.5
# Revised: 
######################################################################
                                                #Set working directory
rm(list=ls(all=TRUE))
setwd("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/")

                                            #Load sample size function
source("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/n.prop.fcn")

                                                 #Common CV constraint
CVmax_=0.05

          #Sample size for Business Unit=SR by Salary Grade, Large pop
SR.ARMs.smp <- n.prop(CV0=CVmax_, N=Inf, pU=c(0.82, 0.81, 0.92))
c(SR.ARMs.smp, sum(SR.ARMs.smp))

                     #Sample size for Business Unit=SR by Salary Grade
SR.As.smp <- n.prop(CV0=CVmax_, N=68,  pU=0.82)
SR.Rs.smp <- n.prop(CV0=CVmax_, N=326, pU=0.81)
SR.Ms.smp <- n.prop(CV0=CVmax_, N=110, pU=0.92)
c(SR.As.smp, SR.Rs.smp, SR.Ms.smp, sum(SR.As.smp, SR.Rs.smp, SR.Ms.smp))

                                                #Revised CV constraint
CVmax_=0.10
                     #Sample size for Business Unit=SR by Salary Grade
SR.As.smp <- n.prop(CV0=CVmax_, N=68,  pU=0.82)
SR.Rs.smp <- n.prop(CV0=CVmax_, N=326, pU=0.81)
SR.Ms.smp <- n.prop(CV0=CVmax_, N=110, pU=0.92)
c(SR.As.smp, SR.Rs.smp, SR.Ms.smp, sum(SR.As.smp, SR.Rs.smp, SR.Ms.smp))
