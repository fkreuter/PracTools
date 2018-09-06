######################################################################
# Program: Example 3.5.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    07/10/10
# Purpose: Produce results for Example 3.5
# Revised: 02/14/2018
######################################################################

require(PracTools)
                                                 #Common CV constraint
CVmax_=0.06

          #Sample size for Business Unit=SR by Salary Grade, Large pop
SR.ARMs.smp <- nProp(CV0=CVmax_, N=Inf, pU=c(0.82, 0.81, 0.92))

                     #Sample size for Business Unit=SR by Salary Grade
SR.As.smp <- nProp(CV0=CVmax_, N=74,  pU=0.82)
SR.Rs.smp <- nProp(CV0=CVmax_, N=359, pU=0.81)
SR.Ms.smp <- nProp(CV0=CVmax_, N=121, pU=0.92)
cbind(c(SR.ARMs.smp, sum(SR.ARMs.smp)),
      c(SR.As.smp, SR.Rs.smp, SR.Ms.smp, sum(SR.As.smp, SR.Rs.smp, SR.Ms.smp))
      )

                                                #Revised CV constraint
CVmax_=0.10
                     #Sample size for Business Unit=SR by Salary Grade
SR.As.smp <- nProp(CV0=CVmax_, N=74,  pU=0.82)
SR.Rs.smp <- nProp(CV0=CVmax_, N=359, pU=0.81)
SR.Ms.smp <- nProp(CV0=CVmax_, N=121, pU=0.92)

cbind(c(SR.ARMs.smp, sum(SR.ARMs.smp)),
      c(SR.As.smp, SR.Rs.smp, SR.Ms.smp, sum(SR.As.smp, SR.Rs.smp, SR.Ms.smp))
      )
