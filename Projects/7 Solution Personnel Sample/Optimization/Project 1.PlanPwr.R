######################################################################
# Program: Project 1.PlanPwr.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    02/09/12
# Purpose: Sample size given power for Cycle 5
# Revised: 
######################################################################
                                                #Set working directory
rm(list=ls(all=TRUE))
setwd("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/7 Solution Personnel Sample/Optimization/")

######################################################################
# Cycle 5 - Sample size given 80% power and specified percent diff
#           for proportions
######################################################################
(est.matrx <- matrix(c(0.84, 0.80, 0.69, 18.10, 0.98,
                       0.90, 0.85, 0.83, 12.60, 0.90,
                       0.67, 0.63, 0.60,  8.94, 0.60), 
                    nrow=3, ncol=5, byrow=T,
                    dimnames = list(c("SR","CR","FO"),
                                    c("Q5","Q12","Q15","Mean","SE"))))


                            # Function to calculate min sizes per unit

prop.sizes <- function(min.days.diff=NULL){

  if (is.null(min.days.diff))
    min.days.diff <- 0.05

  dif.matrx <- est.matrx + min.days.diff

  SR.Q15 <- power.prop.test(p1=est.matrx[1,3], p2=dif.matrx[1,3], 
                  sig.level = 0.05, power = 0.8, alternative = "two.sided")

  CR.Q15 <- power.prop.test(p1=est.matrx[2,3], p2=dif.matrx[2,3], 
                  sig.level = 0.05, power = 0.8, alternative = "two.sided")
  
  FO.Q15 <- power.prop.test(p1=est.matrx[3,3], p2=dif.matrx[3,3], 
                  sig.level = 0.05, power = 0.8, alternative = "two.sided")

  minsz.vectr <- c(SR.Q15$n, CR.Q15$n, FO.Q15$n)

  c(min.days.diff, minsz.vectr, sum(minsz.vectr))
}

                            # Call function with declared pct diff
prop.sizes(0.05)
prop.sizes(0.10)
prop.sizes(0.13)
prop.sizes(0.15)

######################################################################
# Cycle 5 - Sample size given 80% power and specified mean diff
######################################################################

                            # Population standard deviations
(cycle4.resp.n  <- matrix(c(110,  65, 130), byrow=F))
(cycle5.frame   <- matrix(c(554, 418, 897), byrow=F))

 deff.mean      <- 1
 
(pop.std <- sqrt(cycle4.resp.n * est.matrx[,5]**2 / 
                 (1 - cycle4.resp.n / cycle5.frame) * deff.mean))


mean.sizes <- function(days.diff=NULL){

  SR.avg <- power.t.test(delta = days.diff, sd = pop.std[1], sig.level = 0.05,
                         power = 0.8, type = "one.sample", alternative = "two.sided")
  CR.avg <- power.t.test(delta = days.diff, sd = pop.std[2], sig.level = 0.05,
                         power = 0.8, type = "one.sample", alternative = "two.sided")
  FO.avg <- power.t.test(delta = days.diff, sd = pop.std[3], sig.level = 0.05,
                         power = 0.8, type = "one.sample", alternative = "two.sided")

  c(SR.avg$n, CR.avg$n, FO.avg$n, sum(SR.avg$n, CR.avg$n, FO.avg$n))
}

                            # Call function with declared mean diff
mean.sizes(1.0)
mean.sizes(1.5)
mean.sizes(2.0)
mean.sizes(2.5)

######################################################################

