######################################################################
# Program: Project 1.PostPwr.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    02/15/12
# Purpose: Determine power given allocation sample size adjusted for
#          lower response rates
# Revised: 
######################################################################
                                                #Set working directory
rm(list=ls(all=TRUE))
setwd("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/7 Solution Personnel Sample/Optimization/")


######################################################################
# Cycle 5 - Detectable difference given smaller sample sizes associated
#           with higher nonresponse rates (proportions)
######################################################################

                            # Population standard deviations
cycle4.se    <- matrix(c(.98, .90, .60), byrow=F)
cycle4.resp  <- matrix(c(110,  65, 130), byrow=F)
cycle5.frame <- matrix(c(554, 418, 897), byrow=F)

pop.std <- sqrt(cycle4.resp * cycle4.se**2 / 
                 (1 - cycle4.resp / cycle5.frame))

est.delta <- function(n.smp=2){

   SR.Q15 <- power.prop.test(n = n.smp, p1=0.69, sig.level = 0.05, 
                             power = 0.8, alternative = "two.sided")   
   SR.avg <- power.t.test(n = n.smp, sd = pop.std[1], sig.level = 0.05, 
             power = 0.8, type = "one.sample", alternative = "two.sided")

   c("p1"=SR.Q15$p1, "p2"=SR.Q15$p2, "diff"=SR.Q15$p2 - SR.Q15$p1, "delta.avg"=SR.avg$delta)
}

est.delta(n = 177.4)          #  1.0 percent reduction
est.delta(n = 174.7)          #  2.5 percent reduction  
est.delta(n = 170.2)          #  5.0 percent reduction
est.delta(n = 165.7)          #  7.5 percent reduction
est.delta(n = 161.2)          # 10.0 percent reduction

######################################################################
