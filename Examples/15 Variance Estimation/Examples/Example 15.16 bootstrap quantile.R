#*****************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Variance Estimation\        
#                   Examples\Example 15.16 bootstrap quantiles.R                          
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                     
# DATE:     05/29/2011                                                                    
# AUTHOR:   R. Valliant                                                                   
# PURPOSE: Example 15.16 to illustrate estimating SEs & CIs for quantiles                 
#*****************************************************************************************

require(survey)
require(sampling)
require(MASS)
require(PracTools)

data(smho.N874)

x <- smho.N874$BEDS
x[x <= 10] <- 10
x <- sqrt(x)
smho.N874 <- smho.N874[order(x), ]
x <- sort(x)

N <- nrow(smho.N874)
n <- 50
H <- 25

cumx <- cumsum(x)
size <- cumx[N]/H
brks <- (0:H)*size
strat <- cut(cumx, breaks = brks, labels = 1:H)

pop <- data.frame(smho.N874, strat = strat)
set.seed(428274453)
sam <- strata(data = pop, #stratanames = "hosp.type", 
              stratanames = "strat", 
              size = rep(2,H), method=c("srswor"))

sam.dat <- pop[sam$ID_unit,]
d <- 1/sam$Prob

smho.dsgn <- svydesign(ids = ~0,
                       strata = ~strat,
                       data = sam.dat,
                       fpc = sam$Prob,
                       weights = ~d)                       

smho.BRR <- as.svrepdesign(design = smho.dsgn,
                    type = "BRR")
smho.FayBRR <- as.svrepdesign(design = smho.dsgn,
                    type = "Fay",
                    fay.rho = 0.3)
smho.boot <- as.svrepdesign(design = smho.dsgn,
                    type = "subbootstrap",
                    replicates = 500)

a1 <- svyquantile(~EXPTOTAL, design = smho.BRR, quantile=0.5, interval.type="quantile")
a2 <- svyquantile(~EXPTOTAL, design = smho.FayBRR, quantile=0.5, interval.type="quantile")
a3 <- svyquantile(~EXPTOTAL, design = smho.boot, quantile=0.5, interval.type="quantile",
                return.replicates = TRUE)

         # t approximation with BRR
La1 <- a1 + qt(0.025,df=degf(smho.BRR))*SE(a1)
Ua1 <- a1 + qt(0.975,df=degf(smho.BRR))*SE(a1)

         # t approximation with Fay.BRR
La2 <- a2 + qt(0.025,df=degf(smho.FayBRR))*SE(a2)
Ua2 <- a2 + qt(0.975,df=degf(smho.FayBRR))*SE(a2)

         # t approximation with v.boot
La3 <- a3$mean + qt(0.025,df=degf(smho.boot))*sd(a3$replicates)
Ua3 <- a3$mean + qt(0.975,df=degf(smho.boot))*sd(a3$replicates)

par(mfrow=c(1,1))
truehist(a3$replicates/10^3, nbins=25, xlab = "", col = "gray85")
abline(v = a3$mean/10^3, col = "gray50")
ta3 <- quantile(a3$replicates, c(0.025, 0.975))
ta3

        # pop median
median(pop$EXPTOTAL)
        # CI's on median
rbind(c(La1[1], Ua1[1]),
      c(La2[1], Ua2[1]),
      c(La3[1], Ua3[1]),
      ta3)
