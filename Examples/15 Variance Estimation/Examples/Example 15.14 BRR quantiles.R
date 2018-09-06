#*****************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Variance Estimation\        
#                   Examples\Example 15.14 BRR quantiles.R                                
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                     
# DATE:     05/27/2011                                                                    
# AUTHOR:   R. Valliant                                                                   
# PURPOSE: Example 15.14 to illustrate estimating SEs & CIs for quantiles                 
#*****************************************************************************************

require(survey)
require(sampling)

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
sam <- strata(data = pop, 
              stratanames = "strat", 
              size = rep(2,H), method=c("srswor"))

sam.dat <- pop[sam$ID_unit,]
d <- 1/sam$Prob

smho.dsgn <- svydesign(ids = ~0,
                       strata = ~strat,
                       data = sam.dat,
                       fpc = sam$Prob,
                       weights = ~d)
                       
smho.BRR.dsgn <- as.svrepdesign(design = smho.dsgn,
                    type = "BRR")
smho.FayBRR.dsgn <- as.svrepdesign(design = smho.dsgn,
                    type = "Fay",
                    fay.rho = 0.3)

svyquantile(~EXPTOTAL, design = smho.BRR.dsgn, quantile=0.5, interval.type="quantile")
svyquantile(~EXPTOTAL, design = smho.FayBRR.dsgn, quantile=0.5, interval.type="quantile")
