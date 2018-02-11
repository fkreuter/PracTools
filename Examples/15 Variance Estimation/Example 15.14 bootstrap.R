#*****************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\        
#                   Examples\Example 16.13 bootstrap.R                                    
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                     
# DATE:     05/27/2011                                                                    
# AUTHOR:   R. Valliant                                                                   
# PURPOSE: Example 16.13 to illustrate estimating SEs & CIs with Rao-Wu bootstrap         
#*****************************************************************************************

require(survey)
require(sampling)
require(MASS)

attach("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.RData",pos=2)

x <- smho.N874$BEDS
x[x <= 10] <- 10
x <- sqrt(x)
smho.N874 <- smho.N874[order(x), ]
x <- sort(x)

N <- nrow(smho.N874)
n <- 50
H <- n/2

cumx <- cumsum(x)
size <- cumx[N]/H
brks <- (0:H)*size
strat <- cut(cumx, breaks = brks, labels = 1:H)

pop <- data.frame(smho.N874, strat = strat)

        # stsrswor from strata based on a measure of size
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

        # create design with boostrap wts. 
        # Rao-Wu version used with mh = nh-1
smho.boota <- as.svrepdesign(design = smho.dsgn, 
                             type = "subbootstrap",
                             replicates = 500)

        # mean & CI for EOYCNT based on RW bootstrap
a1 <- svytotal(~EOYCNT, design = smho.boota, 
        na.rm=TRUE,
        return.replicates = TRUE)
a1
        # proportions in hospital types
#a2 <- svymean(~as.factor(hosp.type), design = smho.boota, 
#        na.rm=TRUE,
#        return.replicates = TRUE)
#a2
        # histogram for hosp.type=1
#truehist(a2$replicates[,1], nbins=25)
#apply(a2$replicates, 2, function(x) quantile(x,c(0.025, 0.975)))

         
             # Compute CI based on bootstrap percentile method.  
ta1 <- quantile(a1$replicates, c(0.025, 0.975))
ta1

         # t approximation with v.boot
La <- a1$mean + qt(0.025,df=degf(smho.boota))*sd(a1$replicates)
Ua <- a1$mean + qt(0.975,df=degf(smho.boota))*sd(a1$replicates)
c(La[1], Ua[1])
        # compare to confint which uses normal approximation
confint(a1)

#**********************************************************************
        # srswor of same size as above

set.seed(-2032540444)
sam <- sample(1:N, n)
sam.dat <- pop[sam,]
d <- rep(N/n,n)

smho.dsgn <- svydesign(ids = ~0,
                       data = sam.dat,
                       weights = ~d)

smho.bootb <- as.svrepdesign(design = smho.dsgn, 
                             type = "subbootstrap",
                             replicates = 500)

b1 <- svytotal(~EOYCNT, design = smho.bootb, 
        na.rm=TRUE,
        return.replicates = TRUE)
b1

             # Compute CI based on bootstrap percentile method.  
tb1 <- quantile(b1$replicates, c(0.025, 0.975))
tb1

         # t approximation with v.boot
Lb <- b1$mean + qt(0.025,df=degf(smho.bootb))*sd(b1$replicates)
Ub <- b1$mean + qt(0.975,df=degf(smho.bootb))*sd(b1$replicates)
c(Lb[1], Ub[1])

        # pop total
sum(pop$EOYCNT)
        # totals & SEs
rbind(c(a1$mean, SE=SE(a1)),
      c(b1$mean, SE=SE(b1)))
        # CIs
rbind("stsrswor boot" = ta1,
      "stsrswor t CI" = c(La[1], Ua[1]),
      "srswor boot" = tb1,
      "srswor t CI" = c(Lb[1], Ub[1]))
 
            # xxx$replicates contains bootstrap estimates
par(mfrow = c(2,1),
    mar = c(3,3,1,1))
r <- range(a1$replicates/10^3, b1$replicates/10^3)
truehist(a1$replicates/10^3, nbins=25,
         xlim = r, col = "gray85")
abline(v = a1$mean/10^3, col="gray50")
title(paste("stsrswor, n =",n), cex.main = 1)
truehist(b1$replicates/10^3, nbins=25,
         xlim = r, col = "gray85")
title(paste("srswor, n =",n), cex.main = 1)
abline(v = b1$mean/10^3, col="gray50")
