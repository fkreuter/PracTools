#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\Example 17.16.R                
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     06/02/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Illustrate 2-phase srs + stsrs with nhis.large pop
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.large.RData", pos=2)
require(sampling)
require(survey)

set.seed(1716768836)

nhis <- as.data.frame(nhis.large)
n1 <- 2000
N <- nrow(nhis.large)
        # recode delay.med to be 0,1
nhis$delay.med <- abs(nhis$delay.med-2)

        # select a phase 1 sample of n1
sam <- sort(sample(1:N, n1))

p1.dat <- nhis[sort(sam), ]

        # Phase 1 weights
p1.dat$p1wts <- rep(N/n1, n1)

table(p1.dat$age.grp, useNA = "always")

n2 <- rep(100,5)
p2.str.sam <- strata(data.frame(p1.dat), stratanames = c("age.grp"), size = n2,
        method = "srswor")

p2.dat <- p1.dat[p2.str.sam$ID_unit, ]
table(p2.dat$age.grp)
        # set a T/F variable for whether person is in phase 2 sample or not
p1.dat$p2 <- FALSE
p1.dat$p2[p2.str.sam$ID_unit] <- TRUE

        # Phase 2 conditional weights
p1.dat$p2wts <- 0
p1.dat$p2wts[p2.str.sam$ID_unit] <- 1/p2.str.sam$Prob

        # 2-phase design object
d2.nhis <- twophase(id = list(~ID, ~ID),
         data = p1.dat,
         strata=list(NULL,~age.grp),
         weights = list(~p1wts, ~p2wts),
         subset = ~p2,
         method = "approx")
         
summary(d2.nhis)

mns <-svymean(~factor(delay.med), design = d2.nhis, na.rm = TRUE)
ftab <- ftable(mns, rownames=list(delay.med = c("No","Yes")))
round(ftab,4)

V<-vcov(svymean(~factor(delay.med), design = d2.nhis, na.rm = TRUE))
V
        # retrive variance compnents
V1 <- attr(V, "phases")$phase1 
V2 <- attr(V,"phases")$phase2 

V1/(V1+V2)
V2/(V1+V2)

        # by age group
age.mns <- svyby(formula= ~delay.med, by=~age.grp, FUN=svymean, design = d2.nhis, na.rm=TRUE)
round(age.mns,4)

#----------------------------------------------------------------------------------------
age.tots <- table(nhis$age.grp)
calibrate(d2.nhis, phase=2, 
          calfun="linear", 
          population=age.tots, 
          formula = ~ 0 + age.grp)

#----------------------------------------------------------------------------------------
        # Compare to a directly selected stsrs of the same units
p2.dat <- p1.dat[p1.dat$p2wts > 0, ]
p2.dat$fullwt <- p2.dat$p2wts * N/n1

sum(p2.dat$fullwt)
stsrs.dsgn <- svydesign(ids = ~0,    # no clusters
                        strata = ~age.grp,
                        data = data.frame(p2.dat),
                        weights = ~p2.dat$fullwt)

svymean(~factor(delay.med), design = stsrs.dsgn, na.rm = TRUE)

        # Compare to a directly selected srs of the same units
p2.dat <- p1.dat[p1.dat$p2wts > 0, ]
p2.dat$fullwt <- p2.dat$p2wts * N/n1

srs.dsgn <- svydesign(ids = ~0,    
                        strata = NULL,
                        data = data.frame(p2.dat),
                        weights = ~p2.dat$fullwt)

svymean(~factor(delay.med), design = srs.dsgn, na.rm = TRUE)

#----------------------------------------------------------------------------------------
        # hand checks
by(p2.dat$delay.med, INDICES=p2.dat$age.grp, mean, na.rm=TRUE)
by(p2.dat$delay.med, INDICES=p2.dat$age.grp, var, na.rm=TRUE)
