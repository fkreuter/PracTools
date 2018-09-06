#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\Example 17.13.R                
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     06/02/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Illustrate 2-phase srs + stsrs & calibration with nhis.large pop
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

        # Phase 1 weights and selection probs
p1.dat$p1wts <- rep(N/n1, n1)
p1.dat$p1probs <- rep(n1/N, n1)

srs.dsgn <- svydesign(id = ~1, strata = NULL, weights = ~p1wts, data = p1.dat)
svytable(design=srs.dsgn, ~age.grp)

table(p1.dat$age.grp, useNA = "always")

n2 <- rep(100,5)
p2.str.sam <- strata(data.frame(p1.dat), stratanames = c("age.grp"), size = n2,
                     method = "srswor")

p2.dat <- p1.dat[p2.str.sam$ID_unit, ]
table(p2.dat$age.grp)
        # set a T/F variable for whether person is in phase 2 sample or not
p1.dat$p2 <- FALSE
p1.dat$p2[p2.str.sam$ID_unit] <- TRUE

        # Phase 2 conditional weights and selection probs
p1.dat$p2wts <- 0
p1.dat$p2probs <- 0
p1.dat$p2wts[p2.str.sam$ID_unit] <- 1/p2.str.sam$Prob
p1.dat$p2probs[p2.str.sam$ID_unit] <- p2.str.sam$Prob


        # method = "full" is needed to do calibration
d2.nhis <- twophase(id = list(~ID, ~ID),
                    data = p1.dat,
                    strata=list(NULL,~age.grp),
                    probs = list(~ p1probs, ~ p2probs),
                    subset = ~p2,
                    method = "full")

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
age.tots

sex.tots <- table(nhis$sex)
sex.tots
pop.tots <- c(age.tots[-1], sex.tots)
pop.tots

pop.tots <- table(nhis$age.grp, nhis$sex)
pop.tots <- as.vector(pop.tots)
pop.tots

svytable(design = d2.nhis, ~age.grp)

        # without population
d2.cal <- calibrate(d2.nhis, phase=2, 
                    calfun="linear", 
                    formula = ~ factor(age.grp) - 1)
svytable(design = d2.cal, ~factor(age.grp))

        # with population
d2.cal <- calibrate(d2.nhis, phase=2,
                    calfun="linear", 
                    population = age.tots, 
                    formula = ~ factor(age.grp) - 1)
svytable(design = d2.cal, ~factor(age.grp))





d2.cal <- calibrate(d2.nhis, phase=2, 
          calfun="raking", 
          population = pop.tots, 
#          formula = ~ factor(sex):factor(age.grp) - 1)
          formula = ~ factor(age.grp) + factor(sex) - 1)
#calibrate(d2.nhis, phase=2, 
#          calfun="linear", 
#          population = c(5991, 2014, 6124, 5011, 2448), 
#          formula = ~ age.grp - 1)
#d2.cal <- calibrate(d2.nhis, phase=2, 
#          calfun="linear", 
#          population = c(5991, 2014, 6124, 5011, 2448), 
#          formula = ~ factor(age.grp) - 1)


svytotal(design = d2.cal, ~factor(age.grp))
svytotal(design = d2.cal, ~factor(sex))
svytotal(design = d2.cal, ~factor(age.grp):factor(sex))



mns <-svymean(~factor(delay.med), design = d2.cal, na.rm = TRUE)
ftab <- ftable(mns, rownames=list(delay.med = c("No","Yes")))
round(ftab,4)

# by age group
age.mns <- svyby(formula= ~delay.med, by=~age.grp, FUN=svymean, design = d2.cal, na.rm=TRUE)
round(age.mns,4)


#----------------------------------------------------------------------------------------
