#********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Calibration\Examples\raking.undercoverage.R
# TOPIC:    Select srs sample and compute postratified estimate using large NHIS data set.               
#           Illustrate undercoverage by frame and correction by poststratification.                      
# DATE:     06/24/2010                                                                                   
# AUTHOR:   R. Valliant                                                                                  
#********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.large.RData", pos=2)
require(survey)
require(doBy)

        # set seed for run
set.seed(610376119)

        # collapse hisp = 3,4
hisp.r <- nhis.large$hisp
hisp.r[nhis.large$hisp ==4] <- 3
table(hisp.r)
nhis.large1 <- data.frame(nhis.large, hisp.r)

        # create marginal pop totals 
N.age <- table(nhis.large1$age.grp)
N.age
N.hisp <- table(nhis.large1$hisp.r)
N.hisp


        # create frame with undercoverage
        # 75% coverage of Hispanics and non-Hispanic Black & Other. These correspond to
        # poststrata 1,3,4,6,7,9,10,12,13, and 15.
PS.prob <- rep(c(0.75, 1, 0.75), 5)
cov.prob <- PS.prob[nhis.large1$PS]
N <- nrow(nhis.large1)
rn <- runif(N)
nhis.cov <- nhis.large1[rn <= cov.prob, ]
 
        # select  srswor of size n
n <- 500

N.cov <- nrow(nhis.cov)
sam <- sample(1:N.cov, n)
samdat <- nhis.cov[sam, ]
n.age <- table(samdat[, "age.grp"])
n.age
n.hisp <- table(samdat[, "hisp.r"])
n.hisp

        # compute srs weights and sampling fraction
d <- rep(N.cov/n, n)
f1 <- rep(n/N.cov, n)

nhis.dsgn <- svydesign(ids = ~0,    # no clusters 
          strata = NULL,    # no strata
          fpc = ~f1,
          data = data.frame(samdat), 
          weights = ~d) 
          
pop.totals <- c('(Intercept)' = N, N.age[-1], N.hisp[-1])
rake.dsgn <- calibrate(design = nhis.dsgn,
                        formula = ~ as.factor(age.grp) + as.factor(hisp.r), 
                        calfun = "raking",
                        population = pop.totals
             )

        # Check that weights are calibrated for x's
svytotal(~ as.factor(age.grp), rake.dsgn)
svytotal(~ as.factor(hisp.r), rake.dsgn)

        # actual pop totals
table(nhis.large1$medicaid)
table(nhis.large1$medicaid, nhis.large1$hisp.r)
table(nhis.large1$medicaid, nhis.large1$age.grp, nhis.large1$hisp.r)
abs(mean(nhis.large1$medicaid-2, na.rm=TRUE))
#   [1] 0.1071194

summaryBy(abs(medicaid-2) ~ hisp.r, data = nhis.large1, FUN = mean, na.rm=TRUE)
#     hisp.r abs(medicaid - 2).mean
#   1      1             0.18869828
#   2      2             0.05822137
#   3      3             0.16114733

        # raking standard errors and cv's
svytotal(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE))
svymean(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE)
cv(svymean(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE))

        # srs standard error and cv's
svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE))
svymean(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE)
cv(svymean(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE))

        # estimates for hispanic
svyby(~as.factor(medicaid), ~hisp.r, rake.dsgn, svytotal, na.rm=TRUE)
cv(svyby(~as.factor(medicaid), ~hisp.r, rake.dsgn, svytotal, na.rm=TRUE))
svyby(~as.factor(medicaid), ~hisp.r, rake.dsgn, svymean, na.rm=TRUE)
cv(svyby(~as.factor(medicaid), ~hisp.r, rake.dsgn, svymean, na.rm=TRUE))

svyby(~as.factor(medicaid), ~hisp.r, nhis.dsgn, svytotal, na.rm=TRUE)
cv(svyby(~as.factor(medicaid), ~hisp.r, nhis.dsgn, svytotal, na.rm=TRUE))
svyby(~as.factor(medicaid), ~hisp.r, nhis.dsgn, svymean, na.rm=TRUE)
cv(svyby(~as.factor(medicaid), ~hisp.r, nhis.dsgn, svymean, na.rm=TRUE))
