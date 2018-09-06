#*******************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\          
#                   Examples\Example 16.5 poststrat.R                                       
# TOPIC:    Select srs sample and compute postratified estimate using large NHIS data set.  
# DATE:     05/24/2011                                                                      
# AUTHOR:   R. Valliant                                                                     
#*******************************************************************************************

require(sampling)
require(survey)
require(PracTools)

data(nhis.large)

        # collapse hisp = 3,4
hisp.r <- nhis.large$hisp
hisp.r[nhis.large$hisp ==4] <- 3
table(hisp.r)
nhis.large1 <- data.frame(nhis.large, hisp.r)
t1 <- table(nhis.large$medicaid, nhis.large1$hisp.r)
100*round(prop.table(t1,2),3)

        # create single variable to identify age.grp x hisp.r poststrata
m <- max(nhis.large1$hisp.r)
nhis.large1$PS <- (nhis.large1$age.grp - 1)*m + nhis.large1$hisp.r
N.PS <- table(PS = nhis.large1$PS)

        # select  srswor of size n
set.seed(-1570723087) 
n <- 250
N <- nrow(nhis.large1)
sam <- sample(1:N, n)
samdat <- nhis.large1[sam, ]
n.PS <- table(samdat[, "age.grp"], samdat[, "hisp.r"])
as.vector(n.PS)

        # compute srs weights and sampling fraction
d <- rep(N/n, n)

        # srswor design object
nhis.dsgn <- svydesign(ids = ~0,    # no clusters 
          strata = NULL,    # no strata
          data = data.frame(samdat), 
          weights = ~d) 
          
        # poststratified design object
ps.dsgn <- postStratify(design = nhis.dsgn, 
                        strata = ~PS, 
                        population = N.PS)

        # Check that weights are calibrated for x's
svytotal(~ as.factor(PS), ps.dsgn)

        # PS standard errors and cv's
svytotal(~ as.factor(medicaid), ps.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), ps.dsgn, na.rm=TRUE))
svytotal(~ as.factor(doc.visit), ps.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(doc.visit), ps.dsgn, na.rm=TRUE))

        # standard errors and cv's ignoring poststratification
wts <- weights(ps.dsgn)
        # design object ignoring PS
noPS.dsgn <- svydesign(ids = ~0, 
          strata = NULL,    
          data = data.frame(samdat), 
          weights = ~wts) 

svytotal(~ as.factor(medicaid), noPS.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), noPS.dsgn, na.rm=TRUE))
svytotal(~ as.factor(doc.visit), noPS.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(doc.visit), noPS.dsgn, na.rm=TRUE))
