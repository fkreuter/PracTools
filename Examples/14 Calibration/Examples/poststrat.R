#***********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Calibration\Examples\poststrat.R              
# TOPIC:    Select srs sample and compute postratified estimate using large NHIS data set.                        
# DATE:     06/24/2010                                                                                      
# AUTHOR:   R. Valliant                                                                                     
#***********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.large.RData", pos=2)
require(sampling)
require(doBy)
require(survey)

            # population means of of some variables by age and sex
t1 <- table(nhis.large$delay.med, nhis.large$age.grp)
prop.table(t1,2)
t1 <- table(nhis.large$delay.med, nhis.large$sex)
prop.table(t1,2)
t1 <- table(nhis.large$delay.med, nhis.large$hisp)
prop.table(t1,2)

t1 <- table(nhis.large$notcov, nhis.large$age.grp)
prop.table(t1,2)
t1 <- table(nhis.large$notcov, nhis.large$sex)
prop.table(t1,2)
t1 <- table(nhis.large$notcov, nhis.large$hisp)
prop.table(t1,2)

t1 <- table(nhis.large$medicaid, nhis.large$age.grp)
100*round(prop.table(t1,2),3)
t1 <- table(nhis.large$medicaid, nhis.large$sex)
prop.table(t1,2)
t1 <- table(nhis.large$medicaid, nhis.large$hisp)
prop.table(t1,2)

t1 <- table(nhis.large$doc.visit, nhis.large$age.grp)
prop.table(t1,2)
t1 <- table(nhis.large$doc.visit, nhis.large$sex)
prop.table(t1,2)
t1 <- table(nhis.large$doc.visit, nhis.large$hisp)
prop.table(t1,2)

        # based on the above tabs, collapse hisp = 3,4
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

        # intreraction of age and hisp
t1 <- table(nhis.large1$medicaid, nhis.large1$age.grp, nhis.large1$hisp.r)
100 * round(prop.table(t1[,,1],2),3)
100 * round(prop.table(t1[,,2],2),3)
100 * round(prop.table(t1[,,3],2),3)

        # select  srswor of size n
#set.seed(-530049348)
set.seed(-1570723087) 
#set.seed(1428901756)
n <- 250
N <- nrow(nhis.large1)
sam <- sample(1:N, n)
samdat <- nhis.large1[sam, ]
n.PS <- table(samdat[, "age.grp"], samdat[, "hisp.r"])
as.vector(n.PS)

        # compute srs weights and sampling fraction
d <- rep(N/n, n)
f1 <- rep(n/N, n)

        # srswor design object
nhis.dsgn <- svydesign(ids = ~0,    # no clusters 
          strata = NULL,    # no strata
          fpc = ~f1,
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

        # srs standard error and cv's
svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE))
svytotal(~ as.factor(doc.visit), nhis.dsgn, na.rm=TRUE)
cv(svytotal(~ as.factor(doc.visit), nhis.dsgn, na.rm=TRUE))
