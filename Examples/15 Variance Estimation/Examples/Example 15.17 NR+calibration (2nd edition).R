#**************************************************************************************************************
# FILE :   C:\Projects\Practical Tools Book\Book Chapters\15 Variance estimation\Examples\
#			Example 15.17 NR+calibration (2nd edition).R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# AUTHOR:  R. Valliant
# PURPOSE: Predict response propensities with logistic regn.
#           Form 5 classes and check on balance within classes
# DATE:     11/02/2017
# PGMR:     R. Valliant
#**************************************************************************************************************

require(survey)
require(PracTools)
data(nhis)

nhis.dsgn <- svydesign(ids = ~psu, strata = ~stratum,
                 data = nhis,
                 weights = ~ svywt,
                 nest = TRUE
                 )
nhis.BRR <- as.svrepdesign(design = nhis.dsgn, type = "BRR")

BRRwts <- weights(nhis.BRR, type="analysis")
dim(BRRwts)
#[1] 3911   88  # cols = replicates

full.NRwts <- weights(nhis.dsgn)
pc <- pclass(formula = resp ~ age + as.factor(hisp) + as.factor(race),
                    type = "wtd", link="logit", numcl=5, design = nhis.dsgn)
full.NRwts <- full.NRwts / pc$propensities

options(warn = -1)
BRR.NRwts <- BRRwts

for (r in 1:ncol(BRRwts)){
    wts <- BRRwts[,r]
    d <- svydesign(ids = ~0, strata = NULL, data = nhis, weights = wts)
    pc <- pclass(formula = resp ~ age + as.factor(hisp) + as.factor(race),
                    type = "wtd", link="logit", numcl=5, design = d)
    BRR.NRwts[,r] <- BRRwts[,r] / pc$propensities
}

dim(BRR.NRwts)
#[1] 3911   88

cnames <- paste0("BRR",1:88)
colnames(BRR.NRwts) <- cnames

nhis.NRadj <- nhis[nhis$resp==1, ]
full.NRwts  <- full.NRwts[nhis$resp==1]
BRR.NRwts <- BRR.NRwts[nhis$resp==1, ]

dim(nhis.NRadj)
#[1] 2699  104
length(full.NRwts)
#[1] 2699
dim(BRR.NRwts)
#[1] 2699   88

NRrep.dsgn <- svrepdesign(repweights = BRR.NRwts,
    weights = full.NRwts, data = nhis.NRadj, type = "BRR")

svymean(~as.factor(parents_r), design = NRrep.dsgn, na.rm=TRUE)
#                         mean     SE
#as.factor(parents_r)1 0.10761 0.0061
#as.factor(parents_r)2 0.89239 0.0061

svymean(~age, design = NRrep.dsgn, na.rm=TRUE)
#      mean     SE
#age 45.556 0.3624

NR.norep <- svydesign(ids = ~psu, strata = ~stratum,
                 data = nhis.NRadj,
                 weights = ~full.NRwts,
                 nest = TRUE
                 )
svymean(~as.factor(parents_r), design = NR.norep, na.rm=TRUE)
#                         mean     SE
#as.factor(parents_r)1 0.10761 0.0064
#as.factor(parents_r)2 0.89239 0.0064

svymean(~age, design = NR.norep, na.rm=TRUE)
#      mean     SE
#age 45.556 0.4228

svytotal(~as.factor(parents_r), design = NR.norep, na.rm=TRUE)
svytotal(~as.factor(parents_r), design = NRrep.dsgn, na.rm=TRUE)
