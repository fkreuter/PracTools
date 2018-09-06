#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\18 Nonprobability sampling\Examples\Example 18.5 mibrfss.doublyrobust.R
# Project:  Practical Tools book
# Date: 	11/01/2017
# Author:	R. Valliant
# Purpose:	Doubly robust estimates combining quasi-randomization and superpop model estimation
#--------------------------------------------------------------------------------------------------

require(survey)
require(sampling)
require(PracTools)
set.seed(-643998832)

attach("C:\\Projects\\Stata Weighting Book\\Data\\mibrfss\\mibrfss.RData")
dim(mibrfss)
    # bootstrap to larger pop
N <- 20000
bsam <- sample(1:nrow(mibrfss), N, replace=TRUE)
bpop <- mibrfss[bsam,]

internet <- bpop[bpop$INETHOME==1,]
(Nh.pop <- table(bpop$AGECAT, useNA="always"))
(round(Nh.pop/sum(Nh.pop),3))
(Nh <- table(internet$AGECAT, useNA="always"))

    # select a sample from the internet cases using AGECAT as strata. Rates are set so
    # that younger people are much more likely to be sampled.
nh <- c(24, 62, 37, 41, 27, 9)
    # sampling rates
(nh/Nh[1:6])

internet <- internet[order(internet$AGECAT),]
sam <- strata(internet, stratanames="AGECAT", size=nh, method="srswor")
internet <- getdata(internet, sam)

    # select a reference sample from full pop
n <- 200
rsam <- sample(1:nrow(bpop), n)
rsam <- bpop[rsam,]
rsam.names <- colnames(rsam)

wts <- rep(N/n, n)
rsam <- cbind(rsam, wts)

internet <- internet[, rsam.names]
internet <- cbind(internet, wts = rep(1, nrow(internet)) )

    # check for duplicates in internet and rsam
dups <- duplicated(c(rownames(internet), rownames(rsam)))
(n.dup <- sum(dups))
#[1] 3      # 3 duplicate cases
rsam$wts <- rsam$wts * n/(n-n.dup)
    # eliminate the duplicates (if any) from the reference sample
combined <- rbind(internet, rsam)
combined <- combined[!dups,]
    # recode good or better health
internet$GoB <- internet$GENHLTH <= 3

#--------------------------------------------------------------------------------------------------
    # estimate pseudo inclusion probs
    # create 0-1 vector for (in internet sam)/(in ref sam)
in.internet <- c(rep(1, nrow(internet)), rep(0,nrow(rsam[!dups[(n+1):(2*n)],])))
table(in.internet)
combined <- cbind(combined, in.internet)
m <- glm(in.internet ~ as.factor(AGECAT) + as.factor(RACECAT) +
                       as.factor(EDCAT) + as.factor(INCOMC3),
                       family = binomial(link="logit"),
                       weights = combined$wts,
                       data = combined)
summary(m)

L.hat <- m$linear.predictors
pseudo.probs <- exp(L.hat) / (1+exp(L.hat))
np.rows <- 1:nrow(internet)
summary(1/pseudo.probs[np.rows])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  21.96   36.24   81.75   97.77  109.16  662.63

internet <- cbind(internet, pseudo.probs[np.rows], pseudo.wt = 1/pseudo.probs[np.rows])

#--------------------------------------------------------------------------------------------------
#   Superpop model
    # population control totals
pop.age <- as.vector(table(bpop$AGECAT))
pop.race <- as.vector(table(bpop$RACECAT))
pop.ed <- as.vector(table(bpop$EDCAT))
pop.inc <- as.vector(table(bpop$INCOMC3))

pop.tots <- c('(Intercept)' = N,
                AGECAT = pop.age[-1],
                RACECAT = pop.race[-1],
                EDCAT = pop.ed[-1],
                INCOMC3 = pop.inc[-1])

sdsgn <- svydesign(ids = ~0, strata  = NULL,
                 data    = internet,
                 weights = ~ pseudo.wt
                 )
mdsgn <- calibrate(design = sdsgn,
            formula = ~ as.factor(AGECAT) + as.factor(RACECAT) +
                       as.factor(EDCAT) + as.factor(INCOMC3),
            population = pop.tots,
            calfun="linear")

sum(weights(mdsgn))
#[1] 20000
summary(weights(mdsgn))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  8.666  39.205  66.043 100.000 119.641 632.666

#--------------------------------------------------------------------------------------------------
# Estimate a few things
svymean(~factor(SMOKE100), design=mdsgn)
#                     mean     SE
#factor(SMOKE100)1 0.55341 0.0479
#factor(SMOKE100)2 0.44659 0.0479

    # pop value
prop.table(table(bpop$SMOKE100))
#    1      2
#0.535 0.465
    # unweighted sample estimate
prop.table(table(internet$SMOKE100))
#   1    2
#0.48 0.52

svymean(~factor(GENHLTH), design=mdsgn)
#                     mean     SE
#factor(GENHLTH)1 0.208336 0.0334
#factor(GENHLTH)2 0.344496 0.0417
#factor(GENHLTH)3 0.311070 0.0409
#factor(GENHLTH)4 0.093120 0.0288
#factor(GENHLTH)5 0.042978 0.0246

svymean(~factor(GoB), design=mdsgn)
#                   mean     SE
#factor(GoB)FALSE 0.1361 0.0335
#factor(GoB)TRUE  0.8639 0.0335

    # treat GENHLTH as continuous
zz <- svymean(~ GENHLTH, design=mdsgn)
#          mean     SE
#GENHLTH 2.4179 0.0989
mean(zz)
#[1] 2.417908
SE(zz)
#          GENHLTH
#GENHLTH 0.09894767
