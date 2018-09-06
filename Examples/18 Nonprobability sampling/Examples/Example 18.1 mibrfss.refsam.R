#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\18 Nonprobability sampling\Examples\Example 18.1 mibrfss.refsam.R
# Project:  Practical Tools book
# Date: 	08/26/2017
# Author:	R. Valliant
# Purpose:	Extract MI BRFSS cases that have Internet at home and create a reference pop to use
#           for pseudo inclusion prb estimation
#--------------------------------------------------------------------------------------------------

require(PracTools)
require(survey)
require(sampling)

set.seed(-643998832)

data(mibrfss)
dim(mibrfss)
#[1] 2845   21
    # bootstrap to larger pop
N <- 20000
bsam <- sample(1:nrow(mibrfss), N, replace=TRUE)
bpop <- mibrfss[bsam,]

head(bpop,3)

    # tabs on pop
table(bpop$SMOKE100)
prop.table(table(bpop$SMOKE100))
prop.table(table(bpop$GENHLTH))
cumsum(prop.table(table(bpop$GENHLTH)))

internet <- bpop[bpop$INETHOME==1,]
dim(internet)
#[1] 13048    21
(Nh.pop <- table(bpop$AGECAT, useNA="always"))
(round(Nh.pop/sum(Nh.pop),3))
(Nh <- table(internet$AGECAT, useNA="always"))
#   1    2    3    4    5    6 <NA>
# 790 2037 3071 3363 2254 1533    0

    # select a sample from the internet cases using AGECAT as strata. Rates are set so
    # that younger people are much more likely to be sampled.
nh <- c(24, 62, 37, 41, 27, 9)
    # sampling rates
(nh/Nh[1:6])
#          1           2           3           4           5           6
#0.030379747 0.030436917 0.012048193 0.012191496 0.011978705 0.005870841


internet <- internet[order(internet$AGECAT),]
sam <- strata(internet, stratanames="AGECAT", size=nh, method="srswor")
internet <- getdata(internet, sam)

table(internet$AGECAT)
# 1  2  3  4  5  6
#24 62 37 41 27  9
prop.table(table(internet$SMOKE100))
#    1     2
#0.48 0.52
prop.table(table(internet$GENHLTH))
#    1     2     3     4     5
#0.285 0.380 0.275 0.045 0.015
cumsum(prop.table(table(internet$GENHLTH)))
#    1     2     3     4     5
#0.285 0.665 0.940 0.985 1.000

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
    # adjust reference sample wts for dups
rsam$wts <- rsam$wts * n/(n-n.dup)
    # adjust reference sample wts to account for fraction of NP sample in pop
N.hat <- sum(rsam$wts)
rsam$wts <- rsam$wts * (N.hat - n) / N.hat

    # eliminate the duplicates (if any) from the reference sample
combined <- rbind(internet, rsam)
combined <- combined[!dups,]

    # recode good or better health
internet$GoB <- internet$GENHLTH <= 3

    # estimate pseudo inclusion probs
    # create 0-1 vector for (in internet sam)/(in ref sam)
in.internet <- c(rep(1, nrow(internet)), rep(0,nrow(rsam[!dups[(n+1):(2*n)],])))
table(in.internet)
#  0   1
#197 200
combined <- cbind(combined, in.internet)
m <- glm(in.internet ~ as.factor(AGECAT) + as.factor(RACECAT) +
                       as.factor(EDCAT) + as.factor(INCOMC3),
                       family = binomial(link="logit"),
                       weights = combined$wts,
                       data = combined)
summary(m)
#Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)
#(Intercept)         -4.75148    0.52397  -9.068  < 2e-16 ***
#as.factor(AGECAT)2  -0.09422    0.26916  -0.350  0.72630
#as.factor(AGECAT)3  -1.21559    0.29095  -4.178 2.94e-05 ***
#as.factor(AGECAT)4  -1.19989    0.29187  -4.111 3.94e-05 ***
#as.factor(AGECAT)5  -1.21966    0.30628  -3.982 6.83e-05 ***
#as.factor(AGECAT)6  -2.60443    0.40419  -6.444 1.17e-10 ***
#as.factor(RACECAT)2  0.09694    0.31650   0.306  0.75939
#as.factor(RACECAT)3 -0.36956    0.35827  -1.031  0.30231
#as.factor(EDCAT)2    0.87106    0.44665   1.950  0.05115 .
#as.factor(EDCAT)3    1.19936    0.44476   2.697  0.00700 **
#as.factor(EDCAT)4    1.26900    0.44014   2.883  0.00394 **
#as.factor(INCOMC3)2 -0.05201    0.26476  -0.196  0.84427
#as.factor(INCOMC3)3  0.23879    0.26629   0.897  0.36985
#as.factor(INCOMC3)4  0.42232    0.26098   1.618  0.10562
#as.factor(INCOMC3)5  0.15336    0.26089   0.588  0.55665

L.hat <- m$linear.predictors
pseudo.probs <- exp(L.hat) / (1+exp(L.hat))
np.rows <- 1:nrow(internet)
sum(1/pseudo.probs[np.rows])
#[1]  19362.49

summary(1/pseudo.probs[np.rows])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  21.76   35.89   80.95   96.81  108.09  656.14

internet <- cbind(internet, pseudo.probs[np.rows], pseudo.wt = 1/pseudo.probs[np.rows])

np.dsgn <- svydesign(ids = ~0, strata  = NULL,
                 data    = internet,
                 weights = ~pseudo.wt)
svymean(~factor(SMOKE100), design=np.dsgn)
#                     mean     SE
#factor(SMOKE100)1 0.56053 0.0483
#factor(SMOKE100)2 0.43947 0.0483
prop.table(table(bpop$SMOKE100))
#    1      2
#0.535 0.465
prop.table(table(internet$SMOKE100))
#   1    2
#0.48 0.52

svymean(~factor(GENHLTH), design=np.dsgn)
#                     mean     SE
#factor(GENHLTH)1 0.216347 0.0362
#factor(GENHLTH)2 0.357159 0.0478
#factor(GENHLTH)3 0.322210 0.0461
#factor(GENHLTH)4 0.067119 0.0290
#factor(GENHLTH)5 0.037166 0.0242

svymean(~GoB, design=np.dsgn)
#            mean     SE
#GoBFALSE 0.10428 0.0364
#GoBTRUE  0.89572 0.0364

    # treat GENHLTH as continuous
svymean(~GENHLTH, design=np.dsgn)
#          mean     SE
#GENHLTH 2.3516 0.1064

    # Unweighted internet sum
cumsum(prop.table(table(internet$GENHLTH)))
#    1     2     3     4     5
#0.285 0.665 0.940 0.985 1.000
mean(internet$GENHLTH)
#[1] 2.125

    # full pop, combined population cume:
round(cumsum(prop.table(table(bpop$GENHLTH))),3)
#    1     2     3     4     5
#0.179 0.536 0.843 0.959 1.000

mean(bpop$GENHLTH)
#[1] 2.4825
