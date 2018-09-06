#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\18 Nonprobability sampling\Examples\Example 18.2 mibrfss.superpop.R
# Project:  Practical Tools book
# Date: 	09/06/2017
# Author:	R. Valliant
# Purpose:	Extract MI BRFSS cases that have Internet at home, take sample, and estimate means
#           using prediction model
#--------------------------------------------------------------------------------------------------

require(survey)
require(PracTools)
require(sampling)
set.seed(-643998832)

attach("C:\\Projects\\Stata Weighting Book\\Data\\mibrfss\\mibrfss.RData")
dim(mibrfss)
#[1] 2845   21
    # bootstrap to larger pop
N <- 20000
bsam <- sample(1:nrow(mibrfss), N, replace=TRUE)
bpop <- mibrfss[bsam,]

    # select a sample from the internet cases
internet <- bpop[bpop$INETHOME==1,]
dim(internet)
#[1] 13048    21
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
    # recode good or better health
internet$GoB <- internet$GENHLTH <= 3

sdsgn <- svydesign(ids = ~0, strata  = NULL,
                 data    = internet,
                 weights = ~rep(1, nrow(internet))
                 )
mdsgn <- calibrate(design = sdsgn,
            formula = ~ as.factor(AGECAT) + as.factor(RACECAT) +
                       as.factor(EDCAT) + as.factor(INCOMC3),
            population = pop.tots,
            bounds = c(0.25, Inf),
            calfun="linear")

sum(weights(mdsgn))
#[1] 20000
summary(weights(mdsgn))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  31.68   31.68   60.24  100.00  125.43  540.72

svymean(~factor(SMOKE100), design=mdsgn)
#                     mean     SE
#factor(SMOKE100)1 0.54821 0.0504
#factor(SMOKE100)2 0.45179 0.0504
prop.table(table(bpop$SMOKE100))
#    1      2
#0.535 0.465
prop.table(table(internet$SMOKE100))
#   1    2
#0.48 0.52

svymean(~factor(GENHLTH), design=mdsgn)
#                     mean     SE
#factor(GENHLTH)1 0.211801 0.0372
#factor(GENHLTH)2 0.344738 0.0458
#factor(GENHLTH)3 0.313940 0.0423
#factor(GENHLTH)4 0.090709 0.0315
#factor(GENHLTH)5 0.038812 0.0247

svymean(~factor(GoB), design=mdsgn)
#                    mean     SE
#factor(GoB)FALSE 0.12952 0.0372
#factor(GoB)TRUE  0.87048 0.0372

    # treat GENHLTH as continuous
zz <- svymean(~ GENHLTH, design=mdsgn)
#        mean    SE
#GENHLTH  2.4 0.106
mean(zz)
#[1] 2.399994
SE(zz)
#          GENHLTH
#GENHLTH 0.1059729


#--------------------------------------------------------------------------------------------------
    # Unweighted internet sum
cumsum(prop.table(table(internet$GENHLTH)))
#    1     2     3     4     5
#0.285 0.665 0.940 0.985 1.000

prop.table(table(internet$GoB))
#FALSE  TRUE
# 0.06  0.94

#--------------------------------------------------------------------------------------------------

    # full pop, combined population cume:
round(cumsum(prop.table(table(bpop$GENHLTH))),3)
#    1     2     3     4     5
#0.179 0.536 0.843 0.959 1.000

#--------------------------------------------------------------------------------------------------
#   direct calculation of model-based weights
#--------------------------------------------------------------------------------------------------

X <- model.matrix(~ as.factor(AGECAT) + as.factor(RACECAT) +
                       as.factor(EDCAT) + as.factor(INCOMC3),
                       data = internet)
XtX <- t(X) %*% X
sam.age <- as.vector(table(internet$AGECAT))
sam.race <- as.vector(table(internet$RACECAT))
sam.ed <- as.vector(table(internet$EDCAT))
sam.inc <- as.vector(table(internet$INCOMC3))

sam.tots <- c('(Intercept)' = n,
                AGECAT = sam.age[-1],
                RACECAT = sam.race[-1],
                EDCAT = sam.ed[-1],
                INCOMC3 = sam.inc[-1])
nsam.tots <- pop.tots - sam.tots

    # prediction weights
w1 <- rep(1,n) + nsam.tots %*% solve(XtX) %*% t(X)
summary(as.vector(w1))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -27.27   42.06   76.38  100.00  119.51  520.07

sum(round(w1,3) == round(weights(mdsgn),3))
#[1] 0  different from mdsgn weights because of bounding in mdsgn
w2 <- pop.tots %*% solve(XtX) %*% t(X)
sum(round(w2,3) == round(weights(mdsgn),3))
#[1] 200


#--------------------------------------------------------------------------------------------------
#   direct calculation of model-based SEs
#--------------------------------------------------------------------------------------------------
m <- lm(abs(internet$SMOKE100-2) ~ as.factor(AGECAT) + as.factor(RACECAT) +
                       as.factor(EDCAT) + as.factor(INCOMC3),
            data = internet)
h <- hatvalues(m)
r <- resid(m)

sdcal <- function(a, r, h, N){
   rtvR <- sqrt(sum((a*r)^2) / N^2)
   rtvH <- sqrt(sum((a*r)^2 / (1-h)) / N^2)
   rtvJ <- sqrt(sum(((a*r) / (1-h))^2) / N^2)
   c("rtvR"=rtvR, "rtvH"=rtvH, "rtvJ"=rtvJ)
}

    # compute leverages by hand
H <- X %*% solve(XtX) %*% t(X)
h1 <- diag(H)
sum(round(h,5) == round(h1,5))
#[1] 200    hand-calculated and lm leverages are same to 5 decimals

sdcal(a=w1-1, r=r, h=h, N=20000)
sdcal(a=w2-1, r=r, h=h, N=20000)
sdcal(a=weights(mdsgn)-1, r=r, h=h, N=20000)
