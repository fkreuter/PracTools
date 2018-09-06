#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\18 Nonprobability sampling\Examples\Example 18.3 direct SE calc.R
# Project:  Practical Tools book
# Date: 	09/13/2017
# Author:	R. Valliant
# Purpose:	Extract MI BRFSS cases that have Internet at home, take sample, and estimate means
#           using prediction model
#           Example 18.2 must be run to select sample for this example.
#--------------------------------------------------------------------------------------------------


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
sum(round(w1,3) == round(weights(mdsgn),3))
#[1] 200
w2 <- pop.tots %*% solve(XtX) %*% t(X)
#[1] 200
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
#      rtvR       rtvH       rtvJ
#0.04965448 0.05327607 0.05727221
sdcal(a=w2-1, r=r, h=h, N=20000)
#      rtvR       rtvH       rtvJ
#0.04965448 0.05327607 0.05727221
#sdcal(a=weights(mdsgn), r=r, h=h, N=20000)
#      rtvR       rtvH       rtvJ
#0.05027989 0.05396275 0.05802562
