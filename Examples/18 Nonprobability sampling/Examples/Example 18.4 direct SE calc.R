#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\18 Nonprobability sampling\Examples\Example 18.3 mibrfss.superpop.R
# Project:  Practical Tools book
# Date: 	09/06/2017
# Author:	R. Valliant
# Purpose:	Direct calculation of model-based SEs
#           Code in "Example 18.3 mibrfss.superpop.R" must be run before executing code below
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
sdcal(a=weights(mdsgn), r=r, h=h, N=20000)
#      rtvR       rtvH       rtvJ
#0.05027989 0.05396275 0.05802562
