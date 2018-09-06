#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Example 14.1 ratio est.R
# Project:  Practical Tools book
# Date: 	09/10/2017
# Author:	R. Valliant
# Purpose:	Illustrate standard ratio estimate in srswor
#--------------------------------------------------------------------------------------------------

require(PracTools)
data(hospital)

set.seed(974648479)
n <- 50
N <- nrow(hospital)
x.pop <- sum(hospital$x)
sam <- sample(1:N, n)
samdat <- hospital[sam,]

srs.dsgn <- svydesign(ids = ~0, strata = NULL, data = samdat, weights = rep(N/n,n), fpc=rep(N,n))
Rdsgn <- calibrate(design=srs.dsgn, formula= ~x-1, population=x.pop, variance=samdat$x)
weights(Rdsgn)

svytotal(~x, Rdsgn)
#   total SE
#x 107956  0
svytotal(~y, Rdsgn)
#   total    SE
#y 340051 12565
svytotal(~y, srs.dsgn)
#   total    SE
#y 315123 30257
