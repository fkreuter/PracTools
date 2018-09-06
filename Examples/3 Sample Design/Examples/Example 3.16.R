#*********************************************************************************************************
# FILE: C:\Projects\Practical Tools Book\Book Chapters\3 Sample Design\Examples\Example 3.16.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    08/20/2012
# AUTHOR:  R. Valliant
# PURPOSE: Select pps sample from the hospitals population and
#	     estimate V1 under pp(beds) and under pp(sqrt(beds))
#*********************************************************************************************************


#   attach("C:/Projects/Populations/hospital.RData", pos=2)
require(PracTools)
data(hospital)

N <- nrow(hospital)

sam <- c(76,155,192,200,228,243,253,289,297,315,320,321,329,354,360,369,373,376,378,381)
n <- length(sam)

y <- hospital[sam, "y"]
x <- hospital[sam, "x"]
pop.x <- hospital[, "x"]

	# calculate V1 based on pp(x) sample
pi <- pop.x/sum(pop.x)
wi <- 1/(n*pi)
t.hat <- sum(y/pi[sam]/n)
ybar.pwr <- t.hat/N

V1 <- sum((y/pi[sam] - t.hat)^2)/(n-1)
V1
[1] 11001669955

v.pwr <- V1/N^2/n
v.pwr
[1] 3561.587


CV.t <- sqrt(v.pwr)/(t.hat/N)
CV.t
[1] 0.07339767


	# Suppose that we plan to select a future sample with probabilities
	# proportional to the square root of beds
	# calculate new V1 based on pp(rtx) sample
qi <- sqrt(pop.x)/sum(sqrt(pop.x))
V1 <- sum(y^2/pi[sam]/qi[sam]/n) - t.hat^2 + N^2*v.pwr
V1
[1] 21500975135

	# CV in sample of n=20
CV.t <- sqrt(V1/n)/t.hat
CV.t
[1] 0.1026082

                # table values
cbind(sam, y, x)
      sam    y   x
 [1,]  76  244  70
 [2,] 155  402 160
 [3,] 192  732 227
 [4,] 200  925 235
 [5,] 228  632 275
 [6,] 243  557 300
 [7,] 253 1226 310
 [8,] 289  896 378
 [9,] 297 2190 400
[10,] 315 1948 461
[11,] 320 1239 472
[12,] 321 1258 474
[13,] 329 1657 498
[14,] 354 2116 562
[15,] 360 1326 584
[16,] 369 1606 635
[17,] 373 1707 670
[18,] 376 2089 712
[19,] 378 1283 760
[20,] 381 1239 816
