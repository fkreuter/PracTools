#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Fig 14.1.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Two plots with different strength of x-y relationship
#*********************************************************************************************************

set.seed(323)
n0 <- 100
x <- rgamma(n=n0, shape=10, scale=5)
range(x)
 
y1 <- 2.5*x - 0.018*x^2 + rnorm(n0, 0, 2)*(x^0.08)
y2 <- 2.5*x - 0.02*x^2 + rnorm(n0, 0, 2)*x^(0.285)

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig 14.1.eps",
           width=6,
           height=4)

par(mfrow=c(1,2), mar = c(3,3,1,1),
    mgp = c(2,0.5,0))
plot(x,y1,
     pch = 16,
     cex=0.6,
     ylab = "y")
plot(x,y2,
     pch = 16,
     cex=0.6,
     ylab = "y")

dev.off()
