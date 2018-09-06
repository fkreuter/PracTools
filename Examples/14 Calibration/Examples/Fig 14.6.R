#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Fig 14.6.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Plot 500 points from HMT pop
#*********************************************************************************************************

HMT.fcn <- function (N = 5000, H = 10) 
{
    #       HMT.fcn
    #   Generate the population used in 
    #      Hansen, M.H., Madow, W.G., and Tepping, B.J. (1983), 
    #   N = pop size
    #   H = no. of strata
    
    x <- rgamma(n = N, shape = 2, scale = 5)
    p <- 0.04 * (x^(-3/2)) * ((8 + 5*x)^2)
    lam <- (8 + 5*x)/(1.25 * (x^(3/2)))
    y <- rgamma(n = N, shape = p, rate = lam)
    pop <- cbind(x,y)
    pop <- pop[order(pop[, 1]), ]
    
    brk <- (0:H) * sum(pop[, 1]/H)
    brk[length(brk)] <- cumsum(pop[, 1])[N]
    strat <- cut(cumsum(pop[, 1]), brk)
    
    pop <- cbind(strat, pop)
    pop
}

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig 14.6.eps")
set.seed(2146616503)
hmt <- HMT.fcn()

sam <- seq(1,5000,10)

par(mgp = c(2.5,0.8,0))
plot(hmt[sam,"x"], hmt[sam,"y"],
     pch = 16,
     xlab = "x",
     ylab = "y",
     cex = 0.8,
     cex.axis = 1.3,
     cex.lab = 1.3)

dev.off()
