#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\Example 17.3.R                
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     06/012/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Illustrate potential nonresponse bias with 2 strata example
#*********************************************************************************************************

    # W2 = proportion of nonrespondents
W2 <- seq(0,0.7,0.1)
    # k = ratio of NR stratum mean to R stratum mean
k <- seq(0.8, 1.2, 0.05)


postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\17 Multiphase Designs\\Examples\\Fig 17.2.eps")

par(mfrow=c(1,1), mgp = c(2,0.5,0))

for (i in 1:length(k)){
    relbias <- W2*(1-k[i]) / (1 - W2*(1-k[i]))
    if (i==1) {plot(W2, relbias, 
                    type="l",
                    lty=2,
                    xlim = c(0,0.8),
                    ylim = c(-0.16, 0.16),
                    lwd = 1.5,
                    xlab = "W2: proportion of nonrespondents")
               L <- length(relbias)
    }
    else {
        if (k[i]==1) lines(W2, relbias, lty=1, lwd = 2)    
        else lines(W2, relbias, lty=i+1, lwd = 1.5)
    }
    
    text(x = W2[L]+0.02, y = relbias[L], paste("k=",k[i]), 
         adj = 0, cex = 0.8)
}
grid(col="gray30")

dev.off()