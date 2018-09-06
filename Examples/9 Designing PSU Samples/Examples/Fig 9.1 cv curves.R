#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples 
#               \Examples\Fig 9.1 cv curves.R                                          
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     02/19/2012                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Compute CVs in 2-stage sampling for different combinations of cost     
#           components, unit relvariance, and measures of homogeneity.             
#**********************************************************************************

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\9 Designing PSU Samples\\Examples\\Fig 9.1.eps")

nbar <- 1:30    # nbar = number of sample elements per PSU
                # m = number of PSUs
unit.RV <- 1
C1 <- c(750, 500, 250)
C2 <- 250
C.rat <- C1/C2
C.tot <- 100000

delta <- c(0.01, 0.05, 0.10, 0.20)

relvar <- array(0, dim = c(length(C.rat), 
                           length(unit.RV),
                           length(nbar) ) 
               )
n.opt <- rv.opt <- vector("numeric", length(C.rat))

par(mfrow = c(2,2), mar = c(4,4,2,1), mgp = c(2.5, 0.8, 0))

for (d in delta){

    for (c.rat in 1:length(C.rat)){
       for (u in 1:length(unit.RV)){
          for (n in 1:length(nbar)){
             m <- C.tot / (C1[c.rat] + C2 * nbar[n])
             relvar[c.rat, u, n] <- unit.RV[u] / (m * nbar[n]) *(1 + d*(nbar[n] - 1))
          }
# browser() 
          n.opt[c.rat] <- sqrt(C.rat[c.rat] * (1-d)/d)
          m.opt <- C.tot / (C1[c.rat] + C2 * n.opt[c.rat])
          rv.opt[c.rat] <- unit.RV[u] / (n.opt[c.rat] * m.opt) *(1 + d*(n.opt[c.rat] - 1))
       }
    }

    for (c.rat in 1:length(C.rat)){
#   browser()
            if (c.rat == 1) {   
                     ymin <- min(100 * sqrt(relvar[, 1, ]) )
                     ymax <- max(100 * sqrt(relvar[, 1, ]) )
                     plot(nbar, 100 * sqrt(relvar[c.rat, 1, ]), 
                            lty = 1,
                            type = "l",
                            ylim = c(ymin, ymax),
                            xlab = "nbar (sample units per PSU)",
                            ylab = "CV",
                            cex.lab = 1.2
                      )
                      grid(col="gray25")
                      text( paste("delta = ",d), x = median(nbar), y = ymax - 0.5, cex = 1.2) 
            }
              else lines(nbar, 100 * sqrt(relvar[c.rat, u, ]), 
                      lty = 1)
            points(n.opt[c.rat], 100 * sqrt(rv.opt[c.rat]), pch = 16)
            }
    if (d == delta[1]){
        arrows(x0=5, y0=9, x1=2.4, y1=9, length=0.1)
        text(5.5,9,"C1/C2=3", adj=0)
        arrows(x0=5, y0=7.5, x1=2.35, y1=7.1, length=0.1)
        text(5.5,7.5,"C1/C2=2", adj=0)        
        arrows(x0=7.5, y0=6.5, x1=4, y1=5.7, length=0.1)
        text(8,6.5,"C1/C2=1", adj=0)
    }
}
dev.off()

