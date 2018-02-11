#***************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\3 Sample Design\Examples\Fig 3.1 CV plot.R                                                    
# PROJECT: Practical Tools book                                              
# DATE:    03/21/2012                                                        
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Plot sample sizes to achieve CV=0.05, 0.10 for proportions
#***************************************************************************************************

n.cv <- function(cv0, p){
    n <- (1-p) / p / cv0^2
    n
}

p0 <- seq(0.1, 0.9, 0.01)
n.05 <- n.cv(0.05, p = p0)

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\3 Sample Design\\Examples\\Fig 3.1.eps")

plot(p0, n.05, 
     type = "l", 
     lty = 1,
     font.lab = 3,
     xlab = quote(p[U]),
     ylab = "n",
     cex.lab = 1.5,
     cex.axis = 1.2)
     
   
abline(h = seq(0, 4000, 500),
       v = seq(0, 1, 0.05),
       col = "gray70",
       lty = 1)

n.10 <- n.cv(0.10, p = p0)

lines(p0, n.10, type = "l", lty = 1)

text(x = 0.25, y = 1500, "cv = 0.05",
     adj = 0,
     cex = 1.5)
text(x = 0.20, y = 500, "cv = 0.10",
     adj = 0,
     cex = 1.5)

dev.off()