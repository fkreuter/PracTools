#*********************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Fig 4.1 power plot.R   
# PROJECT: Practical Tools for Designing and Weighting Sample Surveys             
# PURPOSE: Plot power curves                                                      
# DATE:    02/19/2012                                                               
# AUTHOR:  R. Valliant                                                            
#*********************************************************************************
#   Power illustration

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\4 Power\\Examples\\Fig 4.1.eps")
#           width = 4.0, height = 3.0,
#           horizontal = FALSE, onefile = TRUE, paper = "special",
#           family = "ComputerModern", encoding = "TeXtext.enc"


x <- seq(-3,6,0.1)
mu1 <- 3
plot(x, dnorm(x),
    type = "l",
    lwd = 2,
#   main = "Normal densities under H0 and HA",
    cex.main = 0.8,
    ylab = "",
    xlab = "Z",
    xlim = c(-3,3 + mu1) )

abline(v = 0, lty=2)
    
#   Color the rejection region under H0 yellow
#   Righthand side
polygon(x=c(seq(1.645,4,0.1), 
        rep(1.645,5),
        seq(1.645,4,0.1)), 
    y=c(rep(0, length(seq(1.645,4,0.1))),
        seq(0, dnorm(1.645), dnorm(1.645)/4),
        dnorm(seq(1.645,4,0.1)) ), 
#    col="yellow")
    col="gray85")
    
#   Lefthand side--for 2-tailed
#polygon(x=c(seq(-1.96,-4,-0.1), 
#       rep(-1.96,5),
#       seq(-1.96,-4,-0.1)), 
#   y=c(rep(0, length(seq(-1.96,-4,-0.1))),
#       seq(0, dnorm(-1.96), dnorm(-1.96)/4),
#       dnorm(seq(-1.96,-4,-0.1)) ), 
#   col="yellow")

            
xx1 <- seq(1.645,6,0.1)
xx1a <- c(6,6)              # close the righthand end
xx2 <- rev(seq(1.645,6,0.1))
xx3 <- rep(1.645, 6)            # close the lefthand end
yy1 <- dnorm(seq(1.645,6,0.1))
yy1a <- c(dnorm(6), dnorm(6,mu1,1))     # close the righthand end
yy2 <- rev(dnorm(seq(1.645,6,0.1),mu1,1))
yy3 <- c(dnorm(1.645), 
        seq(dnorm(1.645), dnorm(1.645,mu1,1), 
                ( dnorm(1.645,mu1,1) - dnorm(1.645) )/4)
        )               # close the lefthand end

#   Power area
polygon(x = c(xx1, xx1a, xx2, xx3),
    y = c(yy1, yy1a, yy2, yy3),
#    col="green")
    col="gray70")
    
lines(x, dnorm(x,mu1,1),
#    col = "red",
    col = "gray30",
    lwd = 3)

abline(v = 3, lty=2)

        # see ?plotmath for details on math text printing
text(-2, 0.28, substitute(italic(H[0])),
     cex = 1.5)
        # family = "serif" not available in postscript
#     cex = 1.5, family="serif")
text(-1, 0.12, substitute(italic(mu==mu[0])),
     cex = 1.5)
#     cex = 1.5, family="serif")
text(5, 0.28, substitute(italic(H[A])),
     cex = 1.5)
#     cex = 1.5, family="serif")
text(3, 0.12, substitute(italic(mu==mu[0]+delta)),
     cex = 1.5)
#     cex = 1.5, family="serif")
     
dev.off()
