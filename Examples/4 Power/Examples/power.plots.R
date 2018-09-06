#*********************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\power.plots.R   
# PROJECT: Practical Tools for Designing and Weighting Sample Surveys             
# PURPOSE: Plot power curves                                                      
# DATE:    07/29/07                                                               
# AUTHOR:  R. Valliant                                                            
# REVISED: 08/08/10 Color changed to gray-scale.                                  
#*********************************************************************************

# Q-Q plot with reference line

require(MASS)

par(mfrow = c(2,1), mar = c(3,4,2,1) )
truehist(precip, 
    main = "Precipitation [in/yr] for 70 US cities", cex.title = 1,
    cex.main = 0.8)
qqnorm(precip, main = "Normal Q-Q plot", 
    ylab = "Precipitation [in/yr] for 70 US cities",
    cex.main = 0.8,

    pch=16)
qqline(precip)

#   Plots of different std normals
par(mfrow = c(2,1), mar = c(3,4,2,1) )
x <- seq(-10,10,0.1)
plot(x, dnorm(x),
    type = "l",
    lwd = 2,
    main = "Normal densities with variances 1, 2, 3",
    cex.main = 0.8,
    ylab = "")

lines(x, dnorm(x,0,2),
    col = "red",
    lwd = 2)
    
lines(x, dnorm(x,0,3),
    col = "blue",
    lwd = 2)

# t densities overlaid on N(0,1)    
x <- seq(-3,3,0.1)
plot(x, dnorm(x),
    type = "b",
    lwd = 2,
    main = "N(0,1) and t densities with df = 5 and 30\nNormal in circles",
    cex.main = 0.8,
    ylab = "")

lines(x, dt(x, 5),
    col = "red",
    lwd = 2)
    
lines(x, dt(x, 30),
    col = "royalblue",
    lwd = 2,
    lty = 1)
    

#   Plots of different chi square densities
par(mfrow = c(2,1), mar = c(3,4,2,1) )
x <- seq(0, 50, 0.1)
plot(x, dchisq(x, df=2),
    type = "l",
    lwd = 2,
    main = "Chi square densities with df = 2, 5, 10, and 30",
    cex.main = 0.8,
    ylab = "")

lines(x, dchisq(x, df=5),
    col = "red",
    lwd = 2,)
    
lines(x, dchisq(x, df=10),
    col = "blue",
    lwd = 2)
    
lines(x, dchisq(x, df=30),
    col = "hotpink",
    lwd = 2)
    
        
#   F densities 
x <- seq(0, 6, 0.05)
plot(x, df(x, df1=2, df2=2),
    type = "l",
    lwd = 2,
    main = "F densities with df = (2,2), (5,5), (10,10), and (30,30)",
    cex.main = 0.8,
    ylab = "",
    ylim = range(df(x, df1=30, df2=30)) )

lines(x, df(x, df1=5, df2=5),
    col = "red",
    lwd = 2,)
    
lines(x, df(x, df1=10, df2=10),
    col = "blue",
    lwd = 2)
    
lines(x, df(x, df1=30, df2=30),
    col = "hotpink",
    lwd = 2)
    
#______________________________________________________________________
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
    col="gray90")
    
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
    col="gray50")
    
lines(x, dnorm(x,mu1,1),
#    col = "red",
    col = "gray10",
    lwd = 3)

abline(v = 3, lty=2)

        # see ?plotmath for details on math text printing
text(-2, 0.28, substitute(italic(H[0])),
     cex = 1.5)
#     cex = 1.5, family="serif")
text(-1, 0.12, substitute(italic(mu==mu[o])),
     cex = 1.5)
#     cex = 1.5, family="serif")
text(5, 0.28, substitute(italic(H[A])),
     cex = 1.5)
#     cex = 1.5, family="serif")
text(3, 0.12, substitute(italic(mu==mu[o]+delta)),
     cex = 1.5)
#     cex = 1.5, family="serif")
     
dev.off()
