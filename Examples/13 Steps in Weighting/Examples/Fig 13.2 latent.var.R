#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\13 Steps in Weighting\ 
#                Examples\Fig 13.2 latent.var.R                                          
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     02/19/2012                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Latent variable cutoff value example
#**********************************************************************************

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig 13.2.eps")

x <- seq(0, 8, 0.1)
dval <- dnorm(x, m = 4)
plot(x, dval, 
     type = "l",
     xlab = "R*",
     ylab = "density",
     font.lab = 3)
     
abline(v = 5)

text("R = 0", x = 3, y = 0.04)
text("R = 1", x = 5.7, y = 0.03)

xx <- seq(5, 8, 0.1)
yy <- c(dval[x >= 5])
polygon(x = c(xx, rev(xx)),
        y = c(yy, rep(0, length(xx))),
        col = "gray",
        border = "black",
        lwd = 2
        )

text("R = 0", x = 3, y = 0.04)
text("R = 1", x = 5.7, y = 0.03)
dev.off()



