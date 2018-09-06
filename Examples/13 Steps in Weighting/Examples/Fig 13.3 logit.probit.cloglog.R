#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\13 Steps in Weighting\
#                Examples\Fig 13.3 logit.probit.cloglog.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     02/19/2012
# PGMR:     R. Valliant
# PURPOSE:  Plot logit, probit, cloglog curves
#**********************************************************************************


#postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig133.eps")
pdf("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig133.pdf")

phi <- seq(0.001, .999, 0.001)
link.logistic <- log(phi / (1-phi)) / (pi/sqrt(3))
link.probit <- qnorm(phi)
link.cloglog <- (log( -log(1 - phi)) + 0.577) / (pi/sqrt(6))

plot(link.logistic, phi,
     type = "l",
     lty = 1,
     lwd = 2,
#     col ="red",
     col ="black",
     ylab = "probability",
     xlab = "standardized link value",
     xlim  = range(link.logistic, link.probit, link.cloglog)
)

lines(link.probit, phi,
    lty = 2,
    lwd = 2,
#    col = "blue")
    col = "gray40")

lines(link.cloglog, phi,
    lty = 3,
    lwd = 4,
#    col = "green3")
    col = "gray40")
grid(col = "grey40", lwd=2)
#grid(col = "black", lwd = 2)

legend("topleft",
       c("logit", "probit", "c-log-log"),
       lty = c(1,2,3),
#       col = c("red", "blue", "green3"),
       col = c("black", "gray40", "gray40"),
       lwd = c(2,2,4),
       bg = "white")

dev.off()
