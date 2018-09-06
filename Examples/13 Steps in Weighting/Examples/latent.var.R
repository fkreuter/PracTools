##################################################
#  Latent variable cutoff value example
##################################################

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

##################################################
#  Logit, Probit, c-log-log graph
##################################################

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
    lwd = 3,
#    col = "green3")
    col = "gray70")
        
legend("topleft",
       c("logit", "probit", "c-log-log"),
       lty = c(1,2,3),
#       col = c("red", "blue", "green3"),
       col = c("black", "gray40", "gray70"),
       lwd = 2
       )
       
