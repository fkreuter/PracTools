#****************************************************************************
# FILE:    Example 3.14.R
# PROJECT: Practical Tools book
# DATE:    09/26/2010
# AUTHOR:  R. Valliant
# PURPOSE: Compute sample size in smho98 using model-based formula.
#****************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\smho98.RData", pos=2)
require(PracTools)
data(smho98)

CV0 <- 0.15

          #Isolate certainty selections (i.e., size > 2000)
cert <- smho98[,"BEDS"] > 2000

          #Remove certainties and size=0
tmp  <- smho98[!cert, ]
tmp  <- tmp[tmp[,"BEDS"] > 0, ]

          #Create model variables
N <- nrow(tmp)
x <- tmp[,"BEDS"]
y <- tmp[,"EXPTOTAL"]
rtv.bar <- mean(sqrt(x))
v.bar <- mean(x)

          #Object containing results of functions of x modeled on y
m <- glm(y ~ 0 + sqrt(x) + x, weights = 1/x)
          #Model results
summary(m)

        # mean of predicted values = mean(y) in this model


          # calculate V1 based on pp(x) sample
pik <- x[-cert]/sum(x[-cert])
T <- sum(y[-cert])
V1 <- sum( pik*(y[-cert]/pik - T)^2)
V1

n <- V1 / (N*ybarU*CV0)^2
n <- ceiling(n)

pik <- n*x[-cert]/sum(x[-cert])
summary(pik)
#        Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#   0.0007183 0.0181400 0.0445300 0.0761200 0.0847600 0.9747000


N <- nrow(tmp)
m <- glm(y ~ 0 + sqrt(x) + x, weights = 1/x)
ybarU <- mean(y)
S2R <- m$deviance/m$df.residual
S2R
#[1] 1.723118e+12

n <- mean(sqrt(x))^2 / (0.15^2 * ybarU^2 / S2R + mean(x)/N)
n
[1] 33.46152
