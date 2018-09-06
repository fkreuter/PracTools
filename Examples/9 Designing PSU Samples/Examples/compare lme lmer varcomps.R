#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\compare lme lmer varcomps.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     01/01/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for a 3-stage design
#*********************************************************************************************************

require(lme4)
attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData")
    # create var that is combo of TRACT/BLKGROUP
trtBG <- with(MDarea.pop, 10*TRACT + BLKGROUP)

    # PSUs = TRACTS; SSUs = trtBG
m.y1 <- lmer(y1 ~ (1 | TRACT), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
delta <- as.numeric(tt@REmat[1,3]) / sum(as.numeric(tt@REmat[,3]))
varcomps <- data.frame(tt@REmat)
delta <- data.frame(tt@REmat[1,3]) / sum(data.frame(tt@REmat[,3]))
delta

    # lme code
detach("package:lme4")
require(nlme)
pop.grouped0 <- groupedData(y1 ~ 1| TRACT / BLKGROUP, data = MDarea.pop)
m.y1 <- lme(y1 ~ 1, random = ~1| TRACT / BLKGROUP, data = pop.grouped0)
summary(m.y1)

# Linear mixed-effects model fit by REML
#  Data: pop.grouped0 
#       AIC     BIC   logLik
#   4726225 4726268 -2363108
# 
# Random effects:
#  Formula: ~1 | TRACT
#         (Intercept)
# StdDev:    5.367007
# 
#  Formula: ~1 | BLKGROUP %in% TRACT
#         (Intercept) Residual
# StdDev:    7.446321 83.88288
# 
# Fixed effects: y1 ~ 1 
#                Value Std.Error     DF  t-value p-value
# (Intercept) 69.87496 0.7279979 403690 95.98236       0
# 
# Standardized Within-Group Residuals:
#        Min         Q1        Med         Q3        Max 
# -1.7191076 -0.6077234 -0.2288823  0.4091699 12.7920712 
# 
# Number of Observations: 403997
# Number of Groups: 
#               TRACT BLKGROUP %in% TRACT 
#                  95                 307 
#-------------------------------------------------------------------------------
    # Now, lmer
detach("package:nlme")
require(lme4)

m.y1a <- lmer(y1 ~ (1 | TRACT) + (1 | trtBG), data = MDarea.pop)
summary(m.y1a)
# 
# Linear mixed model fit by REML 
# Formula: y1 ~ (1 | TRACT) + (1 | trtBG) 
#    Data: MDarea.pop 
#      AIC     BIC   logLik deviance REMLdev
#  4726225 4726268 -2363108  4726218 4726217
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  trtBG    (Intercept)   55.454  7.4467 # these results match those above
#  TRACT    (Intercept)   28.803  5.3668 
#  Residual             7036.337 83.8829 
# Number of obs: 403997, groups: trtBG, 307; TRACT, 95
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  69.8744     0.7262   96.22

tt <- summary(m.y1a)
slotNames(tt)
vmat <- data.frame(tt@REmat)
    # numbers in vmat are stored as facotrs. next some very awkward syntax to 
    # to convert them to numbers.
vc <- as.numeric(as.character(vmat[,3]))
del1 <- vc[2] / sum(vc)
del2 <- (vc[1] + vc[2])/ sum(vc)
c(del1, del2)

