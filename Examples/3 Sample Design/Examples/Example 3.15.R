#****************************************************************************
# FILE:    Example 3.15.R                                                    
# PROJECT: Practical Tools book                                              
# DATE:    08/20/2012                                                        
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Sample size in smho98 for ratio model
#****************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\smho98.RData", pos=2)

          #Isolate certainty selections (i.e., size > 2000)
cert <- smho98[,"BEDS"] > 2000

          #Remove certainties and size=0
tmp  <- smho98[!cert, ]
tmp  <- tmp[tmp[,"BEDS"] > 0, ]

x <- tmp[,"BEDS"]
y <- tmp[,"EXPTOTAL"]

m <- glm(y ~ 0 + x, weights = 1/x)
ybarU <- mean(y)
S2R <- sum(m$residuals^2/(length(x)-1))
S2R
[1] 2.078e+14
 
nCont(CV0=0.15, S2=S2R, ybarU=ybarU, N=670)
[1] 51.16394

