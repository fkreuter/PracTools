#****************************************************************************
# FILE:    Example 3.11.R                                                    
# PROJECT: Practical Tools book                                              
# DATE:    08/19/2012                                                        
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Estimate variance parameter gamma in a sample from hospitals pop.
#****************************************************************************

data(hospital)
sam <- c(7, 17, 30, 33, 62, 111, 139, 247, 370, 393)

x <- hospital$x[sam]
y <- hospital$y[sam]


X <- cbind(sqrt(x), x)
gammaFit(X = X, x = x, y = y, maxiter=100, tol=0.001)