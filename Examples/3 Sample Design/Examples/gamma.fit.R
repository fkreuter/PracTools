#****************************************************************************
# FILE:    gamma.fit.R                                                       
# PROJECT: Practical Tools book                                              
# DATE:    08/01/09                                                          
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Estimate gamma in a model E(Y) = X%*%b, V(Y) = sigma2 * (x^gamma) 
#          gamma found by iteratively calling gam.est                        
# REVISED:                                                                   
#****************************************************************************

# PARAMETERS:                                        
#       X         = matrix of predictors               
#       x         = vector of x's in V(Y)              
#       y         = vector of response variables       
#       maxiter   = maximum no. of iterations allowed  
#       show.iter = show values of gamma at each iteration, TRUE or FALSE
#       tol       = relative change in gamma used to judge convergence

gam.est <- function(X1, x1, y1, v1){
    tmp <- lsfit(X1, y1, wt = 1/v1, intercept = FALSE)
    beta1 <- lsfit(log(x1), log(tmp$residuals^2))$coef
    g1 <- beta1["X"]
    g1
}


gamma.fit <- function(X, x, y, maxiter=100, show.iter=FALSE, tol=0.001){
    g.old <- 1
    converged <- FALSE
    step <- 1
    v <- rep(1, length(x))

    while(!converged & (step <= maxiter)) {
        g.hat <- gam.est(X, x, y, v)
        if (show.iter) cat("step",step,"gamma = ",g.hat,"\n")
        v <- x^g.hat
        if(abs((g.old - g.hat)/g.old) < tol) {converged <- TRUE}
        g.old <- g.hat
        v <- x^g.hat
        step <- step+1
    }

    if ((step >= maxiter) & !converged){
        cat("Maximum no. of iterations reached without convergence.\n")
        cat("g.hat = ", g.hat, "\n")
    }
    else{
        cat("Convergence attained in ", step-1, "steps.\n")
        cat("g.hat =", g.hat, "\n")
    }
}


#___________________________________________________________________________________________
# Test the functions

attach("C:\\Projects\\Populations\\hospital.RData", pos=2)
x <- hospital[, "x"]
X <- cbind(x, x^2)
y <- hospital[, "y"]

                # full pop value
gamma.fit(X = X, x = x, y = y, maxiter=100, show.iter=TRUE, tol=0.001)

save.seed <- .Random.seed
set.seed(3)
sam <- sample(1:nrow(hospital), 30)
x <- hospital[sam, "x"]
X <- cbind(x, x^2)
y <- hospital[sam, "y"]
gamma.fit(X = X, x = x, y = y, maxiter=100, tol=0.001)
plot(x,y, pch=16)
sort(sam)

set.seed(13)
sam <- sample(1:nrow(hospital), 10)
x <- hospital[sam, "x"]
X <- cbind(x, x^2)
y <- hospital[sam, "y"]
gamma.fit(X = X, x = x, y = y, maxiter=100, tol=0.001)
plot(x,y, pch=16)
sort(sam)

set.seed(61)
sam <- sample(1:nrow(hospital), 10)
x <- hospital[sam, "x"]
X <- cbind(sqrt(x), x)
# X <- cbind(x, x^2)
y <- hospital[sam, "y"]
gamma.fit(X = X, x = x, y = y, maxiter=100, show.iter=TRUE, tol=0.001)

tmp <- glm(y ~ 0 + X, weights = 1/(x^1.88))
par(mgp = c(2, 0.5, 0))
plot(x,y, pch=16,
    xlim = c(0,max(x)),
    ylim = c(0,max(y, predict(tmp)))
    )
lines(sort(x), sort(predict(tmp)))
sort(sam)
