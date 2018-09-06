#****************************************************************************
# FILE:    n.cont.R                                                          
# PROJECT: Practical Tools book                                              
# DATE:    08/01/09                                                          
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Compute sample sizes for estimating means of continuous variables 
# REVISED: 12/13/09 Changed order of checking whether S2, ybarU, CVpop       
#                   all specified.
#****************************************************************************

n.cont <- function(CV0=NULL, V0=NULL, S2=NULL, ybarU=NULL, N=Inf, CVpop=NULL){
    n.sam <- NULL
    if (sum(sapply(list(CV0, V0, S2, ybarU, CVpop), is.null)) == 5)
        stop("No parameters specified\n")
    
    if (sum(sapply(list(CV0, V0), is.null)) != 1)
        stop("Only one of CV0 and V0 must be specified\n")
    
    if (any(N <= 0, S2 <= 0, CV0 <= 0, V0 <=0)) 
        stop("Neither N nor S2 can be <= 0\n")

    if (sum(sapply(list(S2, ybarU, CVpop), is.null)) == 0){
        cat("S2, ybarU, and CVpop all specified. CVpop ignored.\n")
    }
    
    if (sum(sapply(list(CVpop, N, CV0), is.null)) == 0)
        n.sam <- CVpop^2 / (CV0^2 + CVpop^2/N)

    if (sum(sapply(list(S2, ybarU, N, CV0), is.null)) == 0){
        CVpop <- sqrt(S2)/ybarU
        n.sam <- CVpop^2 / (CV0^2 + CVpop^2/N)  
    }   
     
    if (sum(sapply(list(S2, N, V0), is.null)) == 0)
        n.sam <- S2 / (V0 + S2/N)

    if (is.null(n.sam)) stop("Parameter combination is wrong. Check inputs.\n")
    else n.sam
}


                # N, S2 cannot be negative
n.cont(CV0=0.2, V0=NULL, S2= -10, ybarU=2, N=100, CVpop=NULL)
n.cont(CV0=0.2, V0=NULL, S2=10, ybarU=2, N=-100, CVpop=NULL)
                # mismatched parms
n.cont()
n.cont(V0=NULL, S2=10, ybarU=2, N=100, CVpop=NULL)
n.cont(S2=10, ybarU=2, N=100, CVpop=NULL)
n.cont(CV0=0.2, V0=NULL, S2=10, N=100, CVpop=NULL)

                # CV0, V0, S2, N combo        
n.cont(CV0=0.2, V0=5, S2=8, ybarU=NULL, N=100)
n.cont(CV0=0.2, S2=10, ybarU=2, N=100, CVpop=NULL)
n.cont(CV0=0.2, S2=10, ybarU=2, N=100, CVpop=4)

                # CV0, S2, ybarU, N combo 
n.cont(CV0=0.2, V0=NULL, S2=50, ybarU=2, N=100, CVpop=NULL)
n.cont(CV0=0.2, V0=NULL, S2=50, ybarU=2, N=100, CVpop=4)

                # CV0, CVpop, N combo
n.cont(CV0=0.2, V0=NULL, N=100, CVpop=4)
n.cont(CV0=0.05, N=1000, CVpop=4)
n.cont(CV0=0.05, N=10^6, CVpop=4)
                    # next 2 give same n
n.cont(CV0=0.05, CVpop=4)
n.cont(CV0=0.05, S2=100^2, ybarU=25)

                # V0, N, S2 combo
n.cont(V0=100, N=1000, S2=90^2)
n.cont(V0=100, N=1000, S2=150^2)               

                # Example 3.1
n.cont(CV0=0.05, CVpop=2)
n.cont(CV0=0.05, CVpop=2, N=500)

                # Example 3.2 IRS e = 1.645*CV(T.hat) <= 0.10
n.cont(CV0=0.10/1.645, CVpop=1)

                # Example 3.15 Ratio estimator
attach("C:\\Projects\\Populations\\smho98.RData",pos=2)
cert <- smho98[,"BEDS"] > 2000
tmp <- smho98[!cert, ]
tmp <- tmp[tmp[, "BEDS"] > 0, ]

x <- tmp[,"BEDS"]
y <- tmp[, "EXPTOTAL"]
m <- lm(y ~ 0 + x, weights = 1/x)
ybarU <- mean(y) 
S2R <- sum(m$residuals^2/(length(x)-1))
n.cont(CV0=0.15, S2=S2R, ybarU=ybarU)

m1 <- lm(y ~ 0 + sqrt(x) + x, weights = 1/x)
summary(m)
summary(m1)
