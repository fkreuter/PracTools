#****************************************************************************
# FILE:    n.prop.R                                                          
# PROJECT: Practical Tools book                                              
# DATE:    08/01/09                                                          
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Compute sample sizes for proportions                              
# REVISED:                                                                   
#****************************************************************************

n.prop <- function(CV0=NULL, V0=NULL, pU=NULL, N=Inf){
    n.sam <- NULL
    if (sum(sapply(list(N, pU), is.null) != 0))
        stop("N and pU cannot be NULL.\n")
    if (sum(sapply(list(CV0, V0), is.null)) != 1)
        stop("Either CV0 or V0 must be specified.\n")
    if (any(pU <= 0) | any(pU >= 1)) stop("pU must be in (0,1).\n")

    if (any(N <= 0, CV0 <= 0, V0 <=0)) 
        stop("N, CV0, and V0 cannot be <= 0.\n")
    
    if (sum(sapply(list(pU, N, CV0), is.null)) == 0){
        if (N == Inf) {a <- 1}
            else {a <- N/(N-1)}
        qU <- 1-pU
        n.sam <- a * qU/pU / (CV0^2 + qU/pU/(N-1))
    }
    if (sum(sapply(list(pU, N, V0), is.null)) == 0){
        if (N == Inf) {a <- 1}
            else {a <- N/(N-1)}
        qU <- 1-pU
        n.sam <- a * pU*qU / (V0 + pU*qU/(N-1))
    }
    
    if (is.null(n.sam)) stop("Parameter combination is wrong. Check inputs.\n")
    else n.sam
}

                # N cannot be NULL
n.prop(CV0=0.2, V0=NULL)
                # N, CV0, V0 cannot be negative
n.prop(CV0=-0.05, pU=0.1, N=1000)
n.prop(V0=-0.05, pU=0.1, N=1000)
n.prop(CV0=0.05, pU=0.1, N=-1000)

                # mismatched parms
n.prop()
n.prop(CV0=0.2, V0=0.25, N=100)
n.prop(pU=0.5, N=100)

                # CV0, N, p combo        
                # Fig. 3.1                
n.prop(CV0=0.05, pU=0.1, N=Inf)
n.prop(CV0=0.05, pU=0.5, N=Inf)
n.prop(CV0=0.05, pU=seq(0.1, 0.9, 0.1), N=Inf)

                # Example 3.2
n.prop(V0=0.0005^2, N=Inf, pU=0.01)
n.prop(CV0=0.05, N=Inf, pU=0.01)    # same as n.prop(V0=0.0005^2, N=Inf, pU=0.01)
n.prop(V0=(0.005/1.96)^2, N=Inf, pU=0.01)

                # vector input
n.prop(CV0=0.05, pU=seq(0.05, 0.95, 0.05), N=Inf)
