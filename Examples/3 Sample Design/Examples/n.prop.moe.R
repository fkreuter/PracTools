#****************************************************************************
# FILE:    n.prop.moe.R                                                      
# PROJECT: Practical Tools book                                              
# DATE:    08/10/09                                                          
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Compute sample sizes for proportions based on specified margins   
#          of error.                                                         
# REVISED:                                                                   
#****************************************************************************

n.prop.moe <- function(moe.sw, e, alpha=0.05, pU, N=Inf){
    n.sam <- NULL

    if (!(moe.sw==1 | moe.sw==2))
        stop("moe.sw must equal 1 or 2.\n")
    if (alpha <= 0 | alpha >= 1)
        stop("alpha must be in (0,1).\n")            
    if (sum(sapply(list(e, N, pU), is.null) != 0))
        stop("e, N, and pU cannot be NULL.\n")
    if (any(pU <= 0) | any(pU >= 1)) stop("pU must be in (0,1).\n")
    if (N <= 0) stop("N must be positve.\n")
    
    if (N == Inf) {a <- 1}
            else {a <- N/(N-1)}
            
    z <- qnorm(1 - alpha/2)
    qU <- 1-pU 
    
    if (moe.sw==1){
        n.sam <- a * z^2 *pU*qU / (e^2 + z^2*pU*qU/(N-1) )
    }
    
    if (moe.sw==2){
        n.sam <- a * z^2 * qU/pU / (e^2 + z^2*qU/pU/(N-1) )
    }
    
    if (is.null(n.sam)) stop("Parameter combination is wrong. Check inputs.\n")
    else n.sam
}

                # moe.sw test
n.prop.moe(moe.sw=1, e=0.05, pU=0.2)
               
                # parms cannot be negative
n.prop.moe(moe.sw= -1, e=0.05, pU=0.1, N=1000)
n.prop.moe(moe.sw=1, e=0.05, pU= -0.1, N=1000)
n.prop.moe(moe.sw=1, e=0.05, pU=0.1, N= -1000)

                # mismatched parms
n.prop.moe()
n.prop.moe(moe.sw=1, pU=0.1, N=1000)

                # legal combos
n.prop.moe(moe.sw=1, e=0.05, pU=0.1, N= 1000)
n.prop.moe(moe.sw=2, e=0.05, pU=0.1, N= 1000)

                # Example 3.4
n.prop.moe(moe.sw=1, e=seq(0.01,0.08,0.01), alpha=0.05, pU=0.5, N=Inf)
n.prop.moe(moe.sw=2, e=0.01, alpha=0.05, pU=0.5, N=Inf)

n.prop.moe(moe.sw=1, e=0.01, alpha=0.05, pU=seq(0.05,0.95,0.05), N=Inf)
