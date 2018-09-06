#*****************************************************************************
# FILE:    n.wilson.R                                                         
# PURPOSE: Compute sample size for estimating a proportion using              
#           the Wilson method.                                                
# DATE:    8/20/09                                                            
# AUTHOR:  R. Valliant                                                        
# REVISED: 12/07/09 Modified CI.L and CI.U to avoid recomputing increment.    
#                   Changed formula for n to specify half-width of CI instead 
#                   of full width.                                            
#*****************************************************************************

            # e is half-width in this version
n.wilson <- function(moe.sw,alpha=0.05,pU,e){
    if (!(moe.sw==1 | moe.sw==2))
        stop("moe.sw must equal 1 or 2.\n")
    if (e <=0 | e >= 1) stop("e must be in (0,1).\n")
    if (pU <=0 | pU >= 1) stop("pU must be in (0,1).\n")
    
    za <- qnorm(1-alpha/2)
    qU <- 1-pU
    if (moe.sw == 1){
        rad <- e^2 - pU*qU * (4*e^2 - pU*qU)
    }
    if (moe.sw == 2){
        e <- e*pU
        rad <- e^2 - pU*qU * (4*e^2 - pU*qU)
    }

    n.sam <- (pU*qU - 2*e^2 + sqrt(rad) ) * (za/e)^2 / 2
    
    d <- za*sqrt(za^2 + 4*n.sam*pU*qU)
    CI.L <- (2*n.sam*pU + za^2 - d)/2/(n.sam + za^2)
    CI.U <- (2*n.sam*pU + za^2 + d)/2/(n.sam + za^2)
    leng.CI <- CI.U - CI.L

    list(n.sam=n.sam, "CI lower limit"=CI.L, "CI upper limit"=CI.U, "length of CI" = leng.CI)
}


n.wilson(moe.sw=1, pU=0.2, e=0.05/2)
n.wilson.old(pU=0.2, e=0.05)

n.prop.moe(moe.sw=1, e=0.05/2, alpha=0.05, pU=0.2, N=Inf)

n.wilson(pU=0.05, e=0.01)
n.prop.moe(moe.sw=1, e=0.01/2, alpha=0.05, pU=0.05, N=Inf)

            # Example 3.5
n.wilson(moe.sw =1, pU=0.04, e=0.01)
n.prop.moe(moe.sw=1, e=0.01, alpha=0.05, pU=0.04, N=Inf)


            # check parms
n.wilson(pU=1.05, e=0.01)
n.wilson(pU=0.05, e=1.01)
n.wilson(pU=0.05, e= -0.01)
