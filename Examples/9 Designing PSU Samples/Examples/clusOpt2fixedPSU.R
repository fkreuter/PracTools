#************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples   
#               \Examples\clusOpt2fixedPSU.R                                         
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples               
# DATE:     06/24/2011                                                               
# PGMR:     R. Valliant                                                              
# PURPOSE:  Compute values of elements per PSU assuming that the PSU sample is fixed 
#           and a 2-stage sample is selected.                                        
#************************************************************************************
 
# C1       = unit cost per PSU
# C2       = unit cost per element
#           C1 = cost per PSU
#           C2 = cost per element
# m        = no. of sample PSU's (fixed)
# delta    = homogeneity measure
# unit.rv  = unit relvariance
# CV0      = target CV
# tot.cost = total budget for variable costs, (C-C0)/m - C1 
# cal.sw   = 1, find n for fixed total budget
#            2, find n for target CV0

clusOpt2fixedPSU <- function(C1, C2, m, delta, unit.rv, CV0=NULL, tot.cost=NULL, cal.sw){
    if (any(delta < 0) | any(delta > 1)) stop("delta must be in [0,1].\n")
    if (!is.null(CV0) & !is.null(tot.cost))
        stop("CV0 and tot.cost cannot both be non-null.\n")
    if (is.null(CV0) & is.null(tot.cost))
        stop("CV0 and tot.cost cannot both be null.\n")
        
    if (sum(length(C1)>1, length(C2)>1, length(m)>1, length(delta)>1, 
        length(unit.rv)>1, length(CV0)>1, length(tot.cost)>1) > 1)
            stop("Only one argument to function can be vector.\n")
    
    if (cal.sw == 1){
        n <- (tot.cost - C1*m)/C2/m
        if (n < 0) stop(paste("n is negative. Check inputs. n=",n,"\n"))
        CV <- sqrt(unit.rv/m/n*(1 + delta*(n-1)))
        output <- 
           structure(list(C1 = C1,
                          C2 = C2,
                          m = m,
                          delta = delta,
                          "unit relvar" = unit.rv,
                          budget = tot.cost,
                          n = round(n,1),
                          CV = round(CV,4)),
                     class = "power.htest")
    }
    if (cal.sw == 2) {
        n <- (1 - delta) / (CV0^2*m/unit.rv - delta)
        if (n < 0) stop(paste("n is negative. Check inputs. n=",n,"\n"))
        cost <- C1*m + C2*m*n
        output <- 
           structure(list(C1 = C1,
                          C2 = C2,
                          delta = delta,
                          m = m,
                          "unit relvar" = unit.rv,
                          cost = cost,
                          n = round(n,1),
                          CV = CV0),
                     class = "power.htest")
    }
    output                   
}

clusOpt2fixedPSU(C1=500, C2=100, m=100, delta=0.05, unit.rv=2, CV0=NULL, tot.cost=c(100000, 500000, 10^6), cal.sw=1)

    # vector input; fixed cost
clusOpt2fixedPSU(C1=c(500,1000,5000), C2=100, m=100, delta=0.05, unit.rv=2, CV0=NULL, tot.cost=10^6, cal.sw=1)
clusOpt2fixedPSU(C1=5000, C2=c(100,200,500), m=100, delta=0.05, unit.rv=2, CV0=NULL, tot.cost=10^6, cal.sw=1)

    # fixed CV0
clusOpt2fixedPSU(C1=c(500,1000,5000), C2=100, m=100, delta=0.05, unit.rv=2, CV0=0.05, tot.cost=NULL, cal.sw=2)
clusOpt2fixedPSU(C1=5000, C2=100, m=100, delta=0.05, unit.rv=2, CV0=c(0.05,0.10, 0.20), tot.cost=NULL, cal.sw=2)
clusOpt2fixedPSU(C1=5000, C2=100, m=100, delta=0.05, unit.rv=2, CV0=c(0.05,0.10), tot.cost=NULL, cal.sw=2)
clusOpt2fixedPSU(C1=500, C2=100, m=100, delta=0.05, unit.rv=2, CV0=c(0.05,0.10), tot.cost=NULL, cal.sw=2)

    # Error checks
clusOpt2fixedPSU(C1=5000, C2=c(100,200,500), m=100, delta=0.05, unit.rv=2, CV0=NULL, tot.cost=NULL, cal.sw=2)
clusOpt2fixedPSU(C1=5000, C2=100, m=100, delta=c(0.05,0.10,0.20), unit.rv=2, CV0=c(0.05,0.10), tot.cost=NULL, cal.sw=2)
clusOpt2fixedPSU(C1=c(500,1000,5000), C2=100, m=100, delta=6, unit.rv=2, CV0=NULL, tot.cost=10^6, cal.sw=1)
clusOpt2fixedPSU(C1=c(500,1000,5000), C2=100, m=100, delta=-1, unit.rv=2, CV0=NULL, tot.cost=10^6, cal.sw=1)
clusOpt2fixedPSU(C1=5000, C2=100, m=100, delta=0.05, unit.rv=2, CV0=NULL, tot.cost=c(5*10^4, 10^5, 5*10^5), cal.sw=1)
