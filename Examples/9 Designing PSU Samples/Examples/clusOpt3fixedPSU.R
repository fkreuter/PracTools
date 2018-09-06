#************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples   
#               \Examples\clusOpt3fixedPSU.R                                         
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples               
# DATE:     06/24/2011                                                               
# PGMR:     R. Valliant                                                              
# PURPOSE:  Compute optimum values of SSU's and elements per PSU assuming that the   
#           PSU sample is fixed and a 3-stage is used.
#************************************************************************************
 
# unit.cost   = 3-vector of unit costs (C1,C2,C3)
#    C0       = fixed cost that does not depend on number of PSUs, SSUs, or elements
#                   (not used in function)
#    C1       = unit cost per PSU
#    C2       = unit cost per SSU
#    C3       = unit cost per element
# m        = no. of sample PSU's (fixed)
# delta1    = PSU homogeneity measure 
# delta2    = SSU homogeneity measure
# unit.rv  = unit relvariance
# CV0      = target CV
# C.prime = total budget for variable costs, (C-C0)/m - C1 
# cal.sw   = 1, find n for fixed total budget
#            2, find n for target CV0

clusOpt3fixedPSU <- function(unit.cost, m, delta1, delta2, unit.rv, CV0=NULL, tot.cost=NULL, cal.sw){
    if (any(delta1 < 0) | any(delta1 > 1)) stop("delta1 must be in [0,1].\n")
    if (any(delta2 < 0) | any(delta2 > 1)) stop("delta2 must be in [0,1].\n")
    if (!is.null(CV0) & !is.null(tot.cost))
        stop("CV0 and C.prime cannot both be non-null.\n")
    if (is.null(CV0) & is.null(tot.cost))
        stop("CV0 and C.prime cannot both be null.\n")
        
    if (sum(length(m)>1, length(delta1)>1, 
        length(delta2)>1, length(unit.rv)>1, length(CV0)>1, length(tot.cost)>1) > 1)
            stop("Only one argument to function can be vector.\n")

    C1 <- unit.cost[1] 
    C2 <- unit.cost[2]  
    C3 <- unit.cost[3]  
    C.prime <- tot.cost/m - C1
        
    q.opt <- sqrt((1-delta2)/delta2 * C2 / C3)
     
    if (cal.sw == 1){
        n <- C.prime/(C2 + C3*q.opt)
        if (n < 0) stop(paste("n is negative. Check inputs. n=",n,"\n"))

        tot.cost <- C1*m + C2*m*n + C3*m*n*q.opt
        CV <- sqrt(unit.rv/m/n/q.opt * (delta1*n*q.opt + 1 + delta2*(q.opt-1)))
        
        output <- 
           structure(list(C1 = C1,
                          C2 = C2,
                          C3 = C3,
                          m = m,
                          delta1 = delta1,
                          delta2 = delta2,
                          "unit relvar" = unit.rv,
                          "variable budget" = tot.cost-C1*m,
                          "total cost" = round(tot.cost,0),
                          n = round(n,1),
                          q = round(q.opt,1),
                          CV = round(CV,4)),
                     class = "power.htest")
    }
    if (cal.sw == 2) {
        n <- (1 + delta2*(q.opt-1)) / q.opt / (CV0^2*m/unit.rv - delta1)
        if (n < 0) stop(paste("n is negative. Check inputs. n=",n,"\n"))
        
        tot.cost <- C1*m + C2*m*n + C3*m*n*q.opt
        CV.chk <- sqrt(unit.rv/m/n/q.opt * (delta1*n*q.opt + 1+ delta2*(q.opt-1)))

        output <- 
           structure(list(C1 = C1,
                          C2 = C2,
                          C3 = C3,
                          m = m,
                          delta1 = delta1,
                          delta2 = delta2,                          
                          "unit relvar" = unit.rv,
                          "variable budget" = tot.cost-C1*m,
                          "total cost" = round(tot.cost,0),
                          n = round(n,1),
                          q = round(q.opt,1),
                          CV = CV0,
                          "CV check" = round(CV.chk,4)),
                     class = "power.htest")
    }
    output                   
}

    # fixed variable cost
clusOpt3fixedPSU(unit.cost=c(500, 100, 120), m=100, delta1=0.01, delta2=0.05, unit.rv=1, tot.cost=500000,cal.sw=1)      
clusOpt3fixedPSU(unit.cost=c(500, 100, 120), m=100, delta1=0.01, delta2=c(0.05,0.10,0.20), unit.rv=1, tot.cost=500000,cal.sw=1)      

    # fixed CV
clusOpt3fixedPSU(unit.cost=c(500, 100, 120), m=100, delta1=0.01, delta2=0.05, unit.rv=1, CV0=0.05,cal.sw=2)      

clusOpt3fixedPSU(unit.cost=c(500, 100, 120), m=100, delta1=0.01, delta2=0.10, unit.rv=1, CV0=0.05,cal.sw=2)      
clusOpt3fixedPSU(unit.cost=c(5000, 100, 120), m=100, delta1=0.01, delta2=0.10, unit.rv=1, CV0=0.05,cal.sw=2)      

clusOpt3fixedPSU(unit.cost=c(500, 100, 120), m=100, delta1=c(0.01,0.05,0.10), delta2=0.10, unit.rv=1, CV0=0.05,cal.sw=2)      
clusOpt3fixedPSU(unit.cost=c(500, 100, 120), m=100, delta1=c(0.01,0.05,0.10), delta2=0.10, unit.rv=3, CV0=0.05,cal.sw=2)      
