#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples 
#               \Examples\clus3.opt.R                                             
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     06/13/2011                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Compute optimal values of number of PSUs, SSUs, and elements per SSU:  
#           m.opt, n.opt, q.opt 3-stage sampling.                                  
#**********************************************************************************
 
# unit.cost   = 3-vector of unit costs (C1,C2,C3)
#    C0       = fixed cost that does not depend on number of PSUs, SSUs, or elements
#    C1       = unit cost per PSU
#    C2       = unit cost per SSU
#    C3       = unit cost per element
# delta1      = PSU homogeneity measure
# delta2      = SSU homogeneity measure
# unit.rv     = unit relvariance
# CV0         = target CV
# tot.cost    = total budget for variable costs, C - C0
# cal.sw      = 1, find optima for fixed total budget
#               2, find optima for target CV0

clus3.opt <- function(unit.cost, delta1, delta2, unit.rv, CV0=NULL, tot.cost=NULL, cal.sw){
    if (length(unit.cost) != 3) stop("cost must be a vector with 3 components")
    if (!is.null(CV0) & !is.null(tot.cost))
        stop("CV0 and tot.cost cannot both be non-null.\n")
    if (sum(length(delta1)>1, length(delta2)>1, length(unit.rv)>1, 
            length(CV0)>1, length(tot.cost)>1) > 1)
            stop("Only one argument to function can be vector.\n")

    if (cal.sw==1 & is.null(tot.cost))
            stop("If cal.sw=1, tot.cost must be non-null.\n")
    if (cal.sw==2 & is.null(CV0))
            stop("If cal.sw=2, CV0 must be non-null.\n")

    C1 <- unit.cost[1] 
    C2 <- unit.cost[2]  
    C3 <- unit.cost[3]  
        
    q.opt <- sqrt( (1-delta2)/delta2 * C2 / C3 )
    n.opt <- 1/q.opt * sqrt((1-delta2)/delta1 * C1 / C3 )

    if (cal.sw == 1){            
        m.opt <- tot.cost / (C1 + C2*n.opt + C3*n.opt*q.opt)
        CV <- sqrt(unit.rv/m.opt/n.opt/q.opt * (delta1*n.opt*q.opt + 1 + delta2*(q.opt-1)))
        cost.chk <- C1*m.opt + C2*m.opt*n.opt + C3*m.opt*n.opt*q.opt

        output <- 
           structure(list(C1 = C1,
                      C2 = C2,
                      C3 = C3,
                      delta1 = delta1,
                      delta2 = delta2,
                      "unit relvar" = unit.rv,
                      budget = tot.cost,
                      "cost check" = cost.chk,
                      m.opt = round(m.opt,1),
                      n.opt = round(n.opt,1),
                      q.opt = round(q.opt,1),
                      CV = round(CV,4)),
                 class = "power.htest")
    }
    if (cal.sw == 2) {
        m.opt <- unit.rv/CV0^2/n.opt/q.opt * (delta1*n.opt*q.opt + 1 + delta2*(q.opt-1))
        CV.chk <- sqrt(unit.rv/m.opt/n.opt/q.opt * (delta1*n.opt*q.opt + 1 + delta2*(q.opt-1)))
        cost.chk <- C1*m.opt + C2*m.opt*n.opt + C3*m.opt*n.opt*q.opt

        output <- 
           structure(list(C1 = C1,
                      C2 = C2,
                      C3 = C3,
                      delta1 = delta1,
                      delta2 = delta2,
                      "unit relvar" = unit.rv,
                      cost  = cost.chk,
                      m.opt = round(m.opt,1),
                      n.opt = round(n.opt,1),
                      q.opt = round(q.opt,1),
                      CV0 = CV0,
                      CV.chk = round(CV.chk,4)),
                 class = "power.htest")
    }   
    output                   
}


        # test cal.sw=1,2
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=1, tot.cost=100000,cal.sw=1)      
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=1, CV0=0.01,cal.sw=2)      
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.03, delta2=0.10, unit.rv=1, CV0=0.01,cal.sw=2)      


        # error check on cal.sw=1 & tot.cost=NULL
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=1, cal.sw=1)      
        # error check on cal.sw=2 & CV0=NULL
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=1, cal.sw=2)      
        # test non-null input for CV0 and tot.cost
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=1, cal.sw=2, CV0=0.05, tot.cost=15000)      

            
        # vector input for one parameter
clus3.opt(unit.cost=c(500, 100, 120), delta1=c(0.01,0.05,0.10), delta2=0.10, unit.rv=2, tot.cost=100000,cal.sw=1)   
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=c(0.01,0.05,0.10), unit.rv=2, tot.cost=100000,cal.sw=1)   
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=c(1,2,5), tot.cost=100000,cal.sw=1)   
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=2, tot.cost=c(100000,250000),cal.sw=1)   

clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=0.10, unit.rv=2, CV0=c(0.01, 0.10),cal.sw=2)   

        # Test error msg for vector input
clus3.opt(unit.cost=c(500, 100, 120), delta1=c(0.01,0.05), delta2=0.10, unit.rv=2, CV0=c(0.01, 0.10),cal.sw=2)   
clus3.opt(unit.cost=c(500, 100, 120), delta1=c(0.01,0.05), delta2=c(0.05,0.10), unit.rv=2, CV0=c(0.01, 0.10),cal.sw=2)   
clus3.opt(unit.cost=c(500, 100, 120), delta1=0.01, delta2=c(0.05,0.10), unit.rv=1:2, CV0=c(0.01, 0.10),cal.sw=2)   
