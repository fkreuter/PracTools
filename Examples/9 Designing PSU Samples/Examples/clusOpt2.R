#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples 
#               \Examples\clusOpt2.R                                              
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     06/07/2011                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Compute optimal value of number of elements, n.opt, optimal            
#           value of number of sample PSUs, and CVs in 2-stage sampling.           
#**********************************************************************************

# C1       = unit cost per PSU
# C2       = unit cost per element
#           C1 = cost per PSU
#           C2 = cost per element
# delta    = homogeneity measure
# unit.rv  = unit relvariance or B^2 + W^2
# CV0      = target CV
# tot.cost = total budget for variable costs, C-C0
# cal.sw   = 1, find optimal m.opt for fixed total budget
#            2, find optimal m.opt for target CV0

clusOpt2 <- function(C1, C2, delta, unit.rv, CV0=NULL, tot.cost=NULL, cal.sw){
    options(warn = -1)
    if (!is.null(CV0) & !is.null(tot.cost))
        stop("CV0 and tot.cost cannot both be non-null.\n")
    if (sum(length(C1)>1, length(C2)>1, length(delta)>1, 
        length(unit.rv)>1, length(CV0)>1, length(tot.cost)>1) > 1)
            stop("Only one argument to function can be vector.\n")
 
    c.ratio <- C1/C2    
    n.opt <- sqrt(c.ratio * (1-delta)/delta)
    
    if (cal.sw == 1){
        m.opt <- tot.cost / (C1 + C2*n.opt)
        if (m.opt < 0) stop(paste("m.opt is negative. Check inputs. m.opt=",n,"\n"))

        CV <- sqrt(unit.rv/m.opt/n.opt*(1 + delta*(n.opt-1)))
        output <- 
           structure(list(C1 = C1,
                          C2 = C2,
                          delta = delta,
                          "unit relvar" = unit.rv,
                          budget = tot.cost,
                          m.opt = round(m.opt,1),
                          n.opt = round(n.opt,1),
                          CV = round(CV,4)),
                     class = "power.htest")
    }
    if (cal.sw == 2) {
        m.opt <- unit.rv * (1 + delta*(n.opt-1)) / n.opt / CV0^2
        if (m.opt < 0) stop(paste("m.opt is negative. Check inputs. m.opt=",n,"\n"))

        cost <- C1*m.opt + C2*m.opt*n.opt
        output <- 
           structure(list(C1 = C1,
                          C2 = C2,
                          delta = delta,
                          "unit relvar" = unit.rv,
                          cost = cost,
                          m.opt = round(m.opt,1),
                          n.opt = round(n.opt,1),
                          CV = CV0),
                     class = "power.htest")
    }
    output                   
}

clusOpt2(C1=750, C2=100, delta=0.01, unit.rv=1, tot.cost=100000, cal.sw=1)
clusOpt2(C1=750, C2=100, delta=0.05, unit.rv=1, tot.cost=100000, cal.sw=1)
clusOpt2(C1=750, C2=100, delta=0.01, unit.rv=1, CV0=0.05, cal.sw=2)

        # test non-null input for CV0 and tot.cost
clusOpt2(C1=750, C2=100, delta=0.01, unit.rv=1, CV0=0.05, tot.cost=100000, cal.sw=2)

        # vector input for one parameter
clusOpt2(C1=750, C2=100, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1, tot.cost=100000, cal.sw=1)
clusOpt2(C1=750, C2=250, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1, CV0=0.05, cal.sw=2)
clusOpt2(C1=c(750, 500, 250), C2=250, delta=0.01, unit.rv=1, tot.cost=100000, cal.sw=1)
clusOpt2(C1=750, C2=c(250,100), delta=0.01, unit.rv=1, CV0=0.05, cal.sw=2)
clusOpt2(C1=750, C2=100, delta=0.01, unit.rv=1:3, CV0=0.05, cal.sw=2)
clusOpt2(C1=750, C2=100, delta=0.01, unit.rv=1, CV0=c(0.05, 0.10), cal.sw=2)
clusOpt2(C1=250, C2=250, delta=0.01, unit.rv=1, tot.cost=c(50000,100000), cal.sw=1)

        # Test error msg for vector input
clusOpt2(C1=750, C2=250, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1:3, tot.cost=100000, cal.sw=1)
clusOpt2(C1=c(750, 500), C2=250, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1, tot.cost=100000, cal.sw=1)
clusOpt2(C1=c(750, 500), C2=250, delta=0.01, unit.rv=1, tot.cost=c(50,100000), cal.sw=1)
