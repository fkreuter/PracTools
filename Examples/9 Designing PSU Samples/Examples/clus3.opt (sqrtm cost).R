#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples 
#               \Examples\clus3.opt.R                                             
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     06/13/2011                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Compute optimal values of number of PSUs, SSUs, and elements per SSU:  
#           m.opt, n.opt, q.opt 3-stage sampling.                                  
#**********************************************************************************

# unit.cost = 5-vector of unit costs
#    C0       = travel cost between PSUs
#    C1       = unit cost per PSU
#    C2       = unit cost per SSU
#    C3       = travel cost between SSUs
#    C4       = unit cost per element
# delta1    = PSU homogeneity measure
# delta2    = SSU homogeneity measure
# unit.rv   = unit relvariance
# CV0       = target CV
# tot.cost  = total budget for variable costs, C
# cal.sw    = 1, find optima for fixed total budget
#             2, find optima for target CV0
# n.init    = initial value for n.bar
# max.iter  = maximum no. of iterations allowed
# show.iter = show values of gamma at each iteration, TRUE or FALSE
# tol       = relative change in sample sizes used to judge convergence


clus3.opt <- function(unit.cost, delta1, delta2, unit.rv, CV0=NULL, tot.cost=NULL, 
                        cal.sw, n.init=NULL, max.iter=100, show.iter=FALSE, tol=0.001){
browser()
    if (length(unit.cost) != 5) stop("cost must be a vector with 5 components")
    if (!is.null(CV0) & !is.null(tot.cost))
        stop("CV0 and tot.cost cannot both be non-null.\n")
#    if (sum(length(delta1)>1, length(delta2)>1, length(unit.rv)>1, 
#            length(CV0)>1, length(tot.cost)>1) > 1)
#            stop("Only one argument to function can be vector.\n")
    if (sum(length(delta1)>1 | length(delta2)>1 | length(unit.rv)>1 |
            length(CV0)>1 | length(tot.cost)>1) > 1)
            stop("None of these parameters can be vector: delta1, delta2, unit.rv, CV0, tot.cost.\n")

    if (cal.sw==1 & is.null(tot.cost))
            stop("If cal.sw=1, tot.cost must be non-null.\n")
    if (cal.sw==2 & is.null(CV0))
            stop("If cal.sw=2, CV0 must be non-null.\n")

        # scale unit costs and total cost by dividing by C4
    scalefac <- unit.cost[5]
    unit.cost <- unit.cost/unit.cost[5]
    C0 <- unit.cost[1]
    C1 <- unit.cost[2] 
    C2 <- unit.cost[3]  
    C3 <- unit.cost[4]  
    C4 <- unit.cost[5]
    
    if (is.null(n.init)) {n.opt <- 10}
    else {n.opt <- n.init}
    
    converged <- FALSE
    step <- 1
    k <- C0^2/tot.cost
    sams.old <- rep(1,3)
    
    if (cal.sw == 1){
        while(!converged & (step <= max.iter)) {
            q.opt <- sqrt( (1-delta2)/delta2 * (C3/2/sqrt(n.opt) + C2) )
            a <- k/4 + sqrt( k/4 * (C3*sqrt(n.opt) + C2*n.opt + n.opt*q.opt + C1 + k/4) )
            n.opt <- 1/q.opt * sqrt((1-delta2)/delta1 * (C3*sqrt(n.opt)/2 + C1 + a))
            m.opt <- (C0/2/a)^2
            
            cost.chk <- C0*sqrt(m.opt) + C1*m.opt + C2*m.opt*n.opt + C3*m.opt*sqrt(n.opt) + C4*m.opt*n.opt*q.opt
            cost.chk <- cost.chk * scalefac
            if (show.iter) cat("step",step,"m.opt = ",round(m.opt,2)," n.opt =", round(n.opt,2),
                    " q.opt =",round(q.opt,2), "a =",round(a,2), " cost.chk =", cost.chk, "\n")
            sams <- c(m.opt, n.opt, q.opt)
            if(all(abs((sams.old - sams)/sams.old) < tol)) {converged <- TRUE}
            
            sams.old <- c(m.opt, n.opt, q.opt)
            step <- step+1
        }

        if ((step >= max.iter) & !converged){
            cat("Maximum no. of iterations reached without convergence.\n")
            cat("Current values: \n  m.opt = ", m.opt, "\n",
                    "n.opt = ", n.opt, "\n",
                    "q.opt = ", q.opt)
        }
        else{
            cat("Convergence attained in ", step-1, "steps.\n")
        }
            
    CV <- sqrt(unit.rv/m.opt/n.opt/q.opt * (delta1*n.opt*q.opt + 1 + delta2*(q.opt-1)))
    cost.chk <- C0*sqrt(m.opt) + C1*m.opt + C2*m.opt*n.opt + C3*m.opt*sqrt(n.opt) + C4*m.opt*n.opt*q.opt
    cost.chk <- cost.chk * scalefac        

    output <- 
           structure(list(C0 = C0*scalefac,
                          C1 = C1*scalefac,
                          C2 = C2*scalefac,
                          C3 = C3*scalefac,
                          C4 = C4*scalefac,
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
        while(!converged & (step <= max.iter)) {
            q.opt <- sqrt( (1-delta2)/delta2 * (C3/2/sqrt(n.opt) + C2) )
            a <- CV0*C0 / sqrt(unit.rv) / sqrt(delta1 + delta2/n.opt + (1-delta2)/n.opt/q.opt)
            n.opt <- 1/q.opt * sqrt((1-delta2)/delta1 * (C3*sqrt(n.opt)/2 + C1 + a))
            m.opt <- (C0/2/a)^2
            
            CV <- sqrt(unit.rv/m.opt/n.opt/q.opt * (delta1*n.opt*q.opt + 1 + delta2*(q.opt-1)))
            if (show.iter) cat("step",step,"m.opt = ",round(m.opt,2)," n.opt =", round(n.opt,2),
                    " q.opt =",round(q.opt,2), "a =",round(a,2), " CV =", CV, "\n")
            sams <- c(m.opt, n.opt, q.opt)
            if(all(abs((sams.old - sams)/sams.old) < tol)) {converged <- TRUE}
            
            sams.old <- c(m.opt, n.opt, q.opt)
            step <- step+1
        }

        if ((step >= max.iter) & !converged){
            cat("Maximum no. of iterations reached without convergence.\n")
            cat("Current values: \n  m.opt = ", m.opt, "\n",
                    "n.opt = ", n.opt, "\n",
                    "q.opt = ", q.opt)
        }
        else{
            cat("Convergence attained in ", step-1, "steps.\n")
        }
            
    CV <- sqrt(unit.rv/m.opt/n.opt/q.opt * (delta1*n.opt*q.opt + 1 + delta2*(q.opt-1)))
    cost.chk <- C0*sqrt(m.opt) + C1*m.opt + C2*m.opt*n.opt + C3*m.opt*sqrt(n.opt) + C4*m.opt*n.opt*q.opt
    cost.chk <- cost.chk * scalefac

    output <- 
           structure(list(C0 = C0*scalefac,
                          C1 = C1*scalefac,
                          C2 = C2*scalefac,
                          C3 = C3*scalefac,
                          C4 = C4*scalefac,
                          delta1 = delta1,
                          delta2 = delta2,
                          "unit relvar" = unit.rv,
                          "cost" = cost.chk,
                          m.opt = round(m.opt,1),
                          n.opt = round(n.opt,1),
                          q.opt = round(q.opt,1),
                          CV = round(CV,4)),
                     class = "power.htest")
    }
    
    output                   
}

clus3.opt(unit.cost=c(200, 500, 100, 25, 100), delta1=0.5, delta2=0.010, unit.rv=1, tot.cost=100000,
            cal.sw=1, n.init=NULL, max.iter=20, show.iter=TRUE, tol=0.001)      
clus3.opt(unit.cost=c(200/100, 500/100, 100/100, 100/100, 100/100), 
            delta1=0.01, delta2=0.10, unit.rv=1, tot.cost=100000/100,
            cal.sw=1, n.init=NULL, max.iter=20, show.iter=TRUE, tol=0.001)      
clus3.opt(unit.cost=c(500, 200, 100, 50, 50), 
            delta1=0.01, delta2=0.10, unit.rv=1, CV0=0.05,
            cal.sw=2, n.init=NULL, max.iter=20, show.iter=TRUE, tol=0.001)      

        # error check on cal.sw=1 & tot.cost=NULL
clus3.opt(unit.cost=c(500, 200, 100, 50, 50), 
            delta1=0.01, delta2=0.10, unit.rv=1, CV0=0.05,
            cal.sw=1, n.init=NULL, max.iter=20, show.iter=TRUE, tol=0.001)      



        # Illustration from HHM, Vol. I, p.406
clus3.opt(unit.cost=c(500, 20, 3, 7, 2), delta1=0.01, delta2=0.09, unit.rv=2, tot.cost=15000,
            cal.sw=1, n.init=9, max.iter=20, show.iter=TRUE, tol=0.001)   
        # HHM with 1/2 costs
        # this agrees with HHM, Vol. I, p.406 except for m.opt. HHM gets 56, I get 59. They have some roundoff error
clus3.opt(unit.cost=c(500/2, 20/2, 3/2, 7/2, 2/2), delta1=0.01, delta2=0.09, unit.rv=2, tot.cost=15000/2,
            cal.sw=1, n.init=9, max.iter=20, show.iter=TRUE, tol=0.001)      
        # what if we double the cost of everything?   
clus3.opt(unit.cost=c(500*2, 20*2, 3*2, 7*2, 2*2), delta1=0.01, delta2=0.09, unit.rv=2, tot.cost=15000*2,
            cal.sw=1, n.init=9, max.iter=20, show.iter=TRUE, tol=0.001)      


        # test non-null input for CV0 and tot.cost
clus3.opt(unit.cost=c(500, 20, 3, 7, 2), delta1=0.01, delta2=0.09, unit.rv=2, CV0=0.05, tot.cost=15000,
            cal.sw=1, n.init=9, max.iter=20, show.iter=TRUE, tol=0.001)   
            
        # vector input for one parameter
clus3.opt(unit.cost=c(500, 20, 3, 7, 2), delta1=c(0.01,0.05,0.10), delta2=0.09, unit.rv=2, tot.cost=15000,
            cal.sw=1, n.init=9, max.iter=20, show.iter=TRUE, tol=0.001)   
clus.nopt(C1=750, C2=250, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1, CV0=0.05, cal.sw=2)
clus.nopt(C1=c(750, 500, 250), C2=250, delta=0.01, unit.rv=1, tot.cost=100000, cal.sw=1)
clus.nopt(C1=750, C2=c(250,100), delta=0.01, unit.rv=1, CV0=0.05, cal.sw=2)
clus.nopt(C1=750, C2=100, delta=0.01, unit.rv=1:3, CV0=0.05, cal.sw=2)
clus.nopt(C1=750, C2=100, delta=0.01, unit.rv=1, CV0=c(0.05, 0.10), cal.sw=2)
clus.nopt(C1=250, C2=250, delta=0.01, unit.rv=1, tot.cost=c(50000,100000), cal.sw=1)

        # Test error msg for vector input
clus.nopt(C1=750, C2=250, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1:3, tot.cost=100000, cal.sw=1)
clus.nopt(C1=c(750, 500), C2=250, delta=c(0.01, 0.05, 0.10, 0.20), unit.rv=1, tot.cost=100000, cal.sw=1)
clus.nopt(C1=c(750, 500), C2=250, delta=0.01, unit.rv=1, tot.cost=c(50,100000), cal.sw=1)
