#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\NRFUopt.R                
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     06/07/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute optimal sample sizes in 2-phase srs + stsrs design for nonresponse followup
#*********************************************************************************************************

NRFUopt <- function(Ctot=NULL, c1, c2, theta, CV0=NULL, CVpop=NULL, N=Inf, type.sw){
    if (!(type.sw %in% c("cost", "cv")))
        stop("type.sw must be cost or cv.\n")
    if (is.null(c1) | is.null(c2) | is.null(theta)) 
        stop("c1, c2, and theta cannot be NULL.\n")
    if (c1 < 0 | c2 < 0)        
        stop("Unit costs, c1 and c2, must be positive.\n")
    if (c1 < 0 | c2 < 0)        
        stop("Unit costs c1 and c2 must be positive.\n")
    if (theta <= 0 | theta > 1)        
        stop("Response probability must be in (0,1].\n")
    
    if (type.sw == "cost"){
        if (is.null(Ctot)) stop("Ctot must be specified for fixed cost allocation.\n")
        allocation <- "fixed cost"
    }
    if (type.sw == "cv"){
        if (is.null(CV0)) stop("CV0 must be specified for fixed CV allocation.\n")
        if (CV0 <0 )
            stop("CV0 must be positive.\n")
        allocation <- "fixed CV"
    }
        
    v.opt <- sqrt(c1/c2/theta)
    
    if (type.sw=="cv"){
        n1.opt <- (1/v.opt) * (1-theta*(1-v.opt)) / ((CV0/CVpop)^2 + 1/N)
    }
    if (type.sw=="cost"){
        n1.opt <- Ctot / (c1 + c2*v.opt*(1-theta))
    }
    
    n2 <- v.opt * (1-theta) * n1.opt
    Ctot.chk <- c1*n1.opt + c2*v.opt*(1-theta)*n1.opt
    
    if (!is.null(CVpop)){
        CV0.chk <- sqrt(CVpop^2/n1.opt * (1-n1.opt/N + (1-v.opt)/v.opt * (1-theta)))
        CV0.chk <- round(CV0.chk,4)
    }
            else {CV0.chk <- NULL}

    n.srs <- n1.opt* 1/(theta + (1-theta)/v.opt - n1.opt/N)  / theta
    c.ratio <- n1.opt/n.srs * (1 + c2/c1 *v.opt * (1-theta))
    
    if (v.opt > 1) warning("v.opt > 1: Solution is not feasible.\n")
    
    list("allocation" = allocation,
         "Total variable cost" = Ctot.chk,
         "Response rate" = theta,
         "CV" = CV0.chk,
         "v.opt" = round(v.opt,4),
         "n1.opt" = round(n1.opt,0),
         "Expected n2" = round(n2,0),
         "Expected total cases (2-phase)" = round(n1.opt + n2),
         "srs sample for same cv" = round(n.srs,0),
         "Cost Ratio: Two phase to srs" = round(c.ratio,3))
}



#Examples
NRFUopt(Ctot=100000, c1=50, c2=200, theta=0.3, CV0=NULL, CVpop=NULL, type.sw="cost")
NRFUopt(Ctot=NULL, c1=75, c2=150, theta=0.7, CV0=0.10, CVpop=3, type.sw="cv")

NRFUopt(Ctot=100000, c1=50, c2=200, theta=0.3, CV0=NULL, CVpop=1, type.sw="cost")
NRFUopt(Ctot=100000, c1=50, c2=200, theta=0.5, CV0=NULL, CVpop=1, type.sw="cost")

    # Exercises
NRFUopt(Ctot=500000, c1=25, c2=200, theta=0.3, CV0=NULL, CVpop=1, type.sw="cost")
NRFUopt(Ctot=NULL, c1=75, c2=350, theta=0.4, CV0=0.10, CVpop=2, type.sw="cv")
    # v.opt > 1 for c1=c2=75: not feasible
NRFUopt(Ctot=NULL, c1=75, c2=75, theta=0.4, CV0=0.10, CVpop=2, type.sw="cv")
NRFUopt(Ctot=NULL, c1=75, c2=150, theta=0.4, CV0=0.10, CVpop=2, type.sw="cv")


NRFUopt(Ctot=NULL, c1=50, c2=200, theta=0.3, CV0=0.05, CVpop=1, type.sw = "cv")
NRFUopt(Ctot=NULL, c1=50, c2=200, theta=0.5, CV0=0.05, CVpop=1, type.sw = "cv")
NRFUopt(Ctot=NULL, c1=100, c2=50, theta=0.5, CV0=0.05, CVpop=1, type.sw = "cv")

NRFUopt(Ctot=NULL, c1=50, c2=200, theta=0.3, CV0=0.05, CVpop=4, type.sw = "cv")
NRFUopt(Ctot=NULL, c1=50, c2=400, theta=0.3, CV0=0.05, CVpop=4, type.sw = "cv")
NRFUopt(Ctot=NULL, c1=50, c2=50, theta=0.3, CV0=0.05, CVpop=4, type.sw = "cv")


NRFUopt(Ctot=NULL, c1=50, c2=200, theta=0.3, CV0=0.05, CVpop=4, type.sw = "cxx")
NRFUopt(Ctot=NULL, c1=50, c2=200, theta=0.3, CV0=0.05, CVpop=4, type.sw = "cost")
NRFUopt(Ctot=100000, c1=50, c2=200, theta=0.9, CV0=NULL, CVpop=4, type.sw = "cv")


