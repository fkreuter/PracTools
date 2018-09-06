#****************************************************************************
# FILE:         Code 5.4 nloptr.example.R
# PURPOSE:      Use optimx to solve establishment allocation problem
# DATE:         07/15/2017
# AUTHOR:       R. Valliant
# REFERENCE:    https://cran.r-project.org/web/packages/nloptr/vignettes/nloptr.pdf
# REVISED:
#****************************************************************************

require(nloptr)

    # Decision vars
nh <- vector("numeric", length = 5)

    # Stratum pop sizes
Nh <- c(6221,
        11738,
        4333,
        22809,
        5467)
    # Stratum costs
ch <- c(120, 80, 80, 90, 150)

    # Stratum means and SDs
    # Revenues
mh.rev <- c(85, 11, 23, 17, 126)
tot.rev <- sum(Nh * mh.rev)
Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0)

    # Employees
mh.emp <- c(511, 21, 70, 32, 157)
tot.emp <- sum(Nh * mh.emp)
Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00)

    # Proportion of estabs claiming research credit
ph.rsch <- c(0.8, 0.2, 0.5, 0.3, 0.9)
tot.rsch <- sum(Nh * ph.rsch)
Sh.rsch <- ph.rsch*(1-ph.rsch)*Nh/(Nh-1)

    # Proportion of estabs with offshore affiliates
ph.offsh <- c(0.06, 0.03, 0.03, 0.21, 0.77)
tot.offsh <- sum(Nh * ph.offsh)
Sh.offsh <- ph.offsh*(1-ph.offsh)*Nh/(Nh-1)

budget = 300000
n.min <- 100
                    # Relvar function used in objective
relvar.rev <- function(nh){
    rv <- sum(Nh * (Nh/nh - 1)*Sh.rev^2)
    rv/tot.rev^2
}

    # gradient of objective function
grad_f0 <- function(nh){
    (Nh*Sh.rev/nh/tot.rev)^2
}

    # Relvar functions used in nonlinear constraints
    # The nonlin constraints can take more than 1 argument, but the first
    #   must be the vector of decision vars nh
relvar.emp <- function(nh){
    rv <- sum(Nh * (Nh/nh - 1)*Sh.emp^2)
    rv/tot.emp^2
}

relvar.rsch <- function(nh){
    rv <- sum( Nh * (Nh/nh - 1)* Sh.rsch^2)
    rv/tot.rsch^2
}

relvar.offsh <- function(nh){
    rv <- sum( Nh * (Nh/nh - 1)* Sh.offsh^2)
    rv/tot.offsh^2
}

    # f = min fraction of budget to spend
f <- 0.995
    # Non-linear inequality constraints must be of form g(x) <= 0
ineq_constr <- function(nh){
    h <- rep(NA, 5)
    h[1] <- relvar.emp(nh) - 0.05^2
    h[2] <- relvar.rsch(nh) - 0.03^2
    h[3] <- relvar.offsh(nh) - 0.03^2
    h[4] <- sum(nh*ch/budget) - 1
    h[5] <- 1 - sum(nh*ch)/(f*budget)
    h
}

grad_jac_ineq <- function(nh){
    return(rbind( -(Nh*Sh.emp/nh/tot.emp)^2,
              -(Nh*Sh.rsch/nh/tot.rsch)^2,
              -(Nh*Sh.offsh/nh/tot.offsh)^2,
               ch/budget,
               -ch/(f*budget)
              ) )
}

nh.start <- rep(1100,5) #initial values

out <- nloptr(x0 = nh.start,        # starting values
       eval_f = relvar.rev,         # objective fcn
       eval_grad_f = grad_f0,       # gradient of objective fcn
       lb = rep(n.min,5),           # lower bound on decision vars
       ub = Nh,                     # lower bound on decision vars
       eval_g_ineq = ineq_constr,   # fcn to evaluate (non-)linear inequality constraints
       eval_jac_g_ineq = grad_jac_ineq,  # fcn to evaluate jacobian of (non-)linear inequality constraints
        opts = list("algorithm" = "NLOPT_LD_MMA",   # MMA = method of moving asymptotes
                    "xtol_rel"=1.0e-8,
                    "print_level" = 2,
                    "check_derivatives" = TRUE,
                    "check_derivatives_print" = "all")
)

sum(out$solution * ch)
#[1] 298502.4

#--------------------------------------------------------------------------------------------------
    # check the solution
relvar.rev(out$solution)        # objective function
[1] 0.002147692
                    # CVs of estimates
sqrt(relvar.rev(out$solution))
# [1] 0.04634319
sqrt(relvar.emp(out$solution))
#[1] 0.02287253
sqrt(relvar.rsch(out$solution))
#[1] 0.01828874
sqrt(relvar.offsh(out$solution))
#[1] 0.02990973

    # total sample size
sum(out$solution)
#[1] 2758.102

    # total cost
sum(out$solution * ch)
#[1] 297067.1

                    # compare to solver GRG2 solution
nh.xls <- c(413,318,124,1397,596)
relvar.rev(nh.xls)        # objective function
#[1] 0.00216982
    # CVs of estimates
sqrt(relvar.emp(nh.xls))
#[1] 0.02391837
sqrt(relvar.rsch(nh.xls))
#[1] 0.009173057
sqrt(relvar.offsh(nh.xls))
#[1] 0.01101557
    # cost
sum(nh.xls * ch)
#[1] 300050
