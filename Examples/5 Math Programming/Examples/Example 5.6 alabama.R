#****************************************************************************
# FILE:    Example 5.6 alabama.R
# PURPOSE: Use constrOptim.nl to solve allocation problems
# DATE:    9/14/09
# AUTHOR:  R. Valliant
# REVISED:
#****************************************************************************

require(alabama)
require(numDeriv)  # need to have "numDeriv" package installed

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
Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0)

                    # Employees
mh.emp <- c(511, 21, 70, 32, 157)
Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00)

                    # Proportion of estabs claiming research credit
ph.rsch <- c(0.8, 0.2, 0.5, 0.3, 0.9)
                    # Proportion of estabs with offshore affiliates
ph.offsh <- c(0.06, 0.03, 0.03, 0.21, 0.77)

budget = 300000
n.min <- 100
                    # Relvar function used in objective
relvar.rev <- function(nh){
    rv <- sum(Nh * (Nh/nh - 1)*Sh.rev^2)
    tot <- sum(Nh * mh.rev)
    rv/tot^2
}

                    # Relvar functions used in nonlinear constraints
                    # The nonlin constraints can take only 1 argument: in this case
                    #   the vector of decision varsOptim.nl
relvar.emp <- function(nh){
    rv <- sum(Nh * (Nh/nh - 1)*Sh.emp^2)
    tot <- sum(Nh * mh.emp)
    rv/tot^2
}

relvar.rsch <- function(nh){
    rv <- sum( Nh * (Nh/nh - 1)*ph.rsch*(1-ph.rsch)*Nh/(Nh-1) )
    tot <- sum(Nh * ph.rsch)
    rv/tot^2
}

relvar.offsh <- function(nh){
    rv <- sum( Nh * (Nh/nh - 1)*ph.offsh*(1-ph.offsh)*Nh/(Nh-1) )
    tot <- sum(Nh * ph.offsh)
    rv/tot^2
}

constraints <- function(nh){
    h <- rep(NA, 13)
                    # stratum sample sizes <= stratum pop sizes
    h[1:length(nh)] <- (Nh + 0.01) - nh
                    # stratum sample sizes >= a minimum
    h[(length(nh)+1) : (2*length(nh)) ] <- (nh + 0.01) - n.min
    h[2*length(nh) + 1] <- 0.05^2 - relvar.emp(nh)
    h[2*length(nh) + 2] <- 0.03^2 - relvar.rsch(nh)
    h[2*length(nh) + 3] <- 0.03^2 - relvar.offsh(nh)
#    h[2*length(nh) + 4] <- budget - sum(nh * ch)
    h
}

heq <- function(nh){
    heq <- 1 - sum(nh*ch/budget)
    heq
}

ans <- constrOptim.nl(          # parameter and objective function
        par = rep(1100,5),      # using par = rep(100,5) gives error: "initial value violates inequality constraints"
        fn = relvar.rev,
                    # parameter bounds
        hin = constraints,
        heq = heq,
        control.outer = list(eps = 1.e-10,
            mu0  = 1e-05,
            NMinit = TRUE,             # default is TRUE, using FALSE is worse
            method = "BFGS"             # default
#            method = "Nelder-Mead"     # worse objective value than BFGS
#        method = "CG"              # objective value about same as BFGS but slower execution
            )
)
ans

$par
[1]  429.7308  233.4132  113.5080 1534.6032  550.4323

$value
[1] 0.002260288

                    # check the constrOptim.nl solution
relvar.rev(ans$par)        # objective function

                    # CVs of estimates
sqrt(relvar.rev(ans$par))
sqrt(relvar.emp(ans$par))
sqrt(relvar.rsch(ans$par))
sqrt(relvar.offsh(ans$par))
                    # cost
sum(ans$par * ch)

                    # compare to solver GRG2 solution
nh.xls <- c(413,318,124,1397,596)
relvar.rev(nh.xls)        # objective function

                    # CVs of estimates
sqrt(relvar.emp(nh.xls))
sqrt(relvar.rsch(nh.xls))
sqrt(relvar.offsh(nh.xls))
                    # cost
sum(nh.xls * ch)



#__________________________________________________________________________________________________
        # start at the solver solution and see what happens alabama constrOptim.nl code

ans <- constrOptim.nl(          # parameter and objective function
        par = c(414, 317, 124, 1395, 650),   # h=5 sample increased so that offshore est CV is met
        fn = relvar.rev,
                    # parameter bounds
        hin = constraints,
        heq = heq,
        control.outer = list(eps = 1.e-10,
            mu0  = 1e-05,
            NMinit = TRUE,             # default is TRUE, using FALSE is worse
            method = "Nelder-Mead"     # worse objective value than BFGS
            )
)
ans

	# answer is a little different and worse objective value than solver
#$par
#[1]  470.8096  323.8774  115.2148 1409.0328  543.7493

#$value
#[1] 0.002214128
