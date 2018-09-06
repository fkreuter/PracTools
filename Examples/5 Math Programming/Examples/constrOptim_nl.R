#########################################################################################################################
constrOptim.nl <- function (par, fn, gr=NULL, hin=NULL, hin.jac=NULL, heq=NULL, heq.jac=NULL, 
mu = 1e-03, control = list(), eps=1.e-07, itmax=50, trace=TRUE, method="BFGS", NMinit=TRUE, ...)  {
# Constrained optimization with nonlinear inequality constraints
# Adaptive barrier MM algorithm (Ken Lange, 1994) for inequalities
# Augmented Lagrangian algorithm (Madsen, Nielsen, Tingeleff, 2004) for equalities
#
# par = starting vector of parameter values; initial vector must be "feasible"
# fn = scalar objective function
# gr = gradient function (will be computed using finite-difference, if not specified; but computations will be faster if specified)
# hin = a vector function specifying inequality constraints such that hin[j] > 0 for all j
# hin.jac = jacobian of the inequality vector function (will be computed using finite-difference, if not specified; but computations will be faster if specified)
# heq = a vector function specifying equality constraints such that heq[j] = 0 for all j
# heq.jac = jacobian of the equality vector function (will be computed using finite-difference, if not specified; but computations will be faster if specified)
#
# mu = parameter for barrier penalty
# control = a list of control parameters, same as that used in optim()
# eps = tolerance for convergence of outer iterations of the barrier and/or augmented lagrangian algorithm
# itmax = maximum number of outer iterations of the barrier and/or augmented lagrangian algorithm
# trace = logical variable indicating whether information on outer iterations should be printed out
# method = algorithm in optim() to be used; default is "BFGS" variable metric method
# NMinit = logical variable indicating whether "Nelder-Mead" algorithm should be used in optim() for the first outer iteration
#
# Author:  Ravi Varadhan, Johns Hopkins University
# Date:  August 20, 2008
#

   if (is.null(heq) & is.null(hin)) stop("This is an unconstrained optimization problem - you should use `optim' \n")

	require(numDeriv, quietly=TRUE)
     if (is.null(gr)) gr <- function(par, ...) grad(func=fn, x=par, method= "simple", ...)

   if (is.null(hin)) {
    if (is.null(heq.jac) ) heq.jac <- function(par, ...) jacobian(func=heq, x=par, method= "simple", ...)
    ans <- auglag(par, fn, gr, heq=heq, heq.jac=heq.jac, control=control, eps = eps, trace=trace, method=method, NMinit=NMinit, itmax=itmax, ...)
  }  else if (is.null(heq)) {

    if (is.null(hin.jac)) hin.jac <- function(par, ...) jacobian(func=hin, x=par, method= "simple", ...)
    ans <- adpbar(par, fn, gr, hin=hin, hin.jac=hin.jac, mu=mu, control = control, eps = eps, itmax=itmax, trace=trace, method=method, NMinit=NMinit, ...)

  }   else  {
    if (is.null(heq.jac) ) heq.jac <- function(par, ...) jacobian(func=heq, x=par, method= "simple", ...)
    if (is.null(hin.jac)) hin.jac <- function(par, ...) jacobian(func=hin, x=par, method= "simple", ...)
	ans <- alabama(par, fn, gr, hin=hin, hin.jac=hin.jac, heq=heq, heq.jac=heq.jac, mu= mu, control = control, 
	eps = eps, itmax=itmax, trace=trace, method=method, NMinit=NMinit, ...)
	}

detach("package:numDeriv")
    return(ans)
}
	
 #########################################################################################################################
 adpbar <- function (theta, fn, gr=gr, hin=hin, hin.jac=hin.jac, mu = mu, control = control, eps = eps, 
 itmax=itmax, trace=trace, method=method, NMinit=NMinit, ...)  {
 # Constrained optimization with nonlinear inequality constraints
 # Adaptive barrier MM algorithm (Ken Lange, 1994)
 # Ravi Varadhan, Johns Hopkins University
 # August 20, 2008
 #
     if (!is.null(control$fnscale) && control$fnscale < 0) 
         mu <- -mu
 
    R <- function(theta, theta.old, ...) {
        gi <- hin(theta, ...)
        if (any(gi < 0)) return(NaN)
        gi.old <- hin(theta.old, ...)
	bar <- sum(gi.old * log(gi) - hin.jac(theta.old, ...) %*% theta)

        if (!is.finite(bar)) 
            bar <- -Inf
      fn(theta, ...) - mu * bar
    }

    dR <- function(theta, theta.old, ...) {
        gi <- hin(theta, ...)
	gi.old <- hin(theta.old, ...)
	hi <- hin.jac(theta.old, ...)         
        dbar <- colSums(hi* gi.old/gi - hi)
        gr(theta, ...) - mu * dbar
    }
   
    if (any(hin(theta, ...) <= 0)) 
        stop("initial value not feasible")

    obj <- fn(theta, ...)
    r <- R(theta, theta, ...)
	feval <- 0
	geval <- 0 

    for (i in 1:itmax) {
	if (trace) {
		cat("par: ", theta, "\n")
		cat("fval: ", obj, "\n")
	}
        obj.old <- obj
        r.old <- r
        theta.old <- theta

        fun <- function(theta, ...) {
            R(theta, theta.old, ...)
        }
        gradient <- function(theta, ...) {
            dR(theta, theta.old, ...)
        }
	
       if ( NMinit & i == 1)  a <- optim(par=theta.old, fn=fun, gr=gradient, control = control, method = "Nelder-Mead", ...)
      else a <- optim(par=theta.old, fn=fun, gr=gradient, control = control, method = method, ...)
        r <- a$value

#  Here is "absolute" convergence criterion:
        if (is.finite(r) && is.finite(r.old) && abs(r - r.old) < eps) 
            break
        theta <- a$par
	feval <- feval + a$counts[1]
	if (!NMinit | i > 1) geval <- geval + a$counts[2]
        obj <- fn(theta, ...)
        if (obj > obj.old * sign(mu)) 
            break
    }
    if (i == itmax) {
        a$convergence <- 7
        a$message <- "Barrier algorithm ran out of iterations and did not converge"
    }
    if (mu > 0 && obj > obj.old) {
        a$convergence <- 11
        a$message <- paste("Objective function increased at outer iteration", 
            i)
    }
    if (mu < 0 && obj < obj.old) {
        a$convergence <- 11
        a$message <- paste("Objective function decreased at outer iteration", 
            i)
    }
    a$outer.iterations <- i
    a$barrier.value <- a$value
    a$value <- fn(a$par, ...)
    a$barrier.value <- a$barrier.value - a$value
    a$counts <- c(feval, geval)  # total number of fevals and gevals in inner iterations in BFGS
    a
}

#########################################################################################################################
auglag <- function (theta, fn, gr=gr, heq=heq, heq.jac=heq.jac, control = control, eps = eps, 
 itmax=itmax, trace=trace, method=method, NMinit=NMinit, ...)  {
# Constrained optimization with nonlinear equality constraints
# Augmented Lagrangian algorithm (Madsen, Nielsen, Tingeleff, 2004)
# Ravi Varadhan, Johns Hopkins University
# August 20, 2008
#

   if (!is.null(control$fnscale) && control$fnscale < 0) {
		pfact <- -1
	} else pfact <- 1


	ans <- vector("list")
	feval <- 0
	geval <- 0 
  	k <- 0
	lam0 <- 0
	sig0 <- 1
	lam <- lam0
	sig <- sig0
	i0 <- heq(theta, ...)
	Kprev <- max(abs(i0))
	K <- Inf

	while (K > eps & k <= itmax) {   # Augmented Lagrangian loop for nonlinear "equality" constraintts 

	if (trace) {
	cat("K, lambda, sig: ", K, lam, sig, "\n")
	cat("theta: ", theta, "\n")
	}

        fun <- function(theta, ...) {
		it <- heq(theta, ...)
            fn(theta, ...) - pfact * sum (lam * it) + pfact * sig/2 * sum(it * it)
        }
        gradient <- function(theta, ...) {
		it <- heq(theta, ...)
		ij <- heq.jac(theta, ...)
            gr(theta, ...) - pfact * colSums(lam * ij) + pfact * sig * drop(t(ij) %*% it)
        }  
	
        if ( NMinit & k == 0 ) a <- optim(par=theta, fn=fun, gr=gradient, control = control, method = "Nelder-Mead", ...)
        else a <- optim(par=theta, fn=fun, gr=gradient, control = control, method = method, ...)

	  theta <- a$par
        r <- a$value
	  i0 <- heq(theta, ...)
	  K <- max(abs(i0)) 
        feval <- feval + a$counts[1]
	  if (!NMinit | k > 0) geval <- geval + a$counts[2]
	  k <- k + 1

		if( K <= Kprev/4) {
			lam <- lam - i0 * sig
			Kprev <- K
		} else sig <- 10 * sig
	}  # Augmented Lagrangian loop completed

     if (k >= itmax) {
        a$convergence <- 7
        a$message <- "Augmented Lagrangian algorithm ran out of iterations and did not converge"
    }
  
    ans$par <- theta
    ans$value <- fn(a$par, ...)
    ans$iterations <- k
    ans$lambda <- lam
    ans$penalty <- r - ans$value
    ans$counts <- c(feval, geval)  # total number of fevals and gevals in inner iterations in BFGS

    ans
}
############################################################################################################
alabama <- function (theta, fn, gr=gr, hin=hin, hin.jac=hin.jac, heq=heq, heq.jac=heq.jac, mu = mu, control = list(), 
itmax = itmax, eps = eps, trace=trace, method="BFGS", NMinit="FALSE", ...) {
# Constrained optimization with nonlinear inequality constraints
# Augmented Lagrangian Adaptive Barrier MM Algorithm (ALABaMA)
# Ravi Varadhan, Johns Hopkins University
# August 24, 2008
#
# See connection to modified barrier augmented Lagrangian (Goldfarb et al., Computational Optimization & Applications 1999)
##########
	if (is.null(heq) & is.null(hin)) stop("This is an unconstrained optimization problem - use `optim' \n")

    if (!is.null(control$fnscale) && control$fnscale < 0) {
		mu <- -mu
		pfact <- -1
	} else pfact <- 1

	alpha <- 0.5
	beta <- 1

    R <- function(theta, theta.old, ...) {
        gi <- hin(theta, ...)
        if (any(gi < 0)) return(NaN)
        gi.old <- hin(theta.old, ...)
	  hjac <- hin.jac(theta.old, ...)
	bar <- sum(gi.old * log(gi) - hjac %*% theta)

        if (!is.finite(bar)) 
            bar <- -Inf
      fn(theta, ...) - mu * bar
    }

    dR <- function(theta, theta.old, ...) {
        gi <- hin(theta, ...)
	gi.old <- hin(theta.old, ...)
	hjac <- hin.jac(theta.old, ...)         
        dbar <- colSums(hjac* gi.old/gi - hjac)
        gr(theta, ...) - mu * dbar
    }
   
	h0 <- hin(theta, ...)
     if (any(h0 <= 0)) 
        stop("initial value violates inequality constraints")

    obj <- fn(theta, ...)
    r <- R(theta, theta, ...)
	feval <- 0
	geval <- 0 
	lam0 <- 0
	sig0 <- 1
	lam <- lam0
	sig <- sig0
	i0 <- heq(theta, ...)
	Kprev <- max(abs(i0))
	K <- Inf

    for (i in 1:itmax) {  # Adaptive Barrier MM loop for nonlinear "inequality" constraintts 
	if (trace) {
		cat("Outer iteration: ", i, "\n")
	cat("K = ", signif(K,2), ", lambda  = ", signif(lam,2), ", Sigma  = ", signif(sig,2), "\n ")
		cat("par: ", signif(theta,6), "\n")
		cat("fval =  ", signif(obj,4), "\n \n")
	}
        obj.old <- obj
        r.old <- r
        theta.old <- theta

        fun <- function(theta, ...) {
		it <- heq(theta, ...)
            R(theta, theta.old, ...) - pfact * sum (lam * it) + pfact * sig/2 * sum(it * it)
        }
        gradient <- function(theta, ...) {
		it <- heq(theta, ...)
		ij <- heq.jac(theta, ...)
            dR(theta, theta.old, ...) - pfact * colSums(lam * ij) + pfact * sig * drop(t(ij) %*% it)
        }  

#	mu <- 1/sig * pfact
	if(sig > 1e05) control$reltol <- 1.e-10

        if ( NMinit & i == 1)  a <- optim(par=theta.old, fn=fun, gr=gradient, control = control, method = "Nelder-Mead", ...)
       else a <- optim(par=theta.old, fn=fun, gr=gradient, control = control, method = method, ...)

	theta <- a$par
        r <- a$value

	  i0 <- heq(theta, ...)
	  K <- max(abs(i0))
         feval <- feval + a$counts[1]
	if (!NMinit | i > 1) geval <- geval + a$counts[2]

		if( K <= Kprev/4) {
			lam <- lam - i0 * sig
			Kprev <- K
		} else sig <- 10 * sig

        obj <- fn(theta, ...)

 #  Here is "absolute" convergence criterion:
	pconv <- max(abs(theta - theta.old))
        if ((is.finite(r) && is.finite(r.old) && abs(r - r.old) < eps && K < eps) | pconv < 1.e-12) {
#        if (is.finite(obj) && is.finite(obj.old) && abs(obj - obj.old) < eps && K < eps) {
		theta.old <- theta
		atemp <- optim(par=theta, fn=fun, gr=gradient, control = control, method = "BFGS", hessian=TRUE, ...)
		a$hessian <- atemp$hess 
		break
		}
		
 }

    if (i == itmax) {
        a$convergence <- 7
        a$message <- "ALABaMA ran out of iterations and did not converge"
    } else {
	evals <- eigen(a$hessian)$val
	if (min(evals) < -0.1)  {
        a$convergence <- 8
        a$message <- "Second-order KKT conditions not satisfied"
	} 
	else if (K > eps)  {
        a$convergence <- 9
        a$message <- "Convergence due to lack of progress in parameter updates"
	} 
   }

    a$outer.iterations <- i
    a$lambda <- lam
    a$sigma <- sig
    a$barrier.value <- a$value
    a$value <- fn(a$par, ...)
    a$barrier.value <- a$barrier.value - a$value
    a$K <- K
    a$counts <- c(feval, geval)  # total number of fevals and gevals in inner iterations in BFGS
    a
}
##############################################################################################################


