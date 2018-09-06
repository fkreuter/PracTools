##################################################################################
### Use quadprog package to compute bounded weights.
###
### File quadprog.wts.R
### Pgmr: R. Valliant
### Date: 10/31/08
##################################################################################

library(quadprog)
library(survey)
attach("C:\\JPSM\\SURV 699S-Prediction Theory\\R\\.RData", pos = 2)
                                # Select pps(sqrt(beds)) sample from smho98.sub
                                # Same sample as used for calibration example.
n <- 80
set.seed(331)
x <- smho98.sub[,"BEDS"]
                    # recode small hospitals to have a minimum MOS
x[x <= 5] <- 5
x <- sqrt(x)

sam <- pps.random.fcn(x, n)     # probability proportional to sqrt(x)
sam.dat <- smho98.sub[sam, ]
d <- sum(x) / (n * x[sam])      # base wts

                                # Tabulate pop totals for constraints
x.beds <- sum(smho98.sub[,"BEDS"])
x.seen <- sum(smho98.sub[,"SEENCNT"])
x.eoy <- sum(smho98.sub[,"EOYCNT"])
x.Nhosp <- table(smho98.sub[,"hosp.type"])

                               
                                # Set up constraints for calibration to pop totals
hosp.dum <- model.matrix(~ 0 + as.factor(hosp.type), data = sam.dat)
X <- rbind(sam.dat[, "BEDS"],
      sam.dat[, "SEENCNT"],
      sam.dat[, "EOYCNT"],
      t(hosp.dum)
      )
c0a <- c(x.beds, x.seen, x.eoy, x.Nhosp)

                                # Set up constraints for lower and upper weight bounds
In <- diag(nrow = n)
L <- 2
U <- 30
one <- rep(1, n)

c0b <- c( L * one,
        -U * one)

Cmat <- rbind(X, In, -In)

wts <- solve.QP(Dmat = diag(1/d),
         dvec = 2 * one,
         Amat = t(Cmat),
         bvec = c(c0a, c0b),
         meq = 8				# first 8 constraints are equality constraints
         )
        
quad.dsgn <- svydesign(ids = ~0,                 # no clusters 
                       strata = NULL,            # no strata
                       data = data.frame(sam.dat), 
                       weights = ~wts$solution)

range(d)
range(weights(quad.dsgn))

sum(weights(quad.dsgn))

                                # Check calibration
c(x.beds, x.seen, x.eoy, x.Nhosp)   # pop.tots
svytotal(~BEDS, quad.dsgn)
svytotal(~SEENCNT, quad.dsgn)
svytotal(~EOYCNT, quad.dsgn)
svytotal(~ as.factor(hosp.type), quad.dsgn)
                                # CV on EXPTOTAL
tot <- svytotal(~EXPTOTAL, quad.dsgn)
SE(tot)/tot

#_______________________________________________________________________________________
#	Histograms of pi-wts. greg wts, and quadprog wts
#_______________________________________________________________________________________

par(mfrow=c(1,3),mar=c(3,3,1,1))
truehist(d, xlab="")
title("Pi weights", cex.main = 2)
		# run GREG wts computation in calibrate.R to get sam.lin
truehist(weights(sam.lin), xlab="")
title("GREG weights", cex.main = 2)
truehist(wts$solution, xlab="")
title("quadprog weights", cex.main = 2)


rm(wts, L, U, In, x, X, n, sam.dat, d, tot) 
