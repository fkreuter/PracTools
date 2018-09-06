#**************************************************************************
# File:     quadprog.wts.R
# Name:     R. Valliant
# Topic:    Practical Tools for Designing and Weighting Sample Surveys
# Date:     07/28/10
# Purpose: Use quadprog package to compute bounded weights.
#**************************************************************************

library(quadprog)
require(sampling)
library(survey)

smho98.sub <- read.csv("C:\\Projects\\Practical Tools Book\\Data\\smho98.sub.csv", row.names = 1)
dim(smho98.sub)

            #   drop hosp.type = 4
delete <- smho98.sub$hosp.type == 4
smho <- smho98.sub[!delete, ]
dim(smho)

##################################################################################
### Select a pps to sqrt(BEDS) sample                                             
##################################################################################

x <- smho[,"BEDS"]
        # recode small hospitals to have a minimum MOS
x[x <= 5] <- 5
x <- sqrt(x)

n <- 80
set.seed(428274453)

pk <- n*x/sum(x)
sam <- UPrandomsystematic(pk)
sam <- sam==1

sam.dat <- smho[sam, ]
d <- 1/pk[sam]

                    # Check to see if all hosp.type's are in sample
table(sam.dat[, "hosp.type"])

                                # Tabulate pop totals for constraints
x.beds <- sum(smho[,"BEDS"])
x.seen <- sum(smho[,"SEENCNT"])
x.eoy <- sum(smho[,"EOYCNT"])
x.Nhosp <- table(smho[,"hosp.type"])
          
                                # Set up constraints for calibration to pop totals
hosp.dum <- model.matrix(~ 0 + as.factor(hosp.type), data = sam.dat)
X <- rbind(sam.dat[, "BEDS"],
      sam.dat[, "SEENCNT"],
      sam.dat[, "EOYCNT"],
      t(hosp.dum)
      )
c0a <- c(x.beds, x.seen, x.eoy, x.Nhosp)

        # Lower and upper weight bounds
L <- 2
U <- 18
        # Compute full sample weights using quadratic programming
In <- diag(nrow = n)
L <- 2
U <- 18
one <- rep(1, n)
c0b <- c( L * one,
        -U * one)

Cmat <- rbind(X, In, -In)

fs.wts <- solve.QP(Dmat = diag(1/d),
         dvec = 2 * one,
         Amat = t(Cmat),
         bvec = c(c0a, c0b),
         meq = 7                # first 7 constraints are equality constraints
         )

        # Compute jackknife version of weights
        # Matrix to hold jackknife weights
rep.wts <- matrix(0, nrow = n, ncol = n)
for (k in 1:n){
    fill <- (1:n)[-k]
    In <- diag(nrow = n-1)
    one <- rep(1, n-1)
    c0b <- c( L * one,
            -U * one)

    Cmat <- rbind(X[,-k], In, -In)

    wts <- solve.QP(Dmat = diag(1/d[-k]),
             dvec = 2 * one,
             Amat = t(Cmat),
             bvec = c(c0a, c0b),
             meq = 7  
             )
    rep.wts[k, fill] <- wts$solution 
}   
        # make jackknife design object                  
qp.dsgn <- svrepdesign(weights = fs.wts$solution, repweights = t(rep.wts),
                type = "JK1", scale = (n-1)/n, data = data.frame(sam.dat), combined = TRUE)

range(d)
range(qp.dsgn$pweights)
sum(qp.dsgn$pweights)
apply(qp.dsgn$repweights,2,sum)

        # Check calibration
c(x.beds, x.seen, x.eoy, x.Nhosp)   # pop.tots
svytotal(~BEDS, qp.dsgn, return.replicates=TRUE)
svytotal(~SEENCNT, qp.dsgn)
svytotal(~EOYCNT, qp.dsgn)
svytotal(~ as.factor(hosp.type), qp.dsgn)

        # CV on total of EXPTOTAL
tot <- svytotal(~EXPTOTAL, qp.dsgn)
tot
cv(svytotal(~EXPTOTAL, qp.dsgn))
        # CV on mean of EXPTOTAL
mn <- svymean(~EXPTOTAL, qp.dsgn)
mn
cv(svymean(~EXPTOTAL, qp.dsgn))
svymean(~ as.factor(FINDIRCT), qp.dsgn)
cv(svymean(~ as.factor(FINDIRCT), qp.dsgn))

#_______________________________________________________________________________________
#   Jackknife on design weights for comparison
#_______________________________________________________________________________________
smho.dsgn <- svydesign(ids = ~0,        # no clusters 
          strata = NULL,            # no strata
          data = data.frame(sam.dat), 
          weights = ~d)

jk1.dsgn <- as.svrepdesign(smho.dsgn, type="JK1")

svytotal(~EXPTOTAL, smho.dsgn)
cv(svytotal(~EXPTOTAL, smho.dsgn))
svymean(~EXPTOTAL, smho.dsgn)
cv(svymean(~EXPTOTAL, smho.dsgn))
svymean(~ as.factor(FINDIRCT), smho.dsgn)
cv(svymean(~ as.factor(FINDIRCT), smho.dsgn))

svytotal(~EXPTOTAL, jk1.dsgn)
cv(svytotal(~EXPTOTAL, jk1.dsgn))
svymean(~EXPTOTAL, jk1.dsgn)
cv(svymean(~EXPTOTAL, jk1.dsgn))
svymean(~ as.factor(FINDIRCT), jk1.dsgn)
cv(svymean(~ as.factor(FINDIRCT), jk1.dsgn))

#_______________________________________________________________________________________
#   Scatterplot matrix of pi-wts. greg wts, bounded greg wts, and quadprog wts          
#_______________________________________________________________________________________

par(mfrow=c(1,3),mar=c(3,3,1,1))
truehist(d, xlab="")
title("Pi weights", cex.main = 2)

        # run GREG wts computation in greg.smho98.R to get sam.lin & sam.linBD
pairs(cbind("Design wts"=d, 
            "GREG wts"=weights(sam.lin), 
            "Bd GREG wts"=weights(sam.linBD),             
            "QP wts"=fs.wts$solution),
            pch = 16)

rm(wts, L, U, In, x, X, n, sam.dat, d, tot) 
