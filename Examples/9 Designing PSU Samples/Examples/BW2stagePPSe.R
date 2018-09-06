#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\BW2stagePPSe.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/14/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs design
#*********************************************************************************************************
# Ni   = no. of elements in pop in PSU i
# ni   = no. of elements in sample in PSU i
# X = data vector
# psuID = vector PSU IDs (length is same as length of X)
# w = full sample weight
# m = no. of sample PSUs
# pp = vector of 1-draw selection probabilities (length is same as length of X)

BW2stagePPSe <- function(Ni, ni, X, psuID, w, m, pp){
    fi <- ni / Ni
    t.pwr <- sum(w*X)
    pi.star <- m * pp

    S2i <- by(X, INDICES = psuID, FUN = var)
    Vi <- Ni * (Ni/ni - 1) * S2i
    F2 <- sum( (1-pi.star )/pi.star^2 * Vi )
    F1 <- by(w*X, psuID, sum)
    v1 <- m * var(F1)     # ultimate cluster est for v(t.pwr)

    Vpsu <- v1 - F2
    Vssu <- sum(Vi / (pi.star)^2)

    B <- (m*v1 - sum((1-pi.star)*Vi / m / pp^2)) / t.pwr^2
    W <- sum(Ni^2 * S2i / m / pp^2) / t.pwr^2
    delta <- B / (B + W)

    c(Vpsu=Vpsu, Vssu=Vssu, B=B, W=W, delta=delta)
}
