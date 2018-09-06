#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\BW2stageSRS.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/03/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an srs/srs/ design using the Anne Arundel pop.
#*********************************************************************************************************

# X      = data vector
# psuID = vector of IDs for clusters
# pop    = population

BW2stageSRS <- function(X, psuID){
    M <- length(unique(psuID))
    Ni <- table(psuID)
    Nbar <- length(X)/M

    ti <- by(X, INDICES = psuID, FUN = sum)
    S2Ui <- by(X, INDICES = psuID, FUN = var)

    tbarU <- mean(ti)
    tU <- M*tbarU
    S2U1 <- var(ti)
    B2 <- S2U1 / tbarU^2

    ybarU <- mean(X)
    S2U <- var(X)

    W2 <- M * sum(Ni^2 * S2Ui) / tU^2

    V <- (M-1)/M*B2 + (Nbar-1)/Nbar*W2
        
    c("B2"=B2, "W2"=W2, 
      "unit relvar"=S2U/ybarU^2, 
      "B2+W2"=B2 + W2, 
      "delta full" = ((M-1)/M*B2 - V/Nbar) / ((Nbar-1)/Nbar*V)
      )
}
