#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\Example 17.12.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     10/19/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Example 17.12 double sampling for stratification
#*********************************************************************************************************


dub <- function(c1, c2, Ctot, Nh, Sh, Yh.bar){
    Wh <- Nh/sum(Nh)
    Ybar <- sum(Wh*Yh.bar)

    V1 <- sum(Wh*(Yh.bar-Ybar)^2)
    V2 <- sum(Wh*Sh)^2
    neyman <- Wh*Sh / sum(Wh*Sh)
    K <- (V2/V1) / (c2/c1)

    n1 <- Ctot / (c1 + c2*sqrt(K))
    n2 <- n1*sqrt(K)
    cost.chk <- c1*n1 + c2*n2
    ney.alloc <- n2 * neyman

    Vopt <- V1/n1 + V2/n2

        # srs of cost Ctot assuming units cost c2 each
    nsrs <- Ctot/c2
    S2 <- sum(Wh*Sh^2) + V1
    Vsrs <- S2/nsrs

    list(V1 = V1,
         V2 = V2,
         K = K,
         n1 = n1,
         n2 = n2,
         "n2/n1" = n2/n1,
         ney.alloc = ney.alloc,
         Vopt = Vopt,
         nsrs = nsrs,
         Vsrs = Vsrs,
         Vratio = round(Vopt/Vsrs, 2),
         Ctot = Ctot,
         cost.chk = cost.chk)
}


Wh <- rep(0.25,4)
Ph <- c(0.02,0.12,0.37,0.54)
Sh <- sqrt(Ph*(1-Ph))
c1 <- 10
c2 <- 50
Ctot <- 20000

dub(c1, c2, Ctot, Wh, Sh, Ph)

$V1
[1] 0.04191875

$V2
[1] 0.1307118

$K
[1] 0.6236434

$n1
[1] 404.1584

$n2
[1] 319.1683

$`n2/n1`
[1] 0.789711

$ney.alloc
[1]  30.89801  71.71903 106.55494 109.99634

$Vopt
[1] 0.0005132573

$nsrs
[1] 400

$Vsrs
[1] 0.0004839844

$Vratio
[1] 1.06

$Ctot
[1] 20000

$cost.chk
[1] 20000
