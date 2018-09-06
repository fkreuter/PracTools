#**************************************************************************
# Program: Example 14.5 Chen-Rust deff.R
# Name:    R. Valliant
# Project: Practical Tools for Designing and Weighting Sample Surveys
# Date:    07/10/2017
# Purpose: Calculate Chen-Rust deff
#**************************************************************************

  require(PracTools)
  data(MDarea.pop)
  Ni <- table(MDarea.pop$TRACT)
  m <- 10
  probi <- m*Ni / sum(Ni)
      # select sample of clusters
  set.seed(-780087528)
  sam <- cluster(data=MDarea.pop, clustername="TRACT",
                  size=m, method="systematic",
                  pik=probi, description=TRUE)
      # extract data for the sample clusters
  samclus <- getdata(MDarea.pop, sam)
  samclus <- rename(samclus, c(Prob = "pi1"))
      # treat sample clusters as strata and select srswor from each
  nbar <- 4
  s <- strata(data = as.data.frame(samclus), stratanames = "TRACT",
              size = rep(nbar,m), method="srswor")
      # extracts the observed data
  samdat <- getdata(samclus,s)
  samdat <- rename(samdat, c(Prob = "pi2"))
      # add an artificial stratum ID
  H <- 2
  nh <- m * nbar / H
  stratum <- NULL
  for (h in 1:H){
      stratum <- c(stratum, rep(h,nh))
  }
  wt <- 1/(samdat$pi1*samdat$pi2) * runif(m*nbar)
  samdat <- cbind(subset(samdat, select = -c(Stratum)),
                    stratum, wt)
  deff(w = samdat$wt, y=samdat$y2, strvar = samdat$stratum,
        clvar = samdat$TRACT, Wh=NULL, type="cr")
