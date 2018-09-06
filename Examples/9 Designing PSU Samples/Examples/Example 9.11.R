#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.11.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/14/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs design
#*********************************************************************************************************

require(PracTools)
require(sampling)
require(reshape)      # has function that allows renaming variables

data(MDarea.pop)

Ni <- table(MDarea.pop$TRACT)
m <- 20
probi <- m*Ni / sum(Ni)
    # select sample of clusters
set.seed(-780087528)
sam <- cluster(data=MDarea.pop, clustername="TRACT", size=m, method="systematic",
                pik=probi, description=TRUE)
    # extract data for the sample clusters
samclus <- getdata(MDarea.pop, sam)
samclus <- rename(samclus, c(Prob = "pi1"))

table(samclus$TRACT)

    # treat sample clusters as strata and select srswor from each
s <- strata(data = as.data.frame(samclus), stratanames = "TRACT",
            size = rep(50,m), method="srswor")
# extracts the observed data
samdat <- getdata(samclus,s)
samdat <- rename(samdat, c(Prob = "pi2"))
table(samdat$TRACT)

    # extract pop counts for PSUs in sample
pick <- names(Ni) %in% sort(unique(samdat$TRACT))
Ni.sam <- Ni[pick]
pp <- Ni.sam / sum(Ni)
wt <- 1/samdat$pi1/samdat$pi2

BW <- rbind(BW2stagePPSe(Ni = Ni.sam, ni = rep(50,20), X = samdat$y1,
            psuID = samdat$TRACT, w = wt,
            m = 20, pp = pp),
       BW2stagePPSe(Ni = Ni.sam, ni = rep(50,20), X = samdat$y2,
            psuID = samdat$TRACT, w = wt,
            m = 20, pp = pp),
       BW2stagePPSe(Ni = Ni.sam, ni = rep(50,20), X = samdat$y3,
            psuID = samdat$TRACT, w = wt,
            m = 20, pp = pp),
       BW2stagePPSe(Ni = Ni.sam, ni = rep(50,20), X = samdat$ins.cov,
            psuID = samdat$TRACT, w = wt,
            m = 20, pp = pp),
       BW2stagePPSe(Ni = Ni.sam, ni = rep(50,20), X = samdat$hosp.stay,
            psuID = samdat$TRACT, w = wt,
            m = 20, pp = pp)
)

round(BW,4)
