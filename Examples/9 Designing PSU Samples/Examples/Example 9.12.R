#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.12.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/28/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs/srs design
#*********************************************************************************************************

    # select 3-stage sample from Maryland population
attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)
trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
MDpop <- cbind(MDarea.pop, trtBG)

#require(doBy)
require(sampling)
require(reshape)      # has function that allows renaming variables

    # make counts of SSUs and elements per PSU
xx <- do.call("rbind",list(by(1:nrow(MDpop),MDpop$trtBG,head,1)))
pop.tmp <- MDpop[xx,]
Ni <- table(pop.tmp$TRACT)
Qi <- table(MDarea.pop$TRACT)
Qij <- table(MDpop$trtBG)
m <- 30         # no. of PSUs to select
probi <- m*Qi / sum(Qi)

#-------------------------------------------------------------------------------
    # select sample of clusters
#set.seed(-780087528)
set.seed(-1055331180)
#set.seed(788430374)
#set.seed(-1103350527)
#set.seed(1838705125)
#set.seed(-963481040)
sam <- cluster(data=MDpop, clustername="TRACT", size=m, method="systematic",
                pik=probi, description=TRUE)
    # extract data for the sample clusters
samclus <- getdata(MDpop, sam)
samclus <- rename(samclus, c(Prob = "p1i"))
samclus <- samclus[order(samclus$TRACT, samclus$BLKGROUP),]

table(samclus$TRACT)

#-------------------------------------------------------------------------------
    # treat sample clusters as strata and select srswor of block groups from each
    # identify psu IDs for 1st instance of each ssuID
xx <- do.call("rbind",list(by(1:nrow(samclus),samclus$trtBG,head,1)))

SSUs <- cbind(TRACT=samclus$TRACT[xx], trtBG=samclus$trtBG[xx], BG=samclus$BLKGROUP[xx])
    # select 2 BGs per tract
n <- 2
s <- strata(data = as.data.frame(SSUs), stratanames = "TRACT",
            size = rep(n,m), method="srswor")
s <- rename(s, c(Prob = "p2i"))

    # extract the BG data
        # s contains selection probs of SSUs, need to get those onto data file
SSUsam <- SSUs[s$ID_unit, ]
SSUsam <- cbind(s, SSUsam[, 2:3])
    # identify rows in PSU sample that correspond to sample SSUs
tmp <- samclus$trtBG %in% SSUsam$trtBG

SSUdat <- samclus[tmp,]
SSUdat <- merge(SSUdat, SSUsam[, c("p2i","trtBG")], by="trtBG")
rm(tmp)

#-------------------------------------------------------------------------------
    # select srswor from each sample BG
n.BG <- m*n
s <- strata(data = as.data.frame(SSUdat), stratanames = "trtBG",
            size = rep(50,n.BG), method="srswor")
s <- rename(s, c(Prob = "p3i"))
samclus <- getdata(SSUdat, s)
del <- (1:ncol(samclus))[dimnames(samclus)[[2]] %in% c("ID_unit","Stratum")]
samclus <- samclus[, -del]

table(samclus$TRACT)
table(samclus$trtBG)

#-------------------------------------------------------------------------------
    # extract pop counts for PSUs in sample
pick <- names(Qi) %in% sort(unique(samclus$TRACT))
Qi.sam <- Qi[pick]
    # extract pop counts of SSUs for PSUs in sample
pick <- names(Ni) %in% sort(unique(samclus$TRACT))
Ni.sam <- Ni[pick]
    # extract pop counts for SSUs in sample
pick <- names(Qij) %in% sort(unique(samclus$trtBG))
Qij.sam <- Qij[pick]

    # compute full sample weight and wts for PSUs and SSUs
wt <- 1 / samclus$p1i / samclus$p2i / samclus$p3i
w1i <- 1 / samclus$p1i
w2ij <- 1 / samclus$p1i / samclus$p2i
samdat <- data.frame(psuID = samclus$TRACT, ssuID = samclus$trtBG,
                w1i = w1i, w2ij = w2ij, w = wt,
                samclus[, c("y1","y2","y3","ins.cov", "hosp.stay")])

#-------------------------------------------------------------------------------
    # call fcn to compute variance component estimates

wtdvar <- function(x, w){
    xbarw <- sum(w*x) / sum(w)
    varw <- sum(w * (x-xbarw)^2) / sum(w)
    varw
}

rbind(BW3stagePPSe(dat=samdat, v="y1", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m),
      BW3stagePPSe(dat=samdat, v="y2", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m), 
      BW3stagePPSe(dat=samdat, v="y3", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m), 
      BW3stagePPSe(dat=samdat, v="ins.cov", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m), 
      BW3stagePPSe(dat=samdat, v="hosp.stay", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m)
     )

