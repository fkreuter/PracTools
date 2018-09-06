#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\BW3stagePPSe.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/19/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs/srs design
#*********************************************************************************************************

# Ni   = no. of elements in pop in PSU i
# ni   = no. of elements in sample in PSU i
# X = data vector
# psuID = vector PSU IDs (length is same as length of X)
# w = full sample weight
# m = no. of sample PSUs
# pp = vector of 1-draw selection probabilities (length is same as length of X)

wtdvar <- function(x, wt){
    xbarw <- weighted.mean(x, wt)
    varw <- sum(wt * (x-xbarw)^2) / sum(wt)
    varw
}

BW3stagePPSe <- function(Ni, ni, Qij, qij, X, psuID, ssuID, w, m, pp){
    pi.star <- m * pp
    nbar <- mean(ni)
    M.hat <- sum(1/pi.star)
    
    wij <- rep(Ni / pi.star / ni, ni)
    Qi.hat <-  by(wij * Qij, INDICES = rep(unique(psuID),ni), sum)
    Qbbar <- sum(wij*Qij) / sum(wij)
    Qbar <- sum(wij*Qij) / M.hat

    ybar.ij <- by(X, INDICES = ssuID, mean)
    tij <- Qij * ybar.ij
    ti <- Ni / ni * sum(tij)
    t.pwr <- sum(w * X)

    S2i <- by(as.vector(tij), INDICES = rep(unique(psuID),ni), var)
    S3ij <- by(X, INDICES = ssuID, var)
    
    V3ij <- Qij * (Qij/qij - 1) * S3ij
    sV3i <- by(as.vector(V3ij), INDICES = rep(unique(psuID),ni), sum)
    V2i <- Ni * (Ni/ni - 1)* S2i + sV3i
    V1 <- sum((ti/pp - t.pwr)^2)/m/(m-1) + sum(V2i/pi.star)
        # within PSU unit var estimate
    ybar.hat <- weighted.mean(X, w)
browser()
    V3i <- by(X, INDICES = psuID, wtdvar, w = w)
    
    Vtsu <- sum(Ni^2/ni^2 / pi.star^2 * sV3i)
    Vssu <- sum(V2i/pi.star^2) - Vtsu
    Vpsu <- V1 - Vssu - Vtsu
    
    B <-  sum((ti/pp - t.pwr)^2)/(m-1) - sum((1-pi.star)/m/pp^2 * V2i)
    B <- B / t.pwr^2
    W <- sum(V3i/pi.star) / (M.hat * ybar.hat)^2
    
    W2 <- sum(Ni^2/m/pp^2 * S2i) - sum(Ni^2/nbar/m/pp^2 * sV3i)
    W2 <- W2 / t.pwr^2
    W3 <- sum(Ni^2/nbar/m/pi.star^2 * sV3i)
    W3 <- W3 / t.pwr^2
    V2 <- W2 + (Qbbar-1)*W3 / Qbbar
    
    delta1 <- (B - W/Qbar) / V1
    delta2 <- (W2 - W3/Qbbar) / V2

    c(Vpsu=Vpsu, Vssu=Vssu, B=B, W=W, W2=W2, W3=W3, delta1=delta1, delta2=delta2)
}


    # select 3-stage sample
attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)
trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
MDpop <- cbind(MDarea.pop, trtBG)

require(doBy)
require(sampling)
require(reshape)      # has function that allows renaming variables

xx <- do.call("rbind",list(by(1:nrow(MDpop),MDpop$trtBG,head,1)))
pop.tmp <- MDpop[xx,]
Ni <- table(pop.tmp$TRACT)
Qij <- table(MDpop$trtBG)
m <- 20
probi <- m*Ni / sum(Ni)
    # select sample of clusters
set.seed(-780087528)
sam <- cluster(data=MDpop, clustername="TRACT", size=m, method="systematic",
                pik=probi, description=TRUE)
    # extract data for the sample clusters
samclus <- getdata(MDpop, sam)
samclus <- rename(samclus, c(Prob = "p1i"))
samclus <- samclus[order(samclus$TRACT, samclus$BLKGROUP),]

table(samclus$TRACT)

    # treat sample clusters as strata and select srswor of block groups from each
    # identify psu IDs for 1st instance of each ssuID
xx <- do.call("rbind",list(by(1:nrow(samclus),samclus$trtBG,head,1)))

SSUs <- cbind(TRACT=samclus$TRACT[xx], trtBG=samclus$trtBG[xx], BG=samclus$BLKGROUP[xx])
    # select 2 BGs per tract
s <- strata(data = as.data.frame(SSUs), stratanames = "TRACT",
            size = rep(2,m), method="srswor")
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

    # select srswor from each sample BG
n.BG <- m*2
s <- strata(data = as.data.frame(SSUdat), stratanames = "trtBG",
            size = rep(50,n.BG), method="srswor")
s <- rename(s, c(Prob = "p3i"))
samclus <- getdata(SSUdat, s)
del <- (1:ncol(samclus))[dimnames(samclus)[[2]] %in% c("ID_unit","Stratum")]
samclus <- samclus[, -del]

table(samclus$TRACT)
table(samclus$trtBG)

    # extract pop counts for PSUs in sample
pick <- names(Ni) %in% sort(unique(samclus$TRACT))
Ni.sam <- Ni[pick]
    # extract pop counts for SSUs in sample
pick <- names(Qij) %in% sort(unique(samclus$trtBG))
Qij.sam <- Qij[pick]

pp <- Ni.sam / sum(Ni)
wt <- 1 / samclus$p1i / samclus$p2i / samclus$p3i

BW3stagePPSe(Ni=Ni.sam, ni=rep(2,m), Qij=Qij.sam, qij=rep(50,n.BG),
      X = samclus$y1, psuID=samclus$TRACT, ssuID=samclus$trtBG, w=wt, m=20, pp=pp)
      