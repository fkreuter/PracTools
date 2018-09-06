#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\BW3stagePPSe.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/19/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs/srs design
#*********************************************************************************************************

# dat   = data frame with psuID, ssuID, w1i, w2ij, w, and any no. of data columns
#         The columns must contain columns named psuID, ssuID, w1i, w2ij, and w.
# v     = name or no. of column in dat with variable to be analyzed
# Ni    = no. of SSUs in pop in PSU i
# ni    = no. of elements in sample in PSU i (should be able to count those from dat)
# Qi    = no. of elements in pop in sample PSU i
# Qij   = no. of elements in pop in sample SSU ij
# qij   = no. of sample elements in pop in sample SSU ij (should be able to count those from dat)
# X     = data vector
# psuID = vector of PSU IDs (length is same as length of X)
# w     = full sample weight
# w1i    = vector of wts for PSUs
# w2ij   = vector of wts for SSUs (PSU wt * SSU wt within PSU)
# m     = no. of sample PSUs
      # pp    = vector of 1-draw selection probabilities (length is same as length of X)
      #         don't input; assume that wi = 1/(m*pp) so pp = 1/(m*wi)

      
BW3stagePPSe <- function(dat, v, Ni, Qi, Qij, m){
    y <- dat[, v]
        # 3rd stage conditional wts
    wk.ij <- dat$w / dat$w2ij
        # 3rd stage sample counts
    qij <- table(dat$ssuID)
    qbbar <- mean(qij)

        # extract first row of dat for each PSU
    xx.psu <- do.call("rbind",list(by(1:nrow(dat),dat$psuID,head,1)))
    w1i.psu <- dat[xx.psu, "w1i"]
        # compute PSU 1-draw probs
        # assume that w1i = m*pp
    pp <- 1/(m * dat[xx.psu,]$w1i)
        # extract first row of dat for each SSU
    xx.ssu <- do.call("rbind",list(by(1:nrow(dat),dat$ssuID,head,1)))
    ni <- table(dat[xx.ssu, "psuID"])
    nbar <- mean(ni)
    w2ij.ssu <- dat[xx.ssu, "w2ij"]
        # estimated no. of PSUs
    M.hat <- sum(w1i.psu)

#    Qi.hat <-  by(wij * Qij, INDICES = rep(unique(psuID),ni), sum)
    Qbbar <- sum(w2ij.ssu*Qij) / sum(w2ij.ssu)    # estimated mean no. of elements per SSU
    Qbar <- sum(w1i.psu*Qi) / M.hat              # estimated mean no. of elements per PSU

#    ybar.ij <- by(X, INDICES = ssuID, mean)
#    tij <- Qij * ybar.ij
#    ti <- Ni / ni * sum(tij)

    tij <- by(wk.ij*y, dat$ssuID, sum)
    ti <- by(as.vector(w2ij.ssu*tij), dat[xx.ssu,]$psuID, sum)
    t.pwr <- sum(dat$w * y)

#    S2i <- by(as.vector(tij), INDICES = rep(unique(dat$psuID),ni), var)
    S2i <- by(as.vector(tij), INDICES = dat[xx.ssu,]$psuID, var)
    S3ij <- by(y, INDICES = dat$ssuID, var)
    sumS3i <- by(as.vector(S3ij), INDICES = dat[xx.ssu,]$psuID, sum)
    V3ij <- Qij * (Qij/qij - 1) * S3ij

#    sV3i <- by(as.vector(V3ij), INDICES = rep(unique(psuID),ni), sum)
    sV3i <- by(as.vector(V3ij), INDICES = dat[xx.ssu,]$psuID, sum)
#    V2i <- Ni * (Ni/ni - 1)* S2i + sV3i
    V2i <- Ni^2/nbar * S2i + Ni/nbar*Qbbar^2/qbbar * sumS3i
    V1 <- sum((ti/pp - t.pwr)^2)/m/(m-1) + sum(V2i * w1i.psu)
        # within PSU unit var estimate
    ybar.hat <- weighted.mean(y, dat$w)

    V3i <- by(y, INDICES = dat$psuID, wtdvar, w = dat$w)
    
#    Vtsu <- sum(Ni^2/ni^2 / pi.star^2 * sV3i)
#    Vssu <- sum(V2i/pi.star^2) - Vtsu
    Vtsu <- sum(Ni^2/ni^2 * w1i.psu^2 * sV3i)
    Vssu <- sum(V2i * w1i.psu^2) - Vtsu
    Vpsu <- V1 - Vssu - Vtsu
browser()
#    B <-  sum((ti/pp - t.pwr)^2)/(m-1) - sum((1-pi.star)/m/pp^2 * V2i)
    B <-  sum((ti/pp - t.pwr)^2)/(m-1) - sum(V2i/m/pp^2)
    B <- B / t.pwr^2
    W <- sum(V3i * w1i.psu) / (M.hat * ybar.hat)^2
    
    W2 <- sum(Ni^2/m/pp^2 * S2i) + Qbbar^2/qbbar * sum(Ni/m/pp^2*sumS3i) -
            Qbbar^2/qbbar * sum(Ni^2/nbar/m/pp^2 * sumS3i)
    W2 <- W2 / t.pwr^2
#    W3 <- sum(Ni^2/nbar/m/pp^2 * sV3i)
#    W3 <- Qbbar^2 * sum(Ni^2/nbar/m/pp^2 * sV3i)
    W3 <- Qbbar^2 * sum(Ni^2/nbar/m/pp^2 * sumS3i)
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
Qi <- table(MDarea.pop$TRACT)

Qij <- table(MDpop$trtBG)
m <- 20
probi <- m*Qi / sum(Qi)
    # select sample of clusters
#set.seed(-780087528)
set.seed(-1055331180)
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
pick <- names(Qi) %in% sort(unique(samclus$TRACT))
Qi.sam <- Qi[pick]
    # extract pop counts of SSUs for PSUs in sample
pick <- names(Ni) %in% sort(unique(samclus$TRACT))
Ni.sam <- Ni[pick]
    # extract pop counts for SSUs in sample
pick <- names(Qij) %in% sort(unique(samclus$trtBG))
Qij.sam <- Qij[pick]

#pp <- Ni.sam / sum(Ni)
wt <- 1 / samclus$p1i / samclus$p2i / samclus$p3i
w1i <- 1 / samclus$p1i
w2ij <- 1 / samclus$p1i / samclus$p2i
samdat <- data.frame(psuID = samclus$TRACT, ssuID = samclus$trtBG,
                w1i = w1i, w2ij = w2ij, w = wt,
                samclus[, c("y1","y2","y3","ins.cov", "hosp.stay")])

#BW3stagePPSe(Ni=Ni.sam, ni=rep(2,m), Qij=Qij.sam, qij=rep(50,n.BG),
#      X = samclus$y1, psuID=samclus$TRACT, ssuID=samclus$trtBG,
#      w1i=w1i, w2ij=w2ij, w=wt, m=20, pp=pp)
      
BW3stagePPSe(dat=samdat, v="y1", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m)                 ..
BW3stagePPSe(dat=samdat, v="y2", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m)
BW3stagePPSe(dat=samdat, v="y3", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m)
BW3stagePPSe(dat=samdat, v="ins.cov", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m)
BW3stagePPSe(dat=samdat, v="hosp.stay", Ni=Ni.sam, Qi=Qi.sam, Qij=Qij.sam, m)

