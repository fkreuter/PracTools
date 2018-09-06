#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.12b.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/28/2011
# REVISED:  10/26/2016 changed call to cluster to use pik=as.vector(probi)
#                      coercing to vector needed as of R v.3.3.1
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs/srs design using TRACTs as PSUs, trtBGs as SSUs.
#	    Persons as 3rd stage units.
#*********************************************************************************************************

    # select 3-stage sample from Maryland population
require(PracTools)
require(sampling)
require(reshape)      # has function that allows renaming variables

data(MDarea.pop)
trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
MDpop <- cbind(MDarea.pop, trtBG)


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

set.seed(-1055331180)
sam <- cluster(data=MDpop, clustername="TRACT", size=m, method="systematic",
                pik=as.vector(probi), description=TRUE)
sam[1:5,]
    # extract data for the sample clusters
samclus <- getdata(MDpop, sam)
samclus <- rename(samclus, c(Prob = "p1i"))
samclus <- samclus[order(samclus$TRACT, samclus$BLKGROUP),]

table(samclus$TRACT)
#701102 701300 702201 702202 702401 702601 706101 706402 707001 708000 730300 730502 730504
#  6119   6351   8437  13579   4382   4814   2666   1790   4941   7644   2841   4319   6066
#730604 730902 731101 731103 731202 731204 731307 731309 740102 740201 740301 740601 740800
#  5684   3044   7429   4905   7039   4711   5459   5263   3587   4866   5926   2789   3649
#750101 750400 750803 751102
#  3713   3756   4239   2271

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
s[1:5, ]

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
#701102 701300 702201 702202 702401 702601 706101 706402 707001 708000 730300 730502 730504
#   100    100    100    100    100    100    100    100    100    100    100    100    100
#730604 730902 731101 731103 731202 731204 731307 731309 740102 740201 740301 740601 740800
#   100    100    100    100    100    100    100    100    100    100    100    100    100
#750101 750400 750803 751102
#   100    100    100    100

table(samclus$trtBG)
#7011022 7011024 7013001 7013004 7022013 7022015 7022021 7022024 7024011 7024012 7026011 7026012
#     50      50      50      50      50      50      50      50      50      50      50      50
#7061011 7061012 7064021 7064022 7070012 7070013 7080001 7080005 7303002 7303003 7305021 7305022
#     50      50      50      50      50      50      50      50      50      50      50      50
#7305041 7305044 7306041 7306042 7309021 7309022 7311012 7311013 7311031 7311033 7312021 7312022
#     50      50      50      50      50      50      50      50      50      50      50      50
#7312043 7312044 7313072 7313073 7313091 7313092 7401021 7401022 7402011 7402014 7403011 7403012
#     50      50      50      50      50      50      50      50      50      50      50      50
#7406012 7406013 7408001 7408003 7501013 7501014 7504001 7504004 7508031 7508033 7511021 7511022
#     50      50      50      50      50      50      50      50      50      50      50      50


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
             Vpsu         Vssu         Vtsu           B           W        W2         W3      delta1      delta2
[1,] 391582830887 8.173328e+11 436357948099 0.015026586  1.46126792 0.1835437  1.7325745 0.010178583 0.095789318
[2,]   2752801717 1.059577e+10   3612041987 0.008489451  0.96804419 0.1913792  1.1527687 0.008693455 0.142379581
[3,] 407976009290 1.301054e+12  43128264915 0.009606860  0.09348129 0.1890784  0.1051165 0.093190737 0.642697769
[4,]     40038969 9.214905e+07      9799791 0.011898628  0.27423710 0.1658823  0.3014194 0.041583861 0.354979096
[5,]      1844705 4.059811e+05      3466875 0.085476862 14.77061286 0.1321288 16.6330557 0.005753658 0.007881144
