#**********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.12a.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/19/2012
# REVISED:  05/08/2014 error corrected in way Ni counted (no. of SSUs in each PSU)
# REVISED:  10/26/2016 changed call to cluster to use pik=as.vector(probi)
#                      coercing to vector needed as of R v.3.3.1
# AUTHOR:   R. Valliant
# PURPOSE:  Estimate variance components for a ppswr/srs/srs design using PSUs as PSUs, SSUs as SSUs.
#           Persons as 3rd stage units.
#**********************************************************************************************************

# select 3-stage sample from Maryland population
#attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)
require(PracTools)
data(MDarea.pop)
MDpop <- MDarea.pop

#require(doBy)
require(sampling)
require(reshape)      # has function that allows renaming variables


	# Another way to extract the first obs in a group is at http://www.ats.ucla.edu/stat/r/faq/firstlast.htm
	# Use something like highest <- by(hsb2.s, hsb2.s$prog, tail, n=1)
	

# make counts of SSUs and elements per PSU
#xx <- do.call("rbind",list(by(1:nrow(MDpop),MDpop$PSU,head,1)))
       # next stmt works for MDarea because SSUs are numbered across all PSUs, not within PSUs
xx <- do.call("rbind",list(by(1:nrow(MDpop),MDpop$SSU,head,1)))
pop.tmp <- MDpop[xx,]
Ni <- table(pop.tmp$PSU)
Qi <- table(MDarea.pop$PSU)
Qij <- table(MDpop$SSU)
m <- 30         # no. of PSUs to select
probi <- m*Qi / sum(Qi)

#-------------------------------------------------------------------------------
# select sample of clusters
set.seed(-1055331180)
sam <- cluster(data=MDpop, clustername="PSU", size=m, method="systematic",
               pik=as.vector(probi), description=TRUE)
sam[1:5, ]
# extract data for the sample clusters
samclus <- getdata(MDpop, sam)
samclus <- rename(samclus, c(Prob = "p1i"))
samclus <- samclus[order(samclus$PSU),]

table(samclus$PSU)
#   2    5    7   10   13   15   18   21   23   26   29   31   34   37   39   42   45   47   50   53   55   58   61   63   66   69
#5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050 5050
#  71   74   77   79
#5050 5050 5050 5050

#-------------------------------------------------------------------------------
# treat sample clusters as strata and select srswor of block groups from each
# identify psu IDs for 1st instance of each ssuID
xx <- do.call("rbind",list(by(1:nrow(samclus),samclus$SSU,head,1)))

SSUs <- cbind(PSU=samclus$PSU[xx], SSU=samclus$SSU[xx])
# select 2 SSUs per sample primary unit
n <- 2
s <- strata(data = as.data.frame(SSUs), stratanames = "PSU",
            size = rep(n,m), method="srswor")
s[1:5,]
#   PSU ID_unit Prob Stratum
#2    2       2  0.4       1
#4    2       4  0.4       1
#6    5       6  0.4       2
#7    5       7  0.4       2
#13   7      13  0.4       3

s <- rename(s, c(Prob = "p2i"))

# extract the SSU data
# s contains selection probs of SSUs, need to get those onto data file
SSUsam <- SSUs[s$ID_unit, ]
SSUsam <- cbind(SSUsam, s[, 2:3])
# identify rows in PSU sample that correspond to sample SSUs
tmp <- samclus$SSU %in% SSUsam$SSU
head(tmp)
#[1]  TRUE  TRUE  TRUE FALSE FALSE  TRUE

SSUdat <- samclus[tmp,]
SSUdat <- merge(SSUdat, SSUsam[, c("p2i","SSU")], by="SSU")
rm(tmp)

#-------------------------------------------------------------------------------
# select srswor from each sample SSU
n.SSU <- m*n
s <- strata(data = as.data.frame(SSUdat), stratanames = "SSU",
            size = rep(50,n.SSU), method="srswor")
s <- rename(s, c(Prob = "p3i"))
dim(s)
# [1] 3000    4         30 PSUs x 2 SSUs x 50 persons = 3000

samclus <- getdata(SSUdat, s)
    # drop the ID_unit ans Stratum columns in samclus
del <- (1:ncol(samclus))[dimnames(samclus)[[2]] %in% c("ID_unit","Stratum")]
samclus <- samclus[, -del]

table(samclus$PSU)
#  2   5   7  10  13  15  18  21  23  26  29  31  34  37  39  42  45  47  50  53  55  58  61  63  66  69  71  74  77  79
#100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100

table(samclus$SSU)
#  7   9  21  22  33  35  46  49  63  65  72  75  86  88 101 105 113 115 126 130 143 144 151 153 166 170 181 184 191 194 208 209 221
# 50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50
#222 231 235 248 249 264 265 272 273 286 288 301 304 311 313 328 330 341 342 353 355 366 370 381 383 391 392
# 50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50  50


#-------------------------------------------------------------------------------
# extract pop counts for PSUs in sample
pick <- names(Qi) %in% sort(unique(samclus$PSU))
Qi.sam <- Qi[pick]
# extract pop counts of SSUs for PSUs in sample
pick <- names(Ni) %in% sort(unique(samclus$PSU))
Ni.sam <- Ni[pick]
# extract pop counts for SSUs in sample
pick <- names(Qij) %in% sort(unique(samclus$SSU))
Qij.sam <- Qij[pick]

# compute full sample weight and wts for PSUs and SSUs
wt <- 1 / samclus$p1i / samclus$p2i / samclus$p3i
w1i <- 1 / samclus$p1i
w2ij <- 1 / samclus$p1i / samclus$p2i
samdat <- data.frame(psuID = samclus$PSU, ssuID = samclus$SSU,
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
#             Vpsu         Vssu         Vtsu           B           W           W2          W3       delta1      delta2
#[1,] 3.175701e+11 3.233347e+11 341799524205 0.012009354  1.36599040 0.0407578464  1.35988353 0.0087150628 0.029099416
#[2,] 9.382800e+08 1.133265e+09   2927630095 0.002900797  0.94809596 0.0116787190  0.95225051 0.0030502700 0.012115743
#[3,] 1.724750e+11 1.440654e+11  37358077399 0.004015478  0.09613492 0.0111801936  0.09150518 0.0400944796 0.108878154
#[4,] 4.508760e+06 7.767656e+05      8697689 0.001347165  0.27092937 0.0007736283  0.27341203 0.0049477813 0.002821549
#[5,] 1.913492e+05 4.296012e+05      3074180 0.008676956 14.54483156 0.0649359134 14.66628701 0.0005962106 0.004408046
