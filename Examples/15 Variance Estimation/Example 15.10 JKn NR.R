#*******************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\16 Variance          
#               Estimation\Examples\Example 16.9.R                              
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples          
# DATE:     05/25/2011                                                          
# PGMR:     R. Valliant                                                         
# PURPOSE:  Example 16.9 using JKn variance estimate and nonresponse adjustment 
#*******************************************************************************

       #attach 3911 record nhis file used to illustrate NR adjustment
attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.RData", pos=2)
require(survey)
require(rpart)
library(doBy)

       # create a design object
nhis.dsgn <- svydesign(ids = ~psu,
                        strata = ~stratum,
                        nest = TRUE,
                        data = nhis,
                        weights = ~svywt)
       # JKn
jkn.dsgn <- as.svrepdesign(design = nhis.dsgn, type = "JKn")

#---------------------------------------------------------------------------------------
#  Tree using rpart
#---------------------------------------------------------------------------------------

set.seed(15097)
nhis <- data.frame(nhis)
            
           # run tree
t1 <- rpart(resp ~ age + hisp + race + parents_r + educ_r,
     method = "class",
     control = rpart.control(minbucket = 50, cp=0),
     data = nhis)
print(t1, digits=2)

           # count cases in each terminal node
table(t1$where)
#   3    6    7    8    9
# 588   67  210 1099 1947

       # append NR classes to nhis object
nhis.NR <- data.frame(nhis, NR.class=t1$where)
       # wt adjustments for JKn (values are 0, 1, or 2)
JKwtadj <- weights(jkn.dsgn)
nreps <- ncol(JKwtadj)
fswts <- nhis$svywt
rep.adjwt <- matrix(0, nrow=nrow(JKwtadj), ncol=nreps)

       # compute NR adjustments for full sample
wt.rr <- by(data = data.frame(resp = as.numeric(nhis$resp), wt =fswts),
           nhis.NR$NR.class,
           function(x) {weighted.mean(x$resp, x$wt)})
tmp1 <- cbind(NR.class=as.numeric(names(wt.rr)), wt.rr)
sam.nr <- merge(nhis.NR, data.frame(tmp1), by = "NR.class")
sam.nr$fs.adjwt <- sam.nr$svywt / sam.nr$wt.rr
sam.nr <- data.frame(ID = sam.nr$ID, fs.adjwt = sam.nr$fs.adjwt, wt.rr = sam.nr$wt.rr)
sam.nr <- orderBy(~ID, data=sam.nr)
fs.adjwt <- sam.nr$fs.adjwt


       # compute NR adjustments for each replicate
for (r in 1:nreps){
   adjwts <- fswts * JKwtadj[,r]
           # wtd RR; adjwts=0 for units not in replicate
   wt.rr <- by(data = data.frame(resp = as.numeric(nhis.NR$resp), wt = adjwts),
               nhis.NR$NR.class,
               function(x) {weighted.mean(x$resp, x$wt)})
   tmp1 <- cbind(NR.class=as.numeric(names(wt.rr)), wt.rr)
   sam.nr <- merge(nhis.NR, data.frame(tmp1), by = "NR.class")
   sam.nr <- data.frame(sam.nr, wt.rr = sam.nr$wt.rr)
   sam.nr <- orderBy(~ID, data=sam.nr)
           # adjust rep wts for NR
   rep.adjwt[,r] <- adjwts / sam.nr$wt.rr
}

       # check sum of NR-adjusted weights in each replicate
sum(fs.adjwt[nhis$resp==1])
apply(rep.adjwt[sam.nr$resp==1,],2,sum)
       # ratio of sum of NR-adjusted wts to sum of full sample wts
range(apply(rep.adjwt[sam.nr$resp==1,],2,sum)/sum(fswts))

       # assign names to rep.adjwt columns and
       # append NR-adjusted weights onto nhis data file
rname <- vector("character", length=nreps)
for (r in 1:nreps){
   rname[r] <- paste("repwt",r,sep="")
}
dimnames(rep.adjwt)[[2]] <- rname

R <- nhis$resp == 1
nhis.NR <- cbind(nhis[R==1, ], fs.adjwt=fs.adjwt[R==1],
rep.adjwt[R==1,])
       # extract wts for respondents only
rep.adjwt <- rep.adjwt[R==1,]

       # JKn design object with NR-adjusted weights
jkn.NR.dsgn <- svrepdesign(data = nhis.NR[,1:16],
                           repweights = rep.adjwt,
                           type = "JKn",
                           weights = nhis.NR$fs.adjwt,
                           combined.weights = TRUE,
                           scale = 1,
                           rscales = rep(1/2,nreps)
)
      # tables & means; variance is computed by centering around mean of replicates
svymean(~factor(age_r), design=jkn.NR.dsgn)
svymean(~age, design=jkn.NR.dsgn)
svytotal(~factor(age_r), design=jkn.NR.dsgn)

svymean(~factor(marital), design=jkn.NR.dsgn)
svytotal(~factor(marital), design=jkn.NR.dsgn)
svytotal(~interaction(factor(hisp),factor(marital)), design=jkn.NR.dsgn)

       # 1-way table; replicate estimates are saved with return.replicates = TRUE
a <- svymean(~factor(age_r), design=jkn.NR.dsgn, return.replicates = TRUE)
b <- ftable(a, rownames = list(age_r = c("18-24 years","25-44 years","45-64 years",
                "65-69 years","70-74 years","75 years and older")))
round(b,4)


       # mean of rep estimates to compare to full sample estimates
apply(a$replicates,2,mean)

       # design without rep wts, ignores effect of NR adjustment
noNR.dsgn <- svydesign(ids = ~psu,
                        strata = ~stratum,
                        nest = TRUE,
                        data = nhis.NR,
                        weights = ~fs.adjwt)

      # tables & means
svymean(~factor(age_r), design=noNR.dsgn)
svymean(~age, design=noNR.dsgn)
svytotal(~factor(age_r), design=noNR.dsgn)

svymean(~factor(marital), design=noNR.dsgn)
svytotal(~factor(marital), design=noNR.dsgn)
svytotal(~interaction(factor(age_r),factor(marital)), design=noNR.dsgn)
