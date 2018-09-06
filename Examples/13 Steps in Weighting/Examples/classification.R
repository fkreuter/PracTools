#*********************************************************************************************************
# FILE: C:\Projects\Practical Tools Book\Book Chapters\14 Steps in Weighting\Examples\classification.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    04/02/2010
# AUTHOR:  R. Valliant
# PURPOSE: Use rpart to create nonresponse adjustment cells using nhis data and same variables as
#           used in propensity model.
# REVISED: 02/19/2012 Saved Fig 13.7 as EPS.
#*********************************************************************************************************

#attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.RData", pos=2)
require(PracTools)
data(nhis)

            # check to see what response rates are in 2-way tables
orig <- table(nhis[,"resp"], nhis[,"hisp"])
round( orig / rbind(apply(orig, 2, sum), apply(orig, 2, sum)), 3)

race <- table(nhis[,"resp"], nhis[,"race"])
round( race / rbind(apply(race, 2, sum), apply(race, 2, sum)), 3)

paren <- table(nhis[,"resp"], nhis[,"parents"])
round( paren / rbind(apply(paren, 2, sum), apply(paren, 2, sum)), 3)

educ1 <- table(nhis[,"resp"], nhis[,"educ"])
round( educ1 / rbind(apply(educ1, 2, sum), apply(educ1, 2, sum)), 3)

rm(orig, race, paren, educ1)


#---------------------------------------------------------------------------------------
#  Tree using rpart
#---------------------------------------------------------------------------------------

require(rpart)
set.seed(15097)
nhis <- data.frame(nhis)

            # run tree
t1 <- rpart(resp ~ age + hisp + race + parents_r + educ_r,
      method = "class",
      control = rpart.control(minbucket = 50, cp=0),
      data = nhis)
print(t1, digits=4)

            # plot tree Fig 13.7
postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig 13.7.eps")
par(mfrow=c(1,1))
plot(t1, uniform=TRUE, compress=TRUE, margin = 0.1)
text(t1, use.n=TRUE, all=TRUE,
     digits=4,
     cex=1.2,
     pretty=1.2,
     fancy=TRUE,
     xpd = TRUE,
     font = 3)
#   title("Tree for identifying nonresponse adjustment cells in the NHIS data set")
dev.off()

            # count cases in each terminal node
table(t1$where)
#   3    6    7    8    9
# 588   67  210 1099 1947

            # crosstab of NR/R by terminal node
table(nhis$resp, t1$where)
#       3    6    7    8    9
#  0  239   36   75  339  523
#  1  349   31  135  760 1424

            # proportions of NR, R in each node
table(nhis$resp, t1$where) / rbind(table(t1$where), table(t1$where))
#            3         6         7         8         9
#  0 0.4064626 0.5373134 0.3571429 0.3084622 0.2686184
#  1 0.5935374 0.4626866 0.6428571 0.6915378 0.7313816


            # compute NR adjustments based on classes formed by the tree
            # Unweighted response rate
unwt.rr <- by(as.numeric(nhis[, "resp"]), t1$where, mean)
unwt.rr
            # Weighted response rate
wt.rr <- by(data = data.frame(resp = as.numeric(nhis[,"resp"]), wt = nhis[,"svywt"]),
   t1$where,
   function(x) {weighted.mean(x$resp, x$wt)}
)
wt.rr

            # merge NR class and inverse RR adjustment onto nhis file
nhis.NR <- cbind(nhis, NR.class=t1$where)
tmp1 <- cbind(NR.class=as.numeric(names(wt.rr)), unwt.rr, wt.rr)

nhis.NR <- merge(nhis.NR, data.frame(tmp1), by="NR.class")
nhis.NR <- nhis.NR[order(nhis.NR$ID),]
nhis.NR[1:15,]

rm(nhis.NR)
