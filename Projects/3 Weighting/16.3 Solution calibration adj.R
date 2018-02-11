#****************************************************************************************
# FILE:     18.3 Solution calibration adj.R                                              
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples                   
# DATE:     12/31/2010                                                                   
# AUTHOR:   R. Valliant                                                                  
# REVISED:  
#****************************************************************************************

require(rpart)
require(doBy)
require(Hmisc)
require(sampling)
require(survey)

# Step 4. CALIBRATION
file_loc1 <- "C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Project Weighting\\Data files\\"
file_loc2 <- "C:\\Projects\\Practical Tools Book\\Book Chapters\\16 Solution Weighting\\"

attach(paste(file_loc2, "sofr.d2.RData", sep=""))

datafile <- sofr.d2[sofr.d2$resp==1,]
dim(datafile)
#[1] 25559    34

            # Variables RA006A, RA006B, RA008, RA115, RA118 are coded as
            # 1 = very dissatisfied
            # 2 = dissatisfied
            # 3 = neither satisfied or dissatisfied
            # 4 = satisfied
            # 5 = very satisfied
            
            # recode as 
            # 0 = 1-3
            # 1 = 4-5

        # total compensation
        
datafile$ra006aR <- datafile$ra006a
a<- (datafile$ra006a %in% 1:3) 
datafile$ra006aR[a] <- 0
datafile$ra006aR[!a] <- 1
table(datafile$ra006a)
cumsum(table(datafile$ra006a))
table(datafile$ra006aR)
        # type of work    
datafile$ra006bR <- datafile$ra006b
a<- (datafile$ra006b %in% 1:3) 
datafile$ra006bR[a] <- 0
datafile$ra006bR[!a] <- 1
table(datafile$ra006b)
cumsum(table(datafile$ra006b))
table(datafile$ra006bR)
        # likely to re-enlist
datafile$ra008R <- datafile$ra008
a<- (datafile$ra008 %in% 1:3) 
datafile$ra008R[a] <- 0
datafile$ra008R[!a] <- 1
table(datafile$ra008)
cumsum(table(datafile$ra008))
table(datafile$ra008R)
        # prepared for job
datafile$ra115R <- datafile$ra115
a<- (datafile$ra115 %in% 1:3) 
datafile$ra115R[a] <- 0
datafile$ra115R[!a] <- 1
table(datafile$ra115)
cumsum(table(datafile$ra115))
table(datafile$ra115R)
        # level of stress
        # 1 Much less than usual 
        # 2 Less than usual 
        # 3 About the same as usual
        # 4 More than usual
        # 5 Much more than usual
datafile$ra118R <- datafile$ra118
a<- (datafile$ra118 %in% 1:3) 
datafile$ra118R[a] <- 0
datafile$ra118R[!a] <- 1
table(datafile$ra118)
cumsum(table(datafile$ra118))
table(datafile$ra118R)

#--------------------------------------------------------------------------------------------------
#   Use regression trees to help identify interactions of frame vars to use in calibration

        # compensation
t1 <- rpart(ra006aR ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 250, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#  1) root 25559 8813 1 (0.3448 0.6552)  
#    2) xcpay1r< 3.5 11272 5067 1 (0.4495 0.5505)  
#      4) xcpay1r< 2.5 5619 2767 1 (0.4924 0.5076)  
#        8) xsrrcr< 4.5 3886 1799 0 (0.5371 0.4629)  
#         16) xsrrcr>=3.5 639  235 0 (0.6322 0.3678) *
#         17) xsrrcr< 3.5 3247 1564 0 (0.5183 0.4817)  
#           34) sred>=2.5 2507 1175 0 (0.5313 0.4687)  
#             68) xsrrcr< 2.5 1716  775 0 (0.5484 0.4516)  
#              136) xsrrcr>=1.5 820  346 0 (0.5780 0.4220) *
#              137) xsrrcr< 1.5 896  429 0 (0.5212 0.4788)  
#                274) xreth4r>=1.5 542  243 0 (0.5517 0.4483) *
#                275) xreth4r< 1.5 354  168 1 (0.4746 0.5254) *
#             69) xsrrcr>=2.5 791  391 1 (0.4943 0.5057)  
#              138) xact2r>=2.5 447  203 0 (0.5459 0.4541) *
#              139) xact2r< 2.5 344  147 1 (0.4273 0.5727) *
#           35) sred< 2.5 740  351 1 (0.4743 0.5257) *
#        9) xsrrcr>=4.5 1733  680 1 (0.3924 0.6076) *
#      5) xcpay1r>=2.5 5653 2300 1 (0.4069 0.5931) *
#    3) xcpay1r>=3.5 14287 3746 1 (0.2622 0.7378) *


        # type of work
t1 <- rpart(ra006bR ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 100, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#  1) root 25559 5567 1 (0.2178 0.7822)  
#    2) xcpay1r< 3.5 11272 3369 1 (0.2989 0.7011)  
#      4) xcpay1r< 2.5 5619 1978 1 (0.3520 0.6480)  
#        8) xsrrcr< 4.5 3886 1543 1 (0.3971 0.6029)  
#         16) sred>=3.5 2216  949 1 (0.4282 0.5718)  
#           32) srmarst>=3.5 1113  514 1 (0.4618 0.5382)  
#             64) xact2r< 2.5 547  270 1 (0.4936 0.5064)  
#              128) xsrrcr< 2.5 332  157 0 (0.5271 0.4729) *
#              129) xsrrcr>=2.5 215   95 1 (0.4419 0.5581) *
#             65) xact2r>=2.5 566  244 1 (0.4311 0.5689) *
#           33) srmarst< 3.5 1103  435 1 (0.3944 0.6056) *
#         17) sred< 3.5 1670  594 1 (0.3557 0.6443) *
#        9) xsrrcr>=4.5 1733  435 1 (0.2510 0.7490) *
#      5) xcpay1r>=2.5 5653 1391 1 (0.2461 0.7539) *
#    3) xcpay1r>=3.5 14287 2198 1 (0.1538 0.8462) *


        # re-enlist
t1 <- rpart(ra008R ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 250, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
# 1) root 25559 5605 1 (0.2193 0.7807)  
#   2) xcpay1r< 2.5 5619 2257 1 (0.4017 0.5983)  
#     4) xsrrcr< 4.5 3886 1719 1 (0.4424 0.5576)  
#       8) xsrrcr>=3.5 639  241 0 (0.6228 0.3772) *
#       9) xsrrcr< 3.5 3247 1321 1 (0.4068 0.5932)  
#        18) xsrrcr< 2.5 2278 1036 1 (0.4548 0.5452)  
#          36) xact2r< 2.5 1221  600 0 (0.5086 0.4914)  
#            72) srmarst>=4 636  276 0 (0.5660 0.4340) *
#            73) srmarst< 4 585  261 1 (0.4462 0.5538) *
#          37) xact2r>=2.5 1057  415 1 (0.3926 0.6074) *
#        19) xsrrcr>=2.5 969  285 1 (0.2941 0.7059) *
#     5) xsrrcr>=4.5 1733  538 1 (0.3104 0.6896) *
#   3) xcpay1r>=2.5 19940 3348 1 (0.1679 0.8321) *

    # Figure 16.3 regression tree
postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\16 Solution Weighting\\Fig 16.3.eps",
          width=14,
          height=11)
par(mfrow = c(1,1))
plot(t1, uniform=TRUE, compress=TRUE, margin = 0.1, branch=0)
text(t1, use.n=TRUE, all=TRUE,
     digits=15,
     cex=1,
     pretty=1.2,
     fancy=TRUE,
     fwidth=0.6,        # set width of node enclosures; works with pretty
     fheight=0.6,       # set height of node enclosures
     xpd = TRUE,
     font = 3)
dev.off()

        # well prepared
t1 <- rpart(ra115R ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 100, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
# 1) root 25559 4156 1 (0.1626 0.8374)  
#   2) xcpay1r< 2.5 5619 1622 1 (0.2887 0.7113)  
#     4) xact2r>=2.5 2579  952 1 (0.3691 0.6309)  
#       8) xsexr>=1.5 636  306 1 (0.4811 0.5189)  
#        16) xreth4r>=1.5 384  168 0 (0.5625 0.4375) *
#        17) xreth4r< 1.5 252   90 1 (0.3571 0.6429) *
#       9) xsexr< 1.5 1943  646 1 (0.3325 0.6675) *
#     5) xact2r< 2.5 3040  670 1 (0.2204 0.7796) *
#   3) xcpay1r>=2.5 19940 2534 1 (0.1271 0.8729) *


        # stress
t1 <- rpart(ra118R ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "class",
                   control = rpart.control(minbucket = 400, cp=0),
                   data = datafile)

print(t1, digits=4)

#n= 25559 
#
#node), split, n, loss, yval, (yprob)
#      * denotes terminal node
#
#  1) root 25559 11560 0 (0.5479 0.4521)  
#    2) xreth4r>=1.5 8726  3513 0 (0.5974 0.4026) *
#    3) xreth4r< 1.5 16833  8042 0 (0.5222 0.4778)  
#      6) xact2r>=2.5 7845  3450 0 (0.5602 0.4398)  
#       12) xcpay1r< 5.5 3796  1569 0 (0.5867 0.4133) *
#       13) xcpay1r>=5.5 4049  1881 0 (0.5354 0.4646)  
#         26) xsrrcr< 3.5 2064   924 0 (0.5523 0.4477) *
#         27) xsrrcr>=3.5 1985   957 0 (0.5179 0.4821)  
#           54) xcpay1r< 6.5 641   293 0 (0.5429 0.4571) *
#           55) xcpay1r>=6.5 1344   664 0 (0.5060 0.4940)  
#            110) xsrrcr>=4.5 727   348 0 (0.5213 0.4787) *
#            111) xsrrcr< 4.5 617   301 1 (0.4878 0.5122) *
#      7) xact2r< 2.5 8988  4396 1 (0.4891 0.5109)  
#       14) xcpay1r< 4.5 4713  2234 0 (0.5260 0.4740)  
#         28) xsrrcr>=4.5 2016   877 0 (0.5650 0.4350) *
#         29) xsrrcr< 4.5 2697  1340 1 (0.4968 0.5032)  
#           58) srmarst>=2.5 1033   475 0 (0.5402 0.4598) *
#           59) srmarst< 2.5 1664   782 1 (0.4700 0.5300) *
#       15) xcpay1r>=4.5 4275  1917 1 (0.4484 0.5516)  
#         30) srmarst>=2.5 824   409 0 (0.5036 0.4964) *
#         31) srmarst< 2.5 3451  1502 1 (0.4352 0.5648) *

        # days in compensated status
t1 <- rpart(ra112ra ~ xsrrcr + xsexr + xcpay1r + xreth4r + sred + srmarst + xact2r,
                   method = "anova",
                   control = rpart.control(minbucket = 750, cp=0),
                   data = datafile)

print(t1, digits=4)
#n=17992 (7567 observations deleted due to missingness)
#
#node), split, n, deviance, yval
#      * denotes terminal node
#
# 1) root 17992 194800000  78.11  
#   2) xact2r>=2.5 8177  21190000  39.37  
#     4) xcpay1r< 4.5 3975  10710000  32.56  
#       8) xsrrcr< 3.5 2351   4688000  27.76  
#        16) sred< 3.5 816    976100  21.70 *
#        17) sred>=3.5 1535   3666000  30.98 *
#       9) xsrrcr>=3.5 1624   5889000  39.51 *
#     5) xcpay1r>=4.5 4202  10120000  45.81  
#      10) xsrrcr< 4.5 3013   5580000  42.81  
#        20) xsrrcr< 2.5 1655   2866000  40.87 *
#        21) xsrrcr>=2.5 1358   2700000  45.18 *
#      11) xsrrcr>=4.5 1189   4443000  53.41 *
#   3) xact2r< 2.5 9815 151200000 110.40  
#     6) xcpay1r< 3.5 4430  61350000  89.61  
#      12) xsrrcr< 4.5 2905  35340000  76.21  
#        24) xcpay1r< 2.5 1358  13240000  64.01 *
#        25) xcpay1r>=2.5 1547  21720000  86.91 *
#      13) xsrrcr>=4.5 1525  24490000 115.20 *
#     7) xcpay1r>=3.5 5385  86340000 127.50  
#      14) xsrrcr< 3.5 2830  42440000 110.50  
#        28) xreth4r>=1.5 818  12020000  94.55 *
#        29) xreth4r< 1.5 2012  30130000 116.90 *
#      15) xsrrcr>=3.5 2555  42170000 146.30  
#        30) xsrrcr< 5.5 1597  25610000 136.50 *
#        31) xsrrcr>=5.5 958  16160000 162.50 *


#------------------------------------------------------------------------------------------------
# Tabulate population controls. Check for missing values.

pop.counts <- sasxport.get(paste(file_loc1,"rccpds57.xpt",sep=""))

        # tabulate number of missing for each variable
table(pop.counts$service, useNA="always")
#   1    2    3    4    5    6 <NA> 
# 859  854  762  551  704  631    4 
table(pop.counts$gender, useNA="always")
#   1    2 <NA> 
#2733 1628    4 
table(pop.counts$pg.group, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
# 484  891 1056  715  251  495  468    5 
table(pop.counts$raceth, useNA="always")
#   1    2 <NA> 
#2350 1992   23 
table(pop.counts$educcat, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
#  84  472  543  717  614  919  652  364 
table(pop.counts$marit, useNA="always")
#   1    2    3    4    5 <NA> 
#1724  298  969   70 1266   38 
table(pop.counts$activatd, useNA="always")
#   1    2    3 <NA> 
# 409 1897 1903  156 
 

#_____________________________________________________________________________________________
# Impute missing covariate values in pop count file. A random draw is made from allowable
# codes in proportion to the pop code-counts for non-missing records.

set.seed(-1570473091)
new.cnts <- pop.counts

impute <- function(col.no){
    rows <- 1:(nrow(cnts)-1)
    codes <- cnts[rows,1]
    prop <- cnts[rows,2]/sum(cnts[rows,2])
    n.impute <- sum(is.na(pop.counts[,col.no]))
    imp <- rep(0,n.impute)

    for (i in 1:n.impute){
        imp[i] <- codes[UPrandomsystematic(prop)==1]
    }
    NAs <- (1:nrow(pop.counts))[is.na(pop.counts[,col.no])]
    new.cnts[NAs,col.no] <- imp
    new.cnts
}


cnts <- summaryBy(count ~ service, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=1)

cnts <- summaryBy(count ~ gender, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=2)

cnts <- summaryBy(count ~ pg.group, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=3)

cnts <- summaryBy(count ~ raceth, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=4)

cnts <- summaryBy(count ~ educcat, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=5)

cnts <- summaryBy(count ~ marit, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=6)

cnts <- summaryBy(count ~ activatd, data = pop.counts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
new.cnts <- impute(col.no=7)


        # re-tabulate after imputation
table(new.cnts$service, useNA="always")
table(new.cnts$gender, useNA="always")
table(new.cnts$pg.group, useNA="always")
table(new.cnts$raceth, useNA="always")
table(new.cnts$educcat, useNA="always")
table(new.cnts$marit, useNA="always")
table(new.cnts$activatd, useNA="always")

#> table(new.cnts$service, useNA="always")
#   1    2    3    4    5    6 <NA> 
# 861  855  762  551  705  631    0 
#> table(new.cnts$gender, useNA="always")
#   1    2 <NA> 
#2736 1629    0 
#> table(new.cnts$pg.group, useNA="always")#
#   1    2    3    4    5    6    7 <NA> 
# 484  895 1057  715  251  495  468    0 
#> table(new.cnts$raceth, useNA="always")
#   1    2 <NA> 
#2363 2002    0 
#> table(new.cnts$educcat, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
#  92  528  598  821  655  984  687    0 
#> table(new.cnts$marit, useNA="always")
#   1    2    3    4    5 <NA> 
#1747  298  971   70 1279    0 
#> table(new.cnts$activatd, useNA="always")
#   1    2    3 <NA> 
# 419 1951 1995    0 

        # crosstab of sample counts, svc x pay
        # The sample is too thin in pay=5 (W1-W5 warrant officers)
        # These will be collapsed with pay=6 (O1-O3)
table(new.cnts$service, new.cnts$pg.group, useNA="always")
#         1   2   3   4   5   6   7 <NA>
#  1     82 177 199 120  88 118  77    0
#  2     89 160 186 144  81 101  94    0
#  3    125 186 206  77  23  72  73    0
#  4     72  74 135 109  56  39  66    0
#  5     59 148 173 146   2  92  85    0
#  6     57 150 158 119   1  73  73    0
#  <NA>   0   0   0   0   0   0   0    0

table(new.cnts$service, new.cnts$activatd, useNA="always")
#         1   2   3 <NA>
#  1    112 344 405    0
#  2     88 401 366    0
#  3     53 324 385    0
#  4     26 268 257    0
#  5     91 322 292    0
#  6     49 292 290    0
#  <NA>   0   0   0    0

#_____________________________________________________________________________________________
#  Recode: combine warrant officers with O1-O3
new.cnts$pg.group[new.cnts$pg.group==5] <- 6
table(new.cnts$service, new.cnts$pg.group, useNA="always")

#         1   2   3   4   6   7 <NA>
#  1     82 177 199 120 206  77    0
#  2     89 160 186 144 182  94    0
#  3    125 186 206  77  95  73    0
#  4     72  74 135 109  95  66    0
#  5     59 148 173 146  94  85    0
#  6     57 150 158 119  74  73    0
#  <NA>   0   0   0   0   0   0    0

#_____________________________________________________________________________________________

        # Tabulate new pop counts in categories to be used in calibration
                            
svc.pay <- summaryBy(count ~ service + pg.group, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
svc <- summaryBy(count.sum.w ~ service, data = svc.pay,
                            FUN = function(x) { c(sum.w = sum(x)) } )
pay <- summaryBy(count.sum.w ~ pg.group, data = svc.pay,
                            FUN = function(x) { c(sum.w = sum(x)) } )
                            
svc.act <- summaryBy(count ~ service + activatd, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
activated <- summaryBy(count.sum.w ~ activatd, data = svc.act,
                            FUN = function(x) { c(sum.w = sum(x)) } )
                            
educ <-  summaryBy(count ~ educcat, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
gender <-  summaryBy(count ~ gender, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
raceth <-  summaryBy(count ~ raceth, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )
marital <-  summaryBy(count ~ marit, data = new.cnts,
                            FUN = function(x) { c(sum.w = sum(x)) } )

        # check totals vs. N
        # After imputation all pop counts sum to 801,809 which is the total count in 
        # the RCCPDS57 pop count file.
(N <- sum(pop.counts$count))
#[1] 801809

sum(svc.pay[,3])
#[1] 801809
sum(svc[,2])
#[1] 801809
sum(pay[,2])
#[1] 801809
sum(svc.act[,3])
#[1] 801809
sum(activated[,2])
#[1] 801809
sum(educ[,2])
#[1] 801809
sum(gender[,2])
#[1] 801809
sum(raceth[,2])
#[1] 801809
sum(marital[,2])
#[1] 801809

                            
#--------------------------------------------------------------------------------------------------
# Impute missing values for marital status, education, and activation in the sample file

tmp2 <- datafile

impute.sam <- function(col.no){
    cnts <- table(tmp2[,col.no],useNA="always")
    
    codes <- as.numeric(names(cnts[-length(cnts)]))
    prop <- cnts[-length(cnts)]/sum(cnts[-length(cnts)])
    n.impute <- cnts[length(cnts)]
    imp <- rep(0,n.impute)

    for (i in 1:n.impute){
        imp[i] <- codes[UPrandomsystematic(prop)==1]
    }
    NAs <- (1:nrow(tmp2))[is.na(tmp2[,col.no])]
    tmp2[NAs,col.no] <- imp
    tmp2
}

        # col 4 = smarst
        # col 10 = sred
        # col 13 = xact2r

tmp2 <- impute.sam(4)
table(datafile$srmarst, useNA="always")
#    1     2     3     4     5  <NA> 
#16934   397  2538    75  5577    38 

table(tmp2$srmarst, useNA="always")
#    1     2     3     4     5  <NA> 
#16965   397  2542    75  5580     0 

tmp2 <- impute.sam(10)
table(datafile$sred, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
# 146 2059 2465 4967 2399 7750 4912  861 

table(tmp2$sred, useNA="always")
#   1    2    3    4    5    6    7 <NA> 
# 150 2128 2541 5143 2479 8031 5087    0 

tmp2 <- impute.sam(13)
table(datafile$xact2r, useNA="always")
#    1     2     3  <NA> 
#  611 12912 11814   222 

table(tmp2$xact2r, useNA="always")
#    1     2     3  <NA> 
#  620 13020 11919     0 

#--------------------------------------------------------------------------------------------------
# Recode warrant officers in sample file. Combine with O1-O3
table(tmp2$xcpay1r)
#   1    2    3    4    5    6    7 
#1494 4125 5653 3162 1356 3783 5986 

tmp2$xcpay1r[tmp2$xcpay1r==5] <- 6
table(tmp2$xcpay1r)
#   1    2    3    4    6    7 
#1494 4125 5653 3162 5139 5986 

sofr.cal <- tmp2

#_____________________________________________________________________________________________
# Compute unbounded greg weights
# Based on the Chapter 13 assignment, we can assume that the pop counts do not include 
# any ineligibles.  Thus, only eligible respondents will be calibrated.


# merge NR class and response rates onto sofr.rie file

#tmp1  <-  cbind(nr.class=as.numeric(names(wt.rr)), unwt.rr, wt.rr)
#sofr.cal  <-  merge(sofr.0.R, data.frame(tmp1), by="nr.class", all=TRUE)
#sofr.cal  <-  sofr.cal[order(sofr.cal$rec.id),]

        # no fpc's in sofr.cal.dsgn.  *****We could add them*****.
sofr.cal.dsgn  <-  svydesign(ids = ~0, # no clusters
                       strata = ~v.strat, 
                       data = data.frame(sofr.cal),
                       weights = ~d2)

        # check how design matrix is formed in calibrate
mm <- model.matrix(~ as.factor(xsrrcr) * as.factor(xcpay1r) 
                    + as.factor(xsrrcr) * as.factor(xact2r)
                    + as.factor(sred)
                    + as.factor(xsexr) 
                    + as.factor(xreth4r)
                    + as.factor(srmarst),
                    data = sofr.cal)
dimnames(mm)[[2]]

        # reorder the pop totals for the interaction terms to match way that calibrate creates model matrix
svc.pay1 <- svc.pay[order(svc.pay[,2]),]
svc.act1 <- svc.act[order(svc.act[,2]),]
del1 <- svc.pay1[,1]==1 | svc.pay1[,2]==1
del2 <- svc.act1[,1]==1 | svc.act1[,2]==1

pop.tots <- c(N,
              svc[-1,2],
              pay[-1,2],
              activated[-1,2],
              educ[-1,2],
              gender[-1,2],
              raceth[-1,2],
              marital[-1,2],
              svc.pay1[!del1,3],
              svc.act1[!del2,3])
length(pop.tots)              
              



sam.lin.ub  <-  calibrate(design = sofr.cal.dsgn,
                    formula = ~  as.factor(xsrrcr) * as.factor(xcpay1r) 
                    + as.factor(xsrrcr) * as.factor(xact2r)
                    + as.factor(sred)
                    + as.factor(xsexr) 
                    + as.factor(xreth4r) 
                    + as.factor(srmarst),
                    population = pop.tots,
                    bounds = c(-Inf,Inf),
                    calfun = c("linear")
)
summary(weights(sam.lin.ub))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.199   4.672  13.430  31.370  30.910 613.400 
sum(weights(sam.lin.ub))
# [1] 801809
sd(weights(sam.lin.ub))
#[1] 58.26436

        # compare to input NR-adjusted wts, d2
summary(sofr.cal$d2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.521   4.746  14.630  31.820  34.310 514.700 
sum(sofr.cal$d2)
#[1] 813341.9
sd(sofr.cal$d2)
#[1] 58.81208


# Check calibrated survey totals

svytotal(~as.factor(xsrrcr), sam.lin.ub)

#                    total        SE
#as.factor(xsrrcr)1 322243 1.639e-12
#as.factor(xsrrcr)2 190327 4.573e-13
#as.factor(xsrrcr)3  77022 2.192e-13
#as.factor(xsrrcr)4  36094 1.334e-13
#as.factor(xsrrcr)5 105101 3.068e-13
#as.factor(xsrrcr)6  71022 2.620e-13

svytotal(~interaction(as.factor(xsrrcr),as.factor(xcpay1r)), sam.lin.ub)
                                                       total        SE
#                                                       total        SE
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))1.1  51596 1.785e-12
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))2.1  29771 8.103e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))3.1   6246 3.874e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))4.1  16795 2.293e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))5.1   5538 5.438e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))6.1   2298 3.938e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))1.2 103700 1.014e-12
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))2.2  45769 5.762e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))3.2  12854 1.258e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))4.2   8297 7.488e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))5.2  18269 1.452e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))6.2   9159 7.231e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))1.3 103351 1.111e-12
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))2.3  49236 3.080e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))3.3  35896 1.955e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))4.3   5942 5.802e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))5.3  42361 2.847e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))6.3  28602 2.135e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))1.4  31213 7.910e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))2.4  28673 2.203e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))3.4   6492 6.435e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))4.4   1798 2.211e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))5.4  26621 2.808e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))6.4  15600 1.361e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))1.6  22019 4.306e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))2.6  17192 2.521e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))3.6   3760 4.521e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))4.6    833 2.370e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))5.6   4214 4.385e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))6.6   4106 3.331e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))1.7  10364 3.231e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))2.7  19686 1.137e-13
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))3.7  11774 5.705e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))4.7   2429 3.446e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))5.7   8098 8.252e-14
#interaction(as.factor(xsrrcr), as.factor(xcpay1r))6.7  11257 9.108e-14

svytotal(~as.factor(xsexr), sam.lin.ub)
#                   total        SE
#as.factor(xsexr)1 663223 1.396e-12
#as.factor(xsexr)2 138586 5.818e-13

svytotal(~as.factor(xcpay1r), sam.lin.ub)
#                     total        SE
#as.factor(xcpay1r)1 112244 1.361e-12
#as.factor(xcpay1r)2 198048 7.446e-13
#as.factor(xcpay1r)3 265388 8.189e-13
#as.factor(xcpay1r)4 110397 4.946e-13
#as.factor(xcpay1r)6  52124 2.391e-13
#as.factor(xcpay1r)7  63608 1.164e-13
#

svytotal(~as.factor(xreth4r), sam.lin.ub)
#                     total        SE
#as.factor(xreth4r)1 540811 1.618e-12
#as.factor(xreth4r)2 260998 7.619e-13

svytotal(~as.factor(sred), sam.lin.ub)
#                  total        SE
#as.factor(sred)1  11389 1.945e-12
#as.factor(sred)2 121695 7.935e-13
#as.factor(sred)3 117349 6.805e-13
#as.factor(sred)4 228600 7.803e-13
#as.factor(sred)5  99731 6.143e-13
#as.factor(sred)6 152684 5.190e-13
#as.factor(sred)7  70361 2.974e-13

svytotal(~as.factor(srmarst), sam.lin.ub)
#                     total        SE
#as.factor(srmarst)1 456439 1.957e-12
#as.factor(srmarst)2  11748 1.932e-13
#as.factor(srmarst)3  75037 4.772e-13
#as.factor(srmarst)4   3324 9.590e-14
#as.factor(srmarst)5 255261 1.063e-12

svytotal(~as.factor(xact2r), sam.lin.ub)
#                    total        SE
#as.factor(xact2r)1  37688 1.499e-12
#as.factor(xact2r)2 252804 6.578e-13
#as.factor(xact2r)3 511317 1.083e-12

        # append calibrated weights onto sofr.cal file
sofr.cal$d3 <- weights(sam.lin.ub)
plot(sofr.d1$d0, sofr.d1$d1)
plot(sofr.d2$d2[!is.na(sofr.d2$d2)], sofr.cal$d3,
     pch = 16, cex = 0.6)
abline(0,1)

quantile(sofr.cal$d3, seq(0,1,0.05))

#----------------------------------------------------------------------------------------------------------
#   Summarize weights at different stages

attach("C:\\Projects\\Practical Tools Book\\Book Chapters\\18 Solution Weighting\\sofr.d1.RData")
dim(sofr.d1)
#[1] 71701    24        sofr.d1 contains all original sample cases

        # base wts
summary(sofr.d1$d0)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   2.201   5.049  12.140  14.270 178.300 

        # wts adjusted for unknown eligibility
summary(sofr.d1$d1)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.023   2.251   5.163  12.410  14.590 182.300 

dim(sofr.d2)
#[1] 66148    33        sofr.d2 includes R + NR

summary(sofr.d2$d2)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#    1.521     4.746    14.630    31.820    34.310   514.700 40589.000 
            # 40589 NAs are nonrespondents

dim(sofr.cal)
#[1] 25559    38        sofr.cal includes only respondents

summary(sofr.cal$d3)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.181   4.725  13.450  31.370  30.660 610.000 

wt.sum <- rbind(summary(sofr.d1$d0),
      summary(sofr.d2$d1),
      summary(sofr.d2$d2[!is.na(sofr.d2$d2)]),
      summary(sofr.cal$d3))
wt.sum <- cbind(wt.sum, 
                Sum = c(sum(sofr.d1$d0),
                    sum(sofr.d2$d1),
                    sum(sofr.d2$d2[!is.na(sofr.d2$d2)]),
                    sum(sofr.cal$d3)),
                Cases = c(length(sofr.d1$d0), 
                length(sofr.d2$d1),
                length(sofr.d2$d2[!is.na(sofr.d2$d2)]),
                length(sofr.cal$d3))
            )
dimnames(wt.sum)[[1]] <- c("Base", 
                            "Adjusted for unknown eligibility",
                            "Adjusted for nonresponse",
                            "GREG")
wt.sum
#                                  Min. 1st Qu. Median  Mean 3rd Qu.  Max.      Sum Cases
#Base                             1.000   2.201  5.049 12.14   14.27 178.3 870373.0 71701
#Adjusted for unknown eligibility 1.023   2.251  5.050 12.30   14.59 182.3 813341.9 66148
#Adjusted for nonresponse         1.521   4.746 14.630 31.82   34.31 514.7 813341.9 25559
#GREG                             1.199   4.672 13.430 31.37   30.91 613.4 801809.0 25559


#----------------------------------------------------------------------------------------------------------
# Write out the calibrated design object, a text file, a SAS file, and a Stata file  based on sofr.cal

        # save calibrated design object
save(sam.lin.ub, file=paste(file_loc2,"sam.lin.ub.RData", sep=""))

        # specify fields for the text, SAS, and Stata files
fields <- c("rec.id", "nr.class", "respstat", "stratum",
            "nsamp", "nstrat", "v.strat",
            "srmarst", "sred", "xsrrcr", "xact2r", "xreth4r", "xsexr", "xcpay1r",   
            "ra006a", "ra006b", "ra008", "ra115", "ra118", "ra112ra", 
            "pred.logit", "p.class.10", "unwt.rr", "wt.rr",
            "d0", "d1", "a1", 
            "d2", "a2",         
            "d3")

write.foreign(df = sofr.cal[, fields], 
              datafile = paste(file_loc2, "sofr.cal.sas.csv", sep=""), 
              codefile = paste(file_loc2, "sofr.sas", sep=""), 
              package = "SAS")

write.foreign(df = sofr.cal[, fields], 
              datafile = paste(file_loc2, "sofr.cal.stata.csv", sep=""), 
              codefile = paste(file_loc2, "sofr.ado", sep=""), 
              package = "Stata")
