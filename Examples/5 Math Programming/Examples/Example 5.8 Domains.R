#*********************************************************************************************************
# FILE: C:\Projects\Practical Tools Book\Book Chapters\5 Math Programming\Examples\Example 5.6 Domains.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples, 2nd edition
# DATE:    06/05/2017
# AUTHOR:  R. Valliant
# PURPOSE: Compute inputs to optimization problem for domain estimates where domains cross strata
#*********************************************************************************************************

require(PracTools)
data(labor)

    # pop counts by stratum
(Nh <- table(labor$h))
#  1   2   3
#210 212  56

#--------------------------------------------------------------------------------------------------
    # wage tabs by race
race.p <- table(labor$h,labor$race)
#      1   2
#  1 188  22
#  2 190  22
#  3  51   5

race.p <- prop.table(wage.p, 1)
#             1          2
#  1 0.89523810 0.10476190
#  2 0.89622642 0.10377358
#  3 0.91071429 0.08928571


wage.m <- by(labor$WklyWage, list(labor$h,labor$race), mean)
wage.m <- data.frame(rep(1:3,2), c(rep(1,3),rep(2,3)), as.vector(wage.m) )
colnames(wage.m) <- c("h", "race", "mean")
#  h race     mean
#1 1    1 295.3989
#2 2    1 327.9737
#3 3    1 254.0784
#4 1    2 181.4091
#5 2    2 214.5909
#6 3    2 259.6000

by(labor$WklyWage, labor$race, mean)
#labor$race: 1
#[1] 304.9138
#------------------------------------------------------------------------------------
#labor$race: 2
#[1] 204.2857


wage.sd <- by(labor$WklyWage, list(labor$h,labor$race), sd)
wage.sd <- data.frame(rep(1:3,2), c(rep(1,3),rep(2,3)), as.vector(wage.sd) )
colnames(wage.sd) <- c("h", "race", "SD")
#  h race        SD
#1 1    1 181.75380
#2 2    1 222.07143
#3 3    1 148.07293
#4 1    2 102.61888
#5 2    2 128.76026
#6 3    2  18.78297

#--------------------------------------------------------------------------------------------------
    # stratum SDs for full pop
(wage.mf <- by(labor$WklyWage, labor$h, mean))
labor$h: 1
[1] 283.4571
------------------------------------------------------------------------------------
labor$h: 2
[1] 316.2075
------------------------------------------------------------------------------------
labor$h: 3
[1] 254.5714

(wage.sdf <- by(labor$WklyWage, labor$h, sd))
labor$h: 1
[1] 178.4368
------------------------------------------------------------------------------------
labor$h: 2
[1] 216.8529
------------------------------------------------------------------------------------
labor$h: 3
[1] 141.2818
