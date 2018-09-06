#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples
#               \Examples\Example 9.10.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/08/2012
# PGMR:     R. Valliant
# PURPOSE:  Compute optimal value of number of SSUs and elements in 3-stage sampling
#           when the set of PSUs is fixed.
#**********************************************************************************

clusOpt3fixedPSU(unit.cost=c(500, 100, 120),
                 m=100,
                 delta1=0.01, delta2=0.05,
                 unit.rv=1,
                 k1=1,
                 k2=1,
                 tot.cost=500000,
                 cal.sw=1)

clusOpt3fixedPSU(unit.cost=c(500, 100, 120),
                 m=100,
                 delta1=0.01, delta2=0.05,
                 unit.rv=4,
                 k1=1,
                 k2=1,
                 CV0=0.05,
                 cal.sw=2)
