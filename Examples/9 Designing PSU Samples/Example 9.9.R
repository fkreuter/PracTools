#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples
#               \Examples\Example 9.9.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/08/2012
# PGMR:     R. Valliant
# PURPOSE:  Compute optimal value of number of elements in 2-stage sampling when the
#           set of PSUs is fixed.
#**********************************************************************************

clusOpt2fixedPSU(C1=500, C2=100,
                 m=100,
                 delta=0.05,
                 unit.rv=2,
                 k=1,
                 CV0=NULL,
                 tot.cost=c(100000, 500000, 10^6),
                 cal.sw=1)
