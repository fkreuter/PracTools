#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.13.sas
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--two sample in SAS
#*********************************************************************************************************

proc power;
    twosamplefreq
    test = pchi
    refproportion = 0.15
    proportiondiff = 0.03
    sides = 1
    power = 0.80
    npergroup = .
;
run;
