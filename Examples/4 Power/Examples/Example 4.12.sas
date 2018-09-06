#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.12.sas
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--one sample in SAS
#*********************************************************************************************************

proc power;
    onesamplemeans
    mean = 60000
    ntotal = .
    stddev = 74000
    sides = 1
    nullmean = 55000
    power = 0.80;
run;
