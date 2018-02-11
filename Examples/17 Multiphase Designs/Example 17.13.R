#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\Example 17.16.R                
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     06/02/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Optimal sample sizes for a fixed budget
#*********************************************************************************************************

NRFUopt(Ctot=100000, c1=50, c2=200, theta=0.5, CV0=NULL, CVpop=1, type.sw="cost")

$allocation
[1] "fixed cost"

$`Total variable cost`
[1] 1e+05

$`Response rate`
[1] 0.5

$CV
[1] 0.0382

$v.opt
[1] 0.7071

$n1.opt
[1] 828

$`Expected n2`
[1] 293

$`Expected total cases (2-phase)`
[1] 1121

$`srs sample for same cv`
[1] 1373

$`Cost Ratio: Two phase to srs`
[1] 1.457

