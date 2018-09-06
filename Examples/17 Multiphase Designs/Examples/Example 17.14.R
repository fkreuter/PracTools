#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\17 Multiphase Designs\Examples\Example 17.14.R                
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     06/02/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Optimal sample sizes for a target CV
#*********************************************************************************************************

NRFUopt(Ctot=NULL, c1=75, c2=150, theta=0.7, CV0=0.10,
            CVpop=3, type.sw="cv")

$allocation
[1] "fixed CV"

$`Total variable cost`
[1] 107320.2

$`Response rate`
[1] 0.7

$CV
[1] 0.1

$v.opt
[1] 0.8452

$n1.opt
[1] 949

$`Expected n2`
[1] 241

$`Expected total cases (2-phase)`
[1] 1190

$`srs sample for same cv`
[1] 1286

$`Cost Ratio: Two phase to srs`
[1] 1.113

