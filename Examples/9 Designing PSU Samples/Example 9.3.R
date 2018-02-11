#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.3.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/05/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an srs/srs design for a ratio estimate.
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)

    # compute linear substitute to use in variance formula
    # estimate is the proportion of Hispanics who have insurance coverage
        # recode Hispanic to be 1=Hispanic, 0 if not
y2 <- abs(MDarea.pop$Hispanic - 2)
y1 <- y2 * MDarea.pop$ins.cov
    # proportion of Hispanics with insurance
p <- sum(y1) / sum(y2)
    # linear sub
z <- y1 - p*y2

trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
BW2stageSRS(z, psuID=MDarea.pop$TRACT)
BW2stageSRS(z, psuID=trtBG)

    # for comparison, take total hispanics with insurance
BW2stageSRS(y1, psuID=MDarea.pop$TRACT)
BW2stageSRS(y1, psuID=trtBG)
