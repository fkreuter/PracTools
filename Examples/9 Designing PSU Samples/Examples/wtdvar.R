#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\wtd.var.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/23/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute weighted unit variance estimate
#*********************************************************************************************************

wtdvar <- function(x, w){
    xbarw <- sum(w*x) / sum(w)
    varw <- sum(w * (x-xbarw)^2) / sum(w)
    varw
}
