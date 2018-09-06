#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.9.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--Two-sample test, effect of size of proportions
#*********************************************************************************************************

power.prop.test(n = 1000,
    p1 = 0.50,
    p2 = 0.53,
    alt = "one.sided",
    sig.level = 0.05
)

     Two-sample comparison of proportions power calculation

              n = 1000
             p1 = 0.5
             p2 = 0.53
      sig.level = 0.05
          power = 0.3810421
    alternative = one.sided

 NOTE: n is number in *each* group
