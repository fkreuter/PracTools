#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.5.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--one sample test
#*********************************************************************************************************

power.t.test(
    n = NULL,
    power = 0.8,
    delta = 5000,
    sd = 74000,
    type = "one.sample",
    alt = "one.sided",
    sig.level = 0.05
    )

     One-sample t test power calculation

              n = 1355.581
          delta = 5000
             sd = 74000
      sig.level = 0.05
          power = 0.8
    alternative = one.sided
