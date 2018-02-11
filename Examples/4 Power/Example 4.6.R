#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.6.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--two sample test
#*********************************************************************************************************

power.t.test(
    n = NULL,
    power = 0.8,
    delta = 5,
    sd = sqrt(200),
    type = "two.sample",
    alt = "one.sided",
    sig.level = 0.05
    )

     Two-sample t test power calculation

              n = 99.60428
          delta = 5
             sd = 14.14214
      sig.level = 0.05
          power = 0.8
    alternative = one.sided

 NOTE: n is number in *each* group
