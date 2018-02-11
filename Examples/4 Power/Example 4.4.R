#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.4.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--one sample test
#*********************************************************************************************************

                    # Note that other values of sample size could be used
                    # other than n=1000. The key is that sd/sqrt(n) = 3300.
power.t.test(
    n = 1000,
    power = NULL,
    delta = 5000,
    sd = 3300*sqrt(1000),       # sd/sqrt(n) = 3300
    type = "one.sample",
    alt = "one.sided",
    sig.level = 0.05
    )

     One-sample t test power calculation

              n = 1000
          delta = 5000
             sd = 104355.2
      sig.level = 0.05
          power = 0.4479952
    alternative = one.sided
