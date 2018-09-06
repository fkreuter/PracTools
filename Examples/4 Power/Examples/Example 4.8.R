#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.8.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--Two-sample test on proportions with independent samples
#*********************************************************************************************************

power.prop.test(power = 0.8,
    p1 = 0.15,
    p2 = 0.18,
    alt = "one.sided",
    sig.level = 0.05
)

     Two-sample comparison of proportions power calculation

              n = 1891.846
             p1 = 0.15
             p2 = 0.18
      sig.level = 0.05
          power = 0.8
    alternative = one.sided

 NOTE: n is number in *each* group

power.prop.test(n = 1000,
    p1 = 0.15,
    p2 = 0.18,
    alt = "one.sided",
    sig.level = 0.05
)

     Two-sample comparison of proportions power calculation

              n = 1000
             p1 = 0.15
             p2 = 0.18
      sig.level = 0.05
          power = 0.56456
    alternative = one.sided

 NOTE: n is number in *each* group
