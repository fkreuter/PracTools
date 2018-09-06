#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Example 4.7.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# DATE:    8/18/09
# AUTHOR:  R. Valliant
# PURPOSE: Sample size calculation based on power--Two-sample test on means with overlapping samples
#*********************************************************************************************************

nDep2sam(S2x=200, S2y=200,
    g=0.75, r=1, rho=0.9,
    alt="one.sided", del=5,
    sig.level=0.05, pow=0.80)

     Two-sample comparison of means
 Sample size calculation for overlapping samples

             n1 = 33
             n2 = 33
        S2x.S2y = 200, 200
          delta = 5
          gamma = 0.75
              r = 1
            rho = 0.9
            alt = one.sided
      sig.level = 0.05
          power = 0.8

nDep2sam(S2x=200, S2y=200, g=0.75, r=1, rho= 0.9,
    alt="two.sided", del=5, sig.level=0.05,
    pow=0.80)

     Two-sample comparison of means
 Sample size calculation for overlapping samples

             n1 = 41
             n2 = 41
        S2x.S2y = 200, 200
          delta = 5
          gamma = 0.75
              r = 1
            rho = 0.9
            alt = two.sided
      sig.level = 0.05
          power = 0.8

nDep2sam(S2x=200, S2y=200, g=1, r=1, rho= 0.9, alt="one.sided",
    del=5, sig.level=0.05, pow=0.80)

     Two-sample comparison of means
 Sample size calculation for overlapping samples

             n1 = 10
             n2 = 10
        S2x.S2y = 200, 200
          delta = 5
          gamma = 1
              r = 1
            rho = 0.9
            alt = one.sided
      sig.level = 0.05
          power = 0.8
