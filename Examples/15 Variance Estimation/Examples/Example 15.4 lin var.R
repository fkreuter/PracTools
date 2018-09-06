#****************************************************************************************************
# FILE C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\Examples\Example 16.4.R 
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                                
# DATE: 04/23/2011                                                                                   
# PGMR: R. Valliant                                                                                  
# PURPOSE: Example 16.4 using a linearization var est                                                
#****************************************************************************************************

require(sampling)
require(doBy)
require(survey)
require(PracTools)

data(nhis.large)


        # tab on design parameters
table(nhis.large$stratum, nhis.large$psu)
range(nhis.large$stratum)
summary(nhis.large$svywt)

        # create a design object 
nhis.dsgn <- svydesign(ids = ~psu,
                         strata = ~stratum,
                         nest = TRUE,           # clusters are renumbered within PSUs
                         data = nhis.large,
                         weights = ~svywt)

        # 1-way table
a <- svymean(~factor(age.grp), deff=TRUE, design=nhis.dsgn)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+")))
round(b,5)           

        # 2-way table
a <- svymean(~interaction(factor(age.grp),factor(race)), design = nhis.dsgn, deff=TRUE)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+"),
            race=c("White", "Black", "Other")))
100*round(b,6)
a <- svymean(~factor(age.grp), deff=TRUE, design=nhis.dsgn)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+")))
round(b,3)           

a <- svymean(~factor(hosp.stay), deff=TRUE, design=nhis.dsgn)
a                       
                         
                         
       
       
       
       
       
       
       
       
       
                         
                         
                         
fay.dsgn <- as.svrepdesign(adults.dsgn, type="Fay", fay.rho=0.5)
summary(fay.dsgn)

#------------------------------------------------------------------------------------------------------------------
#   (a) Find the marginal and overall percentages of the population for subgroups defined by age group (AgeCode) 
#   crossclassified by whether the person has hypertension (Hyper).  How does this design compare to a simple random sample?
#    What would be the consequences of ignoring the sample design in estimating these percentages?
#------------------------------------------------------------------------------------------------------------------

            # note that deffs are in %
a <- svymean(~interaction(factor(AGECODE),factor(HYPER)), design = fay.dsgn, na.rm=TRUE, deff=TRUE)
b <- ftable(a, rownames = list(AGECODE=1:7, HYPER=c("Yes","No")))
100*round(b,6)

#------------------------------------------------------------------------------------------------------------------
#   (b) Find the (arithmetic) mean high-density lipoprotein cholesterol (HDL_CHOL), overall and for subclasses
#    formed by race (BLACK) crossed with overweight (Overwght) status, and by age group crossed with race (BLACK).
#    Do overweight individuals have lower cholesterol than nonoverweight individuals overall? 
#    Do overweight individuals have lower cholesterol than nonoverweight individuals within race groups (black/nonblack)?  
#    (A high level of HDL cholesterol is considered to be cardio-protective.)
#       Results agree with Stata.
#------------------------------------------------------------------------------------------------------------------

            # overall
svymean(~HDL_CHOL, design = fay.dsgn, na.rm=TRUE)

            # remove missing OVERWGHT cases
rdesign.over <- subset(fay.dsgn, !is.na(OVERWGHT))

            # black x overwght
race.owt <- svyby(~HDL_CHOL, by = ~interaction(BLACK,OVERWGHT), 
                  FUN = svymean,
                  na.rm = TRUE,
                  design = rdesign.over,
                  cov.mat = TRUE)       # cov.mat needed for t-tests on diffs by race, overwght
race.owt
                  
            # black x agecode
svyby(~HDL_CHOL,by = ~interaction(BLACK,AGECODE), FUN=svymean, na.rm=TRUE, design=fay.dsgn)
            # test difference in mean HDL_CHOL between overwgt & not overwght

by.over <- svyby(~HDL_CHOL, by = ~ OVERWGHT, 
      FUN = svymean,
      design = rdesign.over,
      covmat = TRUE,
      na.rm = TRUE)
by.over

            # diff between overwght and not overwght
diff.over <- svycontrast(by.over, 
                 list(diff=(c(1,-1))), na.rm=T)
diff.over
diff.over[1] - qt(p=0.975, df=23)*SE(diff.over)
diff.over[1] + qt(p=0.975, df=23)*SE(diff.over)

            # overweight vs non-overweight: by race
            # this does not appear to account for covars when doing var of contrast
            
            # black diff
b.diff <- svycontrast(race.owt, list(diff = (c(1,0,-1,0))))
b.diff
b.diff[1] - qt(p=0.975, df=23)*SE(b.diff)
b.diff[1] + qt(p=0.975, df=23)*SE(b.diff)

            # non-black diff
nb.diff <- svycontrast(race.owt, list(diff = (c(0,1,0,-1))))
nb.diff
nb.diff[1] - qt(p=0.975, df=23)*SE(nb.diff)
nb.diff[1] + qt(p=0.975, df=23)*SE(nb.diff)

#------------------------------------------------------------------------------------------------------------------
#   (c) geometric means are not available in R survey
#------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
#   (d) Find the estimates and confidence intervals for all three quartiles of HDL cholesterol, overall, 
#   and for subclasses formed by race (black/nonblack) crossed with sex.  
#   Estimate the standard errors of the three quartiles for the whole population.
#------------------------------------------------------------------------------------------------------------------

# Note that these results are different from SUDAAN because of different interpolation methods

            # overall
hdlQ <- svyquantile(~HDL_CHOL, design = fay.dsgn, 
            quantiles = c(0.25, 0.50, 0.75),
            ci = TRUE,
            interval.type="quantile",
            na.rm = TRUE)
            
            # CI's must be computed by hand
hdlQ
            # 1st quartile
Q1 <- c(hdlQ[1] - qt(p=0.975, df=23)*SE(hdlQ)[1],
        hdlQ[1],
        hdlQ[1] + qt(p=0.975, df=23)*SE(hdlQ)[1])
            # median
Q2 <- c(hdlQ[2] - qt(p=0.975, df=23)*SE(hdlQ)[2],
        hdlQ[2],
        hdlQ[2] + qt(p=0.975, df=23)*SE(hdlQ)[2])
            # 3rd quartile
Q3 <- c(hdlQ[3] - qt(p=0.975, df=23)*SE(hdlQ)[3],
        hdlQ[3],
        hdlQ[3] + qt(p=0.975, df=23)*SE(hdlQ)[3])
rbind(Q1, Q2, Q3)
                        
            # by black x sex, 1st quartile 
Q1 <- svyby(~HDL_CHOL, ~BLACK+SEX, 
      fay.dsgn, 
      FUN = svyquantile, 
      quantile = 0.25, ci=TRUE, vartype='se', 
      interval.type='quantile',
      na.rm=T) 
cbind(Q1, lower = Q1[,3] - qt(p=0.975, df=23)*Q1[,4], upper = Q1[,3] + qt(p=0.975, df=23)*Q1[,4])

            # by black x sex, median 
Q2 <- svyby(~HDL_CHOL, ~BLACK+SEX, 
      fay.dsgn, 
      FUN = svyquantile, 
      quantile = 0.50, ci=TRUE, vartype='se', 
      interval.type='quantile',       
      na.rm=T) 
cbind(Q2, lower = Q2[,3] - qt(p=0.975, df=23)*Q2[,4], upper = Q2[,3] + qt(p=0.975, df=23)*Q2[,4])

            # by black x sex, 3rd quartile 
Q3 <- svyby(~HDL_CHOL, ~BLACK+SEX, 
      fay.dsgn, 
      FUN = svyquantile, 
      quantile = 0.75, ci=TRUE, vartype='se', 
      interval.type='quantile',        
      na.rm=T) 
cbind(Q3, lower = Q3[,3] - qt(p=0.975, df=23)*Q3[,4], upper = Q3[,3] + qt(p=0.975, df=23)*Q3[,4])
            

#------------------------------------------------------------------------------------------------------------------
#   (e) Find the ratio of mean HDL cholesterol to the mean of total cholesterol (TOT_CHOL), overall, 
#   and for subgroups formed by overweight status crossed with gender.  
#   Which of the four subgroups has the highest ratio?  
#   Account for the fact that you are making multiple comparisons. 
#   (As in part c, the SEX_OVER variable will be useful here.)
#------------------------------------------------------------------------------------------------------------------

            # Ratio of means: HDL cholesterol to Total cholesterol
            # overall
hdl.ratio <- svyratio(~HDL_CHOL, ~TOT_CHOL,  
         design = rdesign.over, 
         na.rm = TRUE)
hdl.ratio
confint(hdl.ratio)

hdl.sub <- svyby(~HDL_CHOL,by = ~interaction(OVERWGHT,SEX),
      denominator = ~TOT_CHOL,
      FUN = svyratio,
      na.rm = TRUE,
      covmat = TRUE,
      design = rdesign.over)
hdl.sub

        # survey will not do t-tests on diffs since cov matrix is not implemented as of 3/13/2010
diff.o1.s1.o2.s1 <- svycontrast(hdl.sub, list(diff=(c(1,-1,0,0))))
diff.o1.s1.o1.s2 <- svycontrast(hdl.sub, list(diff=(c(1,0,-1,0))))
diff.o1.s1.o2.s2 <- svycontrast(hdl.sub, list(diff=(c(1,0,0,-1))))
diff.o2.s1.o1.s2 <- svycontrast(hdl.sub, list(diff=(c(0,1,-1,0))))
diff.o2.s1.o2.s2 <- svycontrast(hdl.sub, list(diff=(c(0,1,0,-1))))
diff.o1.s2.o2.s2 <- svycontrast(hdl.sub, list(diff=(c(0,0,-1,1))))

difs <- rbind(c(diff.o1.s1.o2.s1, diff.o1.s1.o2.s1/SE(diff.o1.s1.o2.s1)),
      c(diff.o1.s1.o1.s2, diff.o1.s1.o1.s2/SE(diff.o1.s1.o1.s2)),
      c(diff.o1.s1.o2.s2, diff.o1.s1.o2.s2/SE(diff.o1.s1.o2.s2)),
      c(diff.o2.s1.o1.s2, diff.o2.s1.o1.s2/SE(diff.o2.s1.o1.s2)),
      c(diff.o2.s1.o2.s2, diff.o2.s1.o2.s2/SE(diff.o2.s1.o2.s2)),  
      c(diff.o1.s2.o2.s2, diff.o1.s2.o2.s2/SE(diff.o1.s2.o2.s2))  
)
dimnames(difs)[[1]] <- c("d.1121", "d.1112", "d.1122", "d.2112", "d.2122", "d.1222")
dimnames(difs)[[2]] <- c("difference", "t-stat")
difs
