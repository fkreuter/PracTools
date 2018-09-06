#****************************************************************************************************
# FILE C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\Examples\Example 16.8.R 
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                                
# DATE: 05/12/2011                                                                                   
# PGMR: R. Valliant                                                                                  
# PURPOSE: Example 16.8 using BRR and Fay-BRR             
#****************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.large.RData", pos=2)
require(survey)

nhis.dsgn <- svydesign(ids = ~psu,
                         strata = ~stratum,
                         nest = TRUE,           # clusters are renumbered within PSUs
                         data = nhis.large,
                         weights = ~svywt)
brr.dsgn <- as.svrepdesign(design = nhis.dsgn, type = "BRR")

        # 1-way table
a <- svymean(~factor(age.grp), deff=TRUE, design=brr.dsgn)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+")))
round(b,4)           

faybrr.dsgn <- as.svrepdesign(design = nhis.dsgn, type = "Fay", fay.rho = 0.3)

        # 1-way table
a <- svymean(~factor(age.grp), deff=TRUE, design=faybrr.dsgn)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+")))
round(b,4)           
