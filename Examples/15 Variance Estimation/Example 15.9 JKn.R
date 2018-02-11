#******************************************************************************
# FILE C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\  
#               Examples\Example 16.8 JKn.R                                    
# PROJECT: Practical Tools for Designing and Weighting Survey Samples          
# DATE: 04/23/2011                                                             
# PGMR: R. Valliant                                                            
# PURPOSE: Example 16.8 using JKn variance estimate                            
#******************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.large.RData", pos=2)
require(survey)

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
        # JKn 
jkn.dsgn <- as.svrepdesign(design = nhis.dsgn, type = "JKn")

        # 1-way table
a <- svymean(~factor(age.grp), deff=TRUE, design=jkn.dsgn)
b <- ftable(a, rownames = list(age = c("< 18", "18-24", "25-44", "45-64", "65+")))
round(b,4)           

     
        # 2-way table
a <- svymean(~interaction(as.factor(delay.med),as.factor(race)), 
                design = jkn.dsgn, 
                na.rm = TRUE,
                deff = TRUE)
b <- ftable(a, rownames = list(Delay = c("Yes","No"),
            Race=c("White", "Black", "Other")))
round(b,6)

a <- svymean(~interaction(as.factor(delay.med),as.factor(race)), 
                design = jkn.dsgn, 
                na.rm = TRUE,
                deff = TRUE)
b <- ftable(a, rownames = list(Delay = c("Yes","No"),
            Race=c("White", "Black", "Other")))
round(b,6)
