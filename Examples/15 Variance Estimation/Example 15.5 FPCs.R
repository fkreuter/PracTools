#*****************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\        
#                   Examples\Example 16.7 FPCs.R                                          
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                     
# DATE:     07/08/2010                                                                    
# AUTHOR:   R. Valliant                                                                   
# PURPOSE: Example 16.7 to illustrate the effect of stratum fpc's                         
#*****************************************************************************************

require(survey)
require(sampling)

attach("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.RData", pos=2)
       
        # Population stratum counts
Nh <- table(smho.N874[, "hosp.type"])

        # Select a stratified simple random sample within hospital type strata
set.seed(428274453)
n <- 50
H <- length(Nh)
sam <- strata(data = smho.N874, stratanames = "hosp.type", 
              size = rep(n,H), method=c("srswor"), description = TRUE)
              
sam.dat <- smho.N874[sam$ID_unit,]
d <- 1/sam$Prob
sam.rates <- sam$Prob

        # Create a design object with fpc's
smho.dsgn <- svydesign(ids = ~0,          # no clusters 
                       strata = ~hosp.type,
                       fpc = ~sam.rates,
                       data = data.frame(sam.dat), 
                       weights = ~d)

cv(svyby(~EXPTOTAL, by=~as.factor(hosp.type), design=smho.dsgn, FUN=svytotal))
cv(svytotal(~EXPTOTAL, design=smho.dsgn))

        # Create a design object without fpc's
smho.nofpc.dsgn <- svydesign(ids = ~0,         
                       strata = ~hosp.type,
                       data = data.frame(sam.dat), 
                       weights = ~d)

cv(svyby(~EXPTOTAL, by=~as.factor(hosp.type), design=smho.nofpc.dsgn, FUN=svytotal))
cv(svytotal(~EXPTOTAL, design=smho.nofpc.dsgn))
