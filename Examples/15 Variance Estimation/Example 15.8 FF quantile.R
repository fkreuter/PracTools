#**********************************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\Examples\Example 16.5 FF quantile.R  
# TOPIC:    Select a pp(sqrt(beds)) sample from smho.N874 and compute quatiles                                         
# DATE:     05/18/2011                                                                                                 
# AUTHOR:   R. Valliant                                                                                                
#**********************************************************************************************************************

require(survey)
require(sampling)

attach("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.RData")
dim(smho.N874)
       
       # partial care, outpatient are hosp.type = 4
       # outpatient all have beds = 0, which goofs up the calibration modeling
       #   so, drop hosp.type = 4
delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]
dim(smho)

       # Select a pps to sqrt(BEDS) sample

x <- smho[,"BEDS"]
       # recode small hospitals to have a minimum MOS
x[x <= 5] <- 5
x <- sqrt(x)

n <- 80
set.seed(428274453)

pk <- n*x/sum(x)
sam <- UPrandomsystematic(pk)
sam <- sam==1

sam.dat <- smho[sam, ]
d <- 1/pk[sam]


        # Create a design object 
smho.dsgn <- svydesign(ids = ~0,         
          strata = NULL,                 
          data = data.frame(sam.dat), 
          weights = ~d)

        # population quartiles
popq <- quantile(smho$SEENCNT, c(0.25, 0.50, 0.75))
popq

        # Compute quartiles and CIs
        # Francisco-Fuller method
FF <- svyquantile(~SEENCNT, design=smho.dsgn, 
    quantiles = c(0.25, 0.50, 0.75),
    ci=TRUE, interval.type="score",
    se = TRUE
)

        # Woodruff method
wood <- svyquantile(~SEENCNT, design=smho.dsgn, 
    quantiles = c(0.25, 0.50, 0.75),
    ci=TRUE, interval.type="Wald",
    se = TRUE
)

round(cbind(t(FF$quantiles), t(FF$CIs[,,1])), 0)
round(cbind(t(wood$quantiles), t(wood$CIs[,,1])), 0)

        # extract SEs
round(SE(FF),1)
round(SE(wood),1)
