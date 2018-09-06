#**************************************************************************
# File:     C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\
#              Examples\Example 14.7.R
# Name:     R. Valliant
# Topic:    Practical Tools for Designing and Weighting Sample Surveys
# Date:     08/19/10
# Purpose:  Use trim parameter in calibrate to compute bounded weights.
# Revised:  02/12/18 JD Loaded data from PracTools package; output file 
#              for Example 14.8 (WTADJUST in SUDAAN)
#**************************************************************************

require(PracTools)
require(sampling)
library(survey)

data(smho.N874)

smho98.sub <- smho.N874
dim(smho98.sub)

            #   drop hosp.type = 4
delete <- smho98.sub$hosp.type == 4
smho <- smho98.sub[!delete, ]
dim(smho)

#_________________________________________________________________________________
### Select a pps to sqrt(BEDS) sample                                             
#_________________________________________________________________________________

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

                    # Check to see if all hosp.type's are in sample
table(sam.dat[, "hosp.type"])

                    # Create design object
smho.dsgn <- svydesign(ids = ~0,          # no clusters 
          strata = NULL,                  # no strata
          data = data.frame(sam.dat), 
          weights = ~d)

                        # Compute pop totals of auxiliaries
                        # Note these are the original not the recoded xs
x.beds <- by(smho$BEDS, smho$hosp.type, sum)
x.seen <- sum(smho$SEENCNT)
x.eoy <- sum(smho$EOYCNT)

N <- nrow(smho)

pop.tots <- c(`(Intercept)` = N,
              SEENCNT = x.seen, 
              EOYCNT = x.eoy,
              x.beds = x.beds
              )
pop.tots
              
sam.lin <- calibrate(design = smho.dsgn, 
      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS, 
      population = pop.tots, 
      bounds=c(-Inf,Inf), 
      calfun=c("linear"),
          )
                            # Check that weights are calibrated for x's
svyby(~BEDS, by=~as.factor(hosp.type), design=sam.lin, FUN=svytotal)
svytotal(~SEENCNT, sam.lin)
svytotal(~EOYCNT, sam.lin)
svytotal(~ as.factor(hosp.type), sam.lin)
summary(weights(sam.lin))
summary(weights(smho.dsgn))

                        # Estimate expenditure total
svytotal(~ EXPTOTAL, sam.lin)
cv(svytotal(~ EXPTOTAL, sam.lin))
cv(svymean(~ EXPTOTAL, sam.lin))

#_________________________________________________________________________________
### Compare to pi-estimator
#_________________________________________________________________________________

svytotal(~ EXPTOTAL, smho.dsgn)
cv(svytotal(~ EXPTOTAL, smho.dsgn))
SE(svytotal(~ EXPTOTAL, smho.dsgn))/SE(svytotal(~ EXPTOTAL, sam.lin))

#_________________________________________________________________________________
### Trim the weights using trimWeights
### Note that the trim option of calibrate does not trim the weights. It defines  
###    "trimmed" bounds. See ?calibrate
#_________________________________________________________________________________

sam.lin.tr <- trimWeights(design = sam.lin, 
            lower = 2,
            upper = 18,
            strict = TRUE)
summary(weights(sam.lin.tr))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.002   5.957   9.043   9.062  11.140  18.000 

        # Check whether weights are calibrated for x's
        # They are not (as expected)
svyby(~BEDS, by=~as.factor(hosp.type), design=sam.lin.tr, FUN=svytotal)
svytotal(~SEENCNT, sam.lin.tr)
svytotal(~EOYCNT, sam.lin.tr)
svytotal(~ as.factor(hosp.type), sam.lin.tr)

        # Estimate expenditure total
svytotal(~ EXPTOTAL, sam.lin.tr)
cv(svytotal(~ EXPTOTAL, sam.lin.tr))
cv(svymean(~ EXPTOTAL, sam.lin.tr))

postscript("C:\\Fig 14.7.eps", width=8, height=8)

par(mgp = c(2,0.5,0))
plot(weights(smho.dsgn), weights(sam.lin.tr),
     pch = 21,
     bg = "gray80",
     xlab = "Design weights & Calibrated weights",
     ylab = "Trimmed weights",
     xlim = range(weights(smho.dsgn), weights(sam.lin), weights(sam.lin.tr)),
     ylim = range(weights(sam.lin.tr))
)
abline(0,1)
abline(h = c(2,18), lty = 2)

points(weights(sam.lin), weights(sam.lin.tr),
        pch = 16)

legend(x = 24, y = 6,
        legend = c("Design wts", "Calibrated wts"),
        pt.bg = c("gray80", "black"),
        pch=21,
        cex = 1.2
)
dev.off()

#_________________________________________________________________________________
### Create export file for Example 14.8 
#_________________________________________________________________________________

require(SASxport)
smho_80 <- cbind(sam.dat, dwt=d)
write.xport(smho_80, file="C:\\Ex14_7.xpt")

#**************************************************************************
