#********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Calibration\Examples\greg.smho98.cat.R     
# TOPIC:    Compute GREG weights with controls for counts of hospitals                                   
# DATE:     08/03/2010                                                                                   
# AUTHOR:   R. Valliant                                                                                  
#********************************************************************************************************

#    Import smho98 file into R

require(survey)
require(sampling)

smho98.sub <- read.csv("C:\\Projects\\Practical Tools Book\\Data\\smho98.sub.csv", row.names = 1)
dim(smho98.sub)
       
            # Some summary statistics on the population
table(smho98.sub[, "FINDIRCT"], exclude = NULL)
table(smho98.sub[, "hosp.type"], exclude = NULL)
summary(smho98.sub[, "EXPTOTAL"])
summary(smho98.sub[, "BEDS"])

by(smho98.sub[,"BEDS"], smho98.sub[,"hosp.type"], mean)

            # partial care, outpatient are hosp.type = 4
            # outpatient all have beds = 0, which goofs up the calibration modeling
            #   so, drop hosp.type = 4

delete <- smho98.sub$hosp.type == 4
smho <- smho98.sub[!delete, ]
dim(smho)

##################################################################################
### Select a pps to sqrt(BEDS) sample 
##################################################################################

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

by(sam.dat[,"EXPTOTAL"], sam.dat[,"hosp.type"], mean)
by(sam.dat[,"FINDIRCT"], sam.dat[,"hosp.type"], mean)-1

##################################################################################
### Create a design object to use for calibration (LS distance fcn)
##################################################################################

smho.dsgn <- svydesign(ids = ~0,          # no clusters 
          strata = NULL,                  # no strata
          data = data.frame(sam.dat), 
          weights = ~d)

        # GREG with separate slope on beds for each hosp type
        # Compute pop totals of auxiliaries
        # Note these are the original not the recoded x’s
x.beds <- by(smho$BEDS, smho$hosp.type, sum)
x.seen <- sum(smho$SEENCNT)
x.eoy <- sum(smho$EOYCNT)
N.hosp <- table(smho$hosp.type)
N <- nrow(smho)

pop.tots <- c(`(Intercept)` = N,
              SEENCNT = x.seen, 
              EOYCNT = x.eoy,
              x.beds = x.beds
              )
pop.tots
              
sam.lin1 <- calibrate(design = smho.dsgn, 
      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS, 
      population = pop.tots, 
      bounds=c(-Inf,Inf), 
      calfun=c("linear"),
          )
        # Check that weights are calibrated for x's
svyby(~BEDS, by=~as.factor(hosp.type), design=sam.lin1, FUN=svytotal)
svytotal(~SEENCNT, sam.lin1)
svytotal(~EOYCNT, sam.lin1)
svytotal(~ as.factor(hosp.type), sam.lin1)

        # Estimate expenditure total
svytotal(~ EXPTOTAL, sam.lin1)
cv(svytotal(~ EXPTOTAL, sam.lin1))
svymean(~ as.factor(FINDIRCT), sam.lin1)
cv(svymean(~ as.factor(FINDIRCT), sam.lin1))

#_________________________________________________________________________________
# GREG with dummies for hosp type
#_________________________________________________________________________________

x.beds <- sum(smho$BEDS)
pop.tots <- c(BEDS = x.beds,
              SEENCNT = x.seen, 
              EOYCNT = x.eoy,
              HOSP = N.hosp
              )
pop.tots
              
sam.lin2 <- calibrate(design = smho.dsgn, 
      formula = ~ 0 + BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
      population = pop.tots, 
      bounds=c(-Inf,Inf), 
      calfun=c("linear"),
          )
        # Check that weights are calibrated for x's
svytotal(~BEDS, sam.lin2)
svytotal(~SEENCNT, sam.lin2)
svytotal(~EOYCNT, sam.lin2)
svytotal(~ as.factor(hosp.type), sam.lin2)

        # Estimate expenditure total, fin direct proportions
svytotal(~ EXPTOTAL, sam.lin2)
cv(svytotal(~ EXPTOTAL, sam.lin2))
cv(svymean(~ EXPTOTAL, sam.lin2))
svymean(~ as.factor(FINDIRCT), sam.lin2)
cv(svymean(~ as.factor(FINDIRCT), sam.lin2))


##################################################################################
### Check ranges of weight adjustments
##################################################################################
range(weights(smho.dsgn))
range(weights(sam.lin1))
range(weights(sam.lin2))
