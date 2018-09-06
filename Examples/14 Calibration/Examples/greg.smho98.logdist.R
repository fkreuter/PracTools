#********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Calibration\Examples\greg.smho98.logdist.R
# TOPIC:    Fit model for expenditures to pop, select sample, and illustrate GREG
#           Use log distance fcn to eliminate negative weights
# DATE:     10/02/2017
# AUTHOR:   R. Valliant
#********************************************************************************************************

require(survey)
require(sampling)
require(PracTools)

data(smho.N874)
dim(smho.N874)

delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]
dim(smho)

#--------------------------------------------------------------------------------------------------
#   Select a pps to sqrt(BEDS) sample and fit a linear model to EXPTOTAL
#--------------------------------------------------------------------------------------------------

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

    # Countd by hosp.type
table(sam.dat[, "hosp.type"])
#    1  2  3  5
#   33 15 17 15

#--------------------------------------------------------------------------------------------------
#   Create a design object to use for calibration (LS distance fcn)
#--------------------------------------------------------------------------------------------------

smho.dsgn <- svydesign(ids = ~0,          # no clusters
          strata = NULL,                  # no strata
          data = data.frame(sam.dat),
          weights = ~d)

     # Compute pop totals of auxiliaries
     # Note these are the original not the recoded x’s
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
#(Intercept)     SEENCNT      EOYCNT    x.beds.1    x.beds.2    x.beds.3    x.beds.5
#        725     1349241      505345       37978       13066        9573       10077

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
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-0.3983  5.7470  8.8320  9.0620 10.9300 33.8300

summary(weights(smho.dsgn))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  2.714   5.693   8.150   8.763  10.090  33.680

    # raking distance fcn with no bds
sam.rake <- calibrate(design = smho.dsgn,
      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
      population = pop.tots,
      calfun=c("raking"),
      maxit = 100,
      epsilon = 1e-06
          )
summary(weights(sam.rake))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  1.038   5.973   8.824   9.062  10.916  33.922

    # logit distance fcn distance fcn with no bds
sam.logit <- calibrate(design = smho.dsgn,
      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
      population = pop.tots,
      calfun=c("logit"),
      bounds = c(0.4, 3),
      maxit = 100,
      epsilon = 1e-06
          )
summary(weights(sam.logit))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  1.409   5.943   8.912   9.062  10.941  33.746


    # Estimate expenditure total
svytotal(~ EXPTOTAL, sam.logit)
#              total        SE
#EXPTOTAL 9491266584 728656171
cv(svytotal(~ EXPTOTAL, sam.logit))
#           EXPTOTAL
#EXPTOTAL 0.07677123

svytotal(~ EXPTOTAL, sam.lin)
#              total        SE
#EXPTOTAL 9563682688 748596001
cv(svytotal(~ EXPTOTAL, sam.lin))
#           EXPTOTAL
#EXPTOTAL 0.07827487

svytotal(~ EXPTOTAL, sam.rake)
#              total        SE
#EXPTOTAL 9534868546 733727625
cv(svytotal(~ EXPTOTAL, sam.rake))
#           EXPTOTAL
#EXPTOTAL 0.07695204

svytotal(~ EXPTOTAL, sam.logit)
#              total        SE
#EXPTOTAL 9491266584 728656171
cv(svytotal(~ EXPTOTAL, sam.logit))
#           EXPTOTAL
#EXPTOTAL 0.07677123
