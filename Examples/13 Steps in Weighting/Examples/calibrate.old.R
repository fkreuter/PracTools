##################################################################################
### Use calibrate function in R survey package to create weights
###  based on smho98 data.
###
### File calibrate.R
### Pgmr: R. Valliant
### Date: 10/21/07
##################################################################################

#    Import smho98 file into R

require(foreign)
smho98 <- read.xport("C:\\JPSM\\SURV 699-Wting case studies\\Data\\smho98.xpt")
		
					# Make sampling functions available from book
attach("C:\\JPSM\\SURV 699S-Prediction Theory\\R\\.RData", pos = 2)

cert <- smho98[,"BEDS"] > 2000
smho98 <- smho98[!cert, ]		# Remove the one large hospital from population

					# Recode stratum values to create 5 hospital types
recode <- function(){
    new.str <- rep(0, nrow(smho98))

    for (i in 1:nrow(smho98)){
	if (smho98[i, "STRATUM"] %in% c(1,2)) new.str[i] <- 1		# psychiatric
	if (smho98[i, "STRATUM"] %in% c(3,4,9,10)) new.str[i] <- 2	# residential, veterans
	if (smho98[i, "STRATUM"] %in% c(5:8)) new.str[i] <- 3		# general
	if (smho98[i, "STRATUM"] %in% c(11:13)) new.str[i] <- 4		# partial care, outpatient
	if (smho98[i, "STRATUM"] %in% c(14:16)) new.str[i] <- 5		# multi-svc, substance abuse
    }

    cbind(smho98, hosp.type = new.str)		# convert new.str to a factor 
}

tmp <- recode()
dat.cols <- c("EXPTOTAL",	# total expenditures 	
               "BEDS", 		# inpatient beds		
#               "IPCSFRST", 	# no. clients/patients present on 1st day of reporting yr
#               			#   receiving hospital/residential service in last 90 days
#               "OPCSFRST",	# no. clients/patients on rolls on 1st day of reporting yr
#                           	#   received OP service in last 90 days
               "SEENCNT",	# unduplicated client/patient count for year
               "EOYCNT",	# unduplicated client/patient count on rolls at end of year
               "FINDIRCT",	# receives state health agency funding
               "hosp.type"	# type of hospital, see above
               )
               
#  IPCSFRST and OPCSFRST have too many missing to use

smho98.sub <- tmp[, dat.cols]
rm(tmp)

table(smho98.sub[, "FINDIRCT"], exclude = NULL)
table(smho98.sub[, "hosp.type"], exclude = NULL)
summary(smho98.sub[, "EXPTOTAL"])
summary(smho98.sub[, "BEDS"])
summary(smho98.sub[, "IPCSFRST"])
summary(smho98.sub[, "OPCSFRST"])

by(smho98.sub[,"BEDS"], smho98.sub[,"hosp.type"], mean)

tmp <- glm(EXPTOTAL ~ BEDS + SEENCNT + EOYCNT + 
#               as.factor(FINDIRCT) +			# FINDIRCT is n.s. in full pop
               as.factor(hosp.type),
     data = smho98.sub
)
summary(tmp)

pairs(smho98.sub[, c("EXPTOTAL", 
                     "BEDS",
                     "SEENCNT",	
                     "EOYCNT")]
      )

##################################################################################
### Select a pps to sqrt(BEDS) sample and fit a linear model to EXPTOTAL
##################################################################################

x <- smho98.sub[,"BEDS"]
x[x <= 5] <- 5
x <- sqrt(x)

n <- 80
set.seed(331)

sam <- pps.random.fcn(x, n)			# probability proportional to sqrt(x)
sam.dat <- smho98.sub[sam, ]
d <- sum(x) / (n * x[sam])

table(sam.dat[, "hosp.type"])

> table(sam.dat[, "hosp.type"])

 1  2  3  4  5 
27  7 27  6 13 

tmp <- glm(EXPTOTAL ~ BEDS + SEENCNT + EOYCNT + 
               as.factor(hosp.type),
     data = sam.dat
)
summary(tmp)

pairs(sam.dat[, c("EXPTOTAL", 
                     "BEDS",
                     "SEENCNT",	
                     "EOYCNT")]
      )
by(sam.dat[,"EXPTOTAL"], sam.dat[,"hosp.type"], mean)

##################################################################################
### Create a design object to use for calibration
##################################################################################

require(survey)
smho.dsgn <- svydesign(ids = ~0,			# no clusters 
          strata = NULL, 
          data = data.frame(sam.dat), 
          weights = ~d)

							# Compute pop totals of auxiliaries
x.beds <- sum(smho98.sub[,"BEDS"])
x.seen <- sum(smho98.sub[,"SEENCNT"])
x.eoy <- sum(smho98.sub[,"EOYCNT"])
x.Nhosp <- table(smho98.sub[,"hosp.type"])

pop.tots <- c(`(Intercept)` = 874,
	      BEDS = x.beds, 
              SEENCNT = x.seen, 
              EOYCNT = x.eoy, 
              hosp.type = x.Nhosp[2:5]			# omit 1st level of new.str
              )

sam.calib <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(-Inf,Inf), 
	  calfun=c("linear"),
       	  maxit=50,
       	  epsilon=1e-7
       	  )
       	  
svytotal(~BEDS, sam.calib)
svytotal(~SEENCNT, sam.calib)
svytotal(~EOYCNT, sam.calib)
svytotal(~ as.factor(hosp.type), sam.calib)

svytotal(~ EXPTOTAL, sam.calib)

##################################################################################
### Compare to pi-estimator
##################################################################################

svytotal(~ EXPTOTAL, smho.dsgn)
