##################################################################################
### Use calibrate function in R survey package to create weights
###  based on smho98 data.
###
### File calibrate.R
### Pgmr: R. Valliant
### Date: 10/22/07
### Revised: 09/28/09  Added code for histograms of weights
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
               "SEENCNT",	# unduplicated client/patient count for year
               "EOYCNT",	# unduplicated client/patient count on rolls at end of year
               "hosp.type"	# type of hospital, see above
               )

smho98.sub <- tmp[, dat.cols]
rm(tmp)
				# Some summary statistics on the population
table(smho98.sub[, "FINDIRCT"], exclude = NULL)
table(smho98.sub[, "hosp.type"], exclude = NULL)
summary(smho98.sub[, "EXPTOTAL"])
summary(smho98.sub[, "BEDS"])

by(smho98.sub[,"BEDS"], smho98.sub[,"hosp.type"], mean)

tmp <- glm(EXPTOTAL ~ BEDS + SEENCNT + EOYCNT + 
	               as.factor(hosp.type),
     data = smho98.sub
)
summary(tmp)

pairs(smho98.sub[, c("EXPTOTAL", 
                     "BEDS",
                     "SEENCNT",	
                     "EOYCNT")]
      )
rm(tmp)

##################################################################################
### Select a pps to sqrt(BEDS) sample and fit a linear model to EXPTOTAL
##################################################################################

x <- smho98.sub[,"BEDS"]
					# recode small hospitals to have a minimum MOS
x[x <= 5] <- 5
x <- sqrt(x)

n <- 80
set.seed(331)

sam <- pps.random.fcn(x, n)		# probability proportional to sqrt(x)
sam.dat <- smho98.sub[sam, ]
d <- sum(x) / (n * x[sam])

					# Check to see if all hosp.type's are in sample
table(sam.dat[, "hosp.type"])

> table(sam.dat[, "hosp.type"])

 1  2  3  4  5 
27  7 27  6 13 

tmp <- glm(EXPTOTAL ~ BEDS + SEENCNT + EOYCNT + 
               as.factor(hosp.type),
     data = sam.dat
)
summary(tmp)
rm(tmp)

pairs(sam.dat[, c("EXPTOTAL", 
                     "BEDS",
                     "SEENCNT",	
                     "EOYCNT")]
      )
by(sam.dat[,"EXPTOTAL"], sam.dat[,"hosp.type"], mean)

##################################################################################
### Create a design object to use for calibration (LS distance fcn)
##################################################################################

require(survey)
smho.dsgn <- svydesign(ids = ~0,		# no clusters 
          strata = NULL, 			# no strata
          data = data.frame(sam.dat), 
          weights = ~d)

						# Compute pop totals of auxiliaries
						# Note these are the original not the recoded x’s
x.beds <- sum(smho98.sub[,"BEDS"])
x.seen <- sum(smho98.sub[,"SEENCNT"])
x.eoy <- sum(smho98.sub[,"EOYCNT"])
x.Nhosp <- table(smho98.sub[,"hosp.type"])

pop.tots <- c(`(Intercept)` = 874,
	      BEDS = x.beds, 
              SEENCNT = x.seen, 
              EOYCNT = x.eoy, 
              hosp.type = x.Nhosp[2:5]		# omit 1st level of hosp.type since
              					# calibrate drops 1st level of any categorical var
              )
              
sam.lin <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(-Inf,Inf), 
	  calfun=c("linear"),
       	  )
       	  					# Check that weights are calibrated for x's
svytotal(~BEDS, sam.lin)
svytotal(~SEENCNT, sam.lin)
svytotal(~EOYCNT, sam.lin)
svytotal(~ as.factor(hosp.type), sam.lin)
						# Estimate expenditure total
svytotal(~ EXPTOTAL, sam.lin)

##################################################################################
### Compare to pi-estimator
##################################################################################

svytotal(~ EXPTOTAL, smho.dsgn)

##################################################################################
### Histograms of pi-wts and GREG wts
##################################################################################
require(MASS)
par(mfrow=c(1,2),mar=c(3,3,1,1))
truehist(d, xlab="")
title("Pi weights")
truehist(weights(sam.lin), xlab="")
title("GREG weights")

##################################################################################
### Compare to raking & bounded linear
##################################################################################

sam.rake <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(0.5, 3), 
	  calfun=c("raking"),
	  maxit=100, epsilon = 0.001
       	  )
 						# Check controls
 c(svytotal(~BEDS, sam.rake), pop.beds = x.beds)
 c(svytotal(~SEENCNT, sam.rake), pop.seen = x.seen)
 c(svytotal(~EOYCNT, sam.rake), pop.eoy = x.eoy)
 cbind(svytotal(~ as.factor(hosp.type), sam.rake), x.Nhosp)
			# Estimate expenditure total
c(svytotal(~ EXPTOTAL, sam.rake), pop.exp = sum(smho98.sub[,"EXPTOTAL"]) )

		# Linear calibration with bounds
sam.linBD <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(0.5, 3), 
	  calfun=c("linear"),
	  maxit = 50, epsilon = 0.001
       	  )
						# Check controls
c(svytotal(~BEDS, sam.linBD), pop.beds = x.beds)
c(svytotal(~SEENCNT, sam.linBD), pop.seen = x.seen)
c(svytotal(~EOYCNT, sam.linBD), pop.eoy = x.eoy)
cbind(svytotal(~ as.factor(hosp.type), sam.linBD), x.Nhosp)

			# Estimate expenditure total
c(svytotal(~ EXPTOTAL, sam.linBD),  pop.exp = sum(smho98.sub[,"EXPTOTAL"]) )

##################################################################################
### Plot weight relationships
##################################################################################

par(mfrow = c(1,2))
plot(d, weights(sam.lin),
     col = "black",
     pch = 16,
     ylab = "weight")
points(d, weights(sam.rake), 
	col = "red",
	pch = 2)
points(d, weights(sam.linBD), 
	col = "blue",
	pch = 16)
abline(0,1)

plot(d, weights(sam.lin)/d,
     col = "black",
     pch = 16,
     ylim = c(0, 3.1),
     ylab = "weight / d"
     )
points(d, weights(sam.rake)/d,
	col = "red",
	pch = 2)
points(d, weights(sam.linBD)/d,
	col = "blue",
	pch = 16)
abline(h = 0.5, col = "hotpink")
abline(h = 3, col = "hotpink")

arrows(x0 = 5, y0 = 0.05,
       x1 = 7.5, y1 = 0.22,
       length = 0.125,
       lwd = 4,
       col = "cadetblue2")
       
arrows(x0 = 33, y0 = 0.25,
       x1 = 35, y1 = 0.06,
       length = 0.125,
       lwd = 4,
       col = "cadetblue2")

##################################################################################
### Check ranges of weight adjustments
##################################################################################
> range(weights(sam.lin)/weights(smho.dsgn))
[1] 0.04642683 2.29984987
> range(weights(sam.rake)/weights(smho.dsgn))
[1] 0.5 3.0
> range(weights(sam.linBD)/weights(smho.dsgn))
[1] 0.5 3.0


##################################################################################
### Write smho98.sub to a csv file
##################################################################################

# dump and source work, but tmp below is a list with 2 components.
# The 1st component contains smho98.sub.dmp

dump("smho98.sub", "C:\\JPSM\\SURV 699-Wting case studies\\Data\\smho98.sub.dmp")
tmp <- source("C:\\JPSM\\SURV 699-Wting case studies\\Data\\smho98.sub.dmp")

write.csv(smho98.sub, "C:\\JPSM\\SURV 699-Wting case studies\\Data\\smho98.sub.csv")
tmp <- read.csv("C:\\JPSM\\SURV 699-Wting case studies\\Data\\smho98.sub.csv", 
         row.names = 1)

					# Write a file to send to Lumley         
save(smho98.sub, d, sam.dat, file = "C:\\JPSM\\SURV 699-Wting case studies\\Data\\test.RData")
