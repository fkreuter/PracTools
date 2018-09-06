
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

						# Linear calibration with no bounds--works OK              
sam.lin <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(-Inf,Inf), 
	  calfun=c("linear"),
       	  )
						# Check controls
c(svytotal(~BEDS, sam.lin), pop.beds = x.beds)
c(svytotal(~SEENCNT, sam.lin), pop.seen = x.seen)
c(svytotal(~EOYCNT, sam.lin), pop.eoy = x.eoy)
cbind(svytotal(~ as.factor(hosp.type), sam.lin), x.Nhosp)

						# Check range of adjustments which is large:
						# [1] 0.04642683 2.29984987
range(weights(sam.lin)/d)

						# Linear calibration with bounds--works but controls
						# not hit too closely
sam.linBD <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(0.5, 3), 
	  calfun=c("linear"),
	  maxit = 50, epsilon = 0.085
       	  )
						# Check controls
c(svytotal(~BEDS, sam.linBD), pop.beds = x.beds)
c(svytotal(~SEENCNT, sam.linBD), pop.seen = x.seen)
c(svytotal(~EOYCNT, sam.linBD), pop.eoy = x.eoy)
cbind(svytotal(~ as.factor(hosp.type), sam.linBD), x.Nhosp)

						# Linear calibration with loose bounds and
						# default epsilon--doesn't converge.
sam.linBD2 <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(-1000, 1000), 
	  calfun=c("linear"),
	  maxit = 1000, epsilon = 1e-7
       	  )

sam.rake <- calibrate(design = smho.dsgn, 
	  formula = ~ BEDS + SEENCNT + EOYCNT + as.factor(hosp.type), 
	  population = pop.tots, 
	  bounds=c(0.5, 3), 
	  calfun=c("linear"),
	  maxit = 50, epsilon = 0.085
       	  )
 						# Check controls
 c(svytotal(~BEDS, sam.rake), pop.beds = x.beds)
 c(svytotal(~SEENCNT, sam.rake), pop.seen = x.seen)
 c(svytotal(~EOYCNT, sam.rake), pop.eoy = x.eoy)
 cbind(svytotal(~ as.factor(hosp.type), sam.rake), x.Nhosp)
 
