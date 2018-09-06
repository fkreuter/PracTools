##################################################################################
### Use JPSM calibrate function to create weights
###  based on smho98 data.
###
### File calibrate.jpsm.R
### Pgmr: R. Valliant, J. Dever
### Date calibrate.jpsm written: 10/27/05
### Date of this program: 10/27/07
##################################################################################

options(editor="c:\\program files\\pfe\\pfe32.exe")

						# source in our calibrate function
source("C:\\JPSM\\SURV 699-Wting case studies\\Lecture Notes\\6 Steps in Weighting\\Examples\\calibrate.jpsm.dmp")
 
						# Compute pop totals to use in calibrate
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
              
						# create dummy vars for hosp.type
hosp.fac <- as.factor(sam.dat[, "hosp.type"])
hosp.dum <- model.matrix(~ hosp.fac - 1)
    						# sample X data
    						# sam.dat created in calibrate.R
X.sam = cbind(rep(1, nrow(sam.dat)),
	  sam.dat[, c("BEDS", "SEENCNT", "EOYCNT")],
	  hosp.dum[, 2:5])

# x.pi.est <- apply(X.sam * d, 2, sum)
x.pi.est <- d %*% as.matrix(X.sam)

cal.lin <- calibrate.jpsm(
		X = as.matrix(X.sam),
		X.pop = pop.tots,
		X.hat = x.pi.est,
		a = d, 				# computed in calibrate.R
		v = rep(1,nrow(X.sam)),
		L = -Inf, 
		U = Inf, 
		conv.crit = 0.01, 
		max.steps = 100.,
		min.cell = 2,
		warn.cell = 2)

cal.lin$f.wts %*% as.matrix(X.sam) 

cal.linBD <- calibrate.jpsm(
		X = as.matrix(X.sam),
		X.pop = pop.tots,
		X.hat = x.pi.est,
		a = d, 				# computed in calibrate.R
		v = rep(1,nrow(X.sam)),
		L = 0.5, 
		U = 3, 
		conv.crit = 0.001, 
		max.steps = 1000.,
		min.cell = 2,
		warn.cell = 2)

						# check wt calibration		
cal.linBD$f.wts %*% as.matrix(X.sam) 
plot(cal.lin$f.wts, cal.linBD$f.wts)

cal.linBD$f.wts %*% sam.dat


############################################################################################
### Hand check on wts with unbounded linear distance fcn
############################################################################################

A <- t(as.matrix(X.sam)) %*% diag(d) %*% as.matrix(X.sam)
one <- rep(1, nrow(X.sam))
						# using ginv(A) does not give calibrated wts unless
						# tol = .Machine$double.eps is used.
						# tol = sqrt(.Machine$double.eps) is default
g.wt1 <- one + (pop.tots - x.pi.est) %*% ginv(A, tol = .Machine$double.eps) %*% t(X.sam)
g.wt <- one + (pop.tots - x.pi.est) %*% solve(A) %*% t(X.sam)

						# Both chk.wts and chk.wts1 are same and 
						# agree with Lumley's calibrate with bounds = c(-Inf, Inf)
chk.wts1 <- g.wt * d
chk.wts <- g.wt * d
						# These wts are all the same
cbind( weights(sam.lin), cal.lin$f.wts, as.vector(chk.wts))
						# Wts are calibrated
chk.wts1 %*% as.matrix(X.sam) 
chk.wts %*% as.matrix(X.sam) 

##################################################################################
### Plot weight relationships
##################################################################################

par(mfrow = c(1,2))
plot(d, weights(sam.linBD),
     col = "black",
     pch = 16,
     ylab = "weight")
points(d, cal.linBD$f.wts, 
	col = "blue",
	pch = 16)
abline(0,1)

plot(d, weights(sam.linBD)/d,
     col = "black",
     pch = 16,
     ylim = c(0, 3.1),
     ylab = "weight / d"
     )
points(d, cal.linBD$f.wts/d,
	col = "blue",
	pch = 16)

abline(h = 0.5, col = "hotpink")
abline(h = 3, col = "hotpink")

