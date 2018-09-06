#################################################################################
#	Power calculations
#################################################################################

#    One sample--determine power for given n
nn <- seq(10, 100, 10)
out <- vector("numeric",  length(nn))
for (i in 1:length(nn)){
    out[i] <- power.t.test(n = nn[i], 
	                   sd = 1, 
	     		   type = "one.sample", 
	     		   alternative = "two.sided",
	     		   sig.level = 0.05,
	     		   delta = 1)$power
}	   

#    One sample--determine n for given power
pp <- seq(0.05, 1, 0.05)
for (i in 1:length(pp)){
browser()
    power.t.test(power = pp[i], 
	     sd = 1, 
	     type = "one.sample", 
	     alternative = "two.sided",
	     sig.level = 0.05,
	     delta = 1)
}	   

#  Check of example in SURV 616 power notes	     
power.t.test(power = 0.8,
	     delta = 5,
	     sd = sqrt(312), 
	     type = "one.sample", 
	     alternative = "one.sided"
	     )	     
#   1-sided Results:
     One-sample t test power calculation 

              n = 78.52896
          delta = 5
             sd = 17.66352
      sig.level = 0.05
          power = 0.8
    alternative = one.sided
	     
power.t.test(power = 0.8,
	     delta = 5,
	     sd = sqrt(312), 
	     type = "one.sample", 
	     alternative = "two.sided"
	     )	 	     
#   2-sided Results:
     One-sample t test power calculation 

              n = 99.89177
          delta = 5
             sd = 17.66352
      sig.level = 0.05
          power = 0.8
    alternative = two.sided
	  
###################################################################################
#	Check Dixon and Massey Table 6-3, p.85
###################################################################################

p.chk <- power.t.test(n = 25, 
	delta = seq(-1.5, 1.5, 0.5), 
	sd = 3, 
	type = "one.sample",
	alt = "two.sided",
	sig.level = 0.05,
	strict = TRUE)
	
p.chk$power	
[1] 0.6697077 0.3596554 0.1259956 0.0500000 0.1259956 0.3596554 0.6697077

# These are close to the Dixon-Massy table, but D-M uses n=25 
# but with +/- 1.96 as critical values, not the t-distribution nos.

###################################################################################
#	Check paired sample size calculation in SURV 616 notes, class 1&2
###################################################################################
power.t.test(power = 0.8,
	delta = 5, 
	sd = sqrt(312), 
	type = "paired",
	alt = "one.sided",
	sig.level = 0.05,
	strict = FALSE)
	
     Paired t test power calculation 

              n = 78.52896
          delta = 5
             sd = 17.66352
      sig.level = 0.05
          power = 0.8
    alternative = one.sided

 NOTE: n is number of *pairs*, sd is std.dev. of *differences* within pairs 
#####	This checks with notes.   #####


	
###################################################################################
#	Draw power curves
###################################################################################

p.chk <- power.t.test(n = 25, 
	delta = seq(-4, 4, 0.1), 
	sd = 3, 
	type = "two.sample",
	alt = "two.sided",
	sig.level = 0.05,
	strict = TRUE)
	
plot(seq(-4, 4, 0.1), p.chk$power, 
	ylab = "Power",

	xlab = "Delta",
	type = "n")	
abline(h = seq(0.1, 1, 0.1), 
	v = seq(-3, 3, 1),
	col = "grey70")
lines(loess(p.chk$power ~ seq(-4, 4, 0.1)))

p.chk <- power.t.test(n = 10, 
	delta = seq(-4, 4, 0.1), 
	sd = 3, 
	type = "two.sample",
	alt = "two.sided",
	sig.level = 0.05,
	strict = TRUE)

lines(loess(p.chk$power ~ seq(-4, 4, 0.1)))

p.chk <- power.t.test(n = 50, 
	delta = seq(-4, 4, 0.1), 
	sd = 3, 
	type = "two.sample",
	alt = "two.sided",
	sig.level = 0.05,
	strict = TRUE)

lines(loess(p.chk$power ~ seq(-4, 4, 0.1)))

p.chk <- power.t.test(n = 100, 
	delta = seq(-4, 4, 0.1), 
	sd = 3, 
	type = "two.sample",
	alt = "two.sided",
	sig.level = 0.05,
	strict = TRUE)

lines(loess(p.chk$power ~ seq(-4, 4, 0.1)))

text(x = 2.5, y = 0.3, "n=10")
text(x = 2.5, y = 0.65, "n=25")
text(x = 2.4, y = 0.92, "n=50")
text(x = 1, y = 0.95, "n=100")