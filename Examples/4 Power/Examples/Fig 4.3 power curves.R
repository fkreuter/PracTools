#*********************************************************************************
# FILE:    C:\Projects\Practical Tools\Book\Book Chapters\4 Power\Fig 4.3 power curves.R    
# PURPOSE: Draw power curves
# DATE:    02/19/2012                                                                
# AUTHOR:  R. Valliant                                                            
#*********************************************************************************

###################################################################################
#	Draw power curves
###################################################################################
postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\4 Power\\Examples\\Fig 4.3.eps")

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

dev.off()