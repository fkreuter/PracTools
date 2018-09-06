#####################################################################################
# File: 	Example 3.9.R
# Project:  	Practical Tools book        
# Date: 	01/17/2014
# Author:	R. Valliant, J.A. Dever
# Purpose:	Find allocation to minimize var of mean for a total budget of $100,000
#####################################################################################

require(PracTools)

ch <- c(1400, 200, 300, 600, 450, 1000)
Nh <- c(215, 65, 252, 50, 149, 144)
Sh <- c(2678, 1064, 690, 1108, 981, 4455)
alloc <- strAlloc(Nh = Nh, Sh = Sh, cost = 100000, ch = ch,alloc = "totcost")
alloc
round(alloc$nh,1)
round(alloc$'nh/n',2)
