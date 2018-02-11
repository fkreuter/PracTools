#*********************************************************************************************************
# FILE: C:\Projects\Practical Tools Book\Book Chapters\3 Sample Design\Examples\Example 3.13.R            
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                                     
# DATE:    12/25/2011                                                                                     
# AUTHOR:  R. Valliant                                                                                    
# PURPOSE: Illustrate formation of strata using cume measure of size
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\hospital.RData",pos=2)
x <- hospital$x
g <- 1.75
H <- 10
nh <- 2
hosp.pop <- hospital[order(x), ]

xg <- sqrt(x^g)
N <- nrow(hosp.pop)

    # create H strata using cume sqrt(x^g) rule
cumxg <- cumsum(xg)
size <- cumxg[N]/H
brks <- (0:H)*size
strata <- cut(cumxg, breaks = brks, labels = 1:H)
Nh <- table(strata)

str.selprobs <- rep(nh,H) / Nh

        # selection probabilities for pp(sqrt(x^g))
pps.selprobs <- H*nh*xg / sum(xg)
round(cbind(Nh = Nh, stsrs = str.selprobs, pps.means = by(pps.selprobs,strata,mean)),4)
