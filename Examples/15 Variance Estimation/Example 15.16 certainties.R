#*****************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\16 Variance Estimation\        
#                   Examples\Example 16.15 certainties.R                                  
# PROJECT: Practical Tools for Designing and Weighting Survey Samples                     
# DATE:     05/30/2011                                                                    
# AUTHOR:   R. Valliant                                                                   
# PURPOSE: Example 16.15 to illustrate handling of certainties                            
#*****************************************************************************************

require(survey)
require(sampling)

attach("C:\\Projects\\Practical Tools Book\\Data\\smho98.RData", pos=2)
pop <- smho98

        # recoded BEDS as MOS
set.seed(428274453)
n <- 80
N <- nrow(pop)
x <- pop$BEDS
x[x<10] <- 10
pik <- n*x/sum(x)

        # check for certainties & adjust selection probs of non-certainties
n.cert <- sum(pik >= 0.8)
n.cert
# [1] 9

certs <- (1:N)[pik >= 0.8]
x.nc <- x[-certs]

n.nc <- n - n.cert
pik <- n.nc*x.nc/sum(x.nc)
sum(pik >= 1)
# [1] 0

sam <- UPrandomsystematic(pik)

pop.nc <- pop[-certs,]             
        # extract rows for non-certainties, then append rows for certainties
sam.dat <- pop.nc[sam==1,]
sam.dat <- rbind(sam.dat, pop[certs,])
        # append strata codes and fpc's
        # stratum = 1 for non-certs, 2 for certs
        # fpc = 0 for non-certs, 1 for certs
stratum <- c(rep(1,n.nc), rep(2,n.cert))
fpc <- c(rep(0,n.nc), rep(1,n.cert))
sam.dat <- cbind(sam.dat, stratum, fpc)

probs <- c(pik[sam==1], rep(1,n.cert))
d <- 1/probs

        # Create a design object with fpc's
smho.dsgn <- svydesign(ids = ~0,          # no clusters 
                       strata = ~stratum,
                       fpc = ~fpc,
                       data = data.frame(sam.dat), 
                       weights = ~d)

svytotal(~EXPTOTAL, design=smho.dsgn)
cv(svytotal(~EXPTOTAL, design=smho.dsgn))
svytotal(~SEENCNT, design=smho.dsgn)
cv(svytotal(~SEENCNT, design=smho.dsgn))

        # Create a design object without fpc's
smho.nofpc.dsgn <- svydesign(ids = ~0,         
                       strata = NULL,
                       data = data.frame(sam.dat), 
                       weights = ~d)

svytotal(~EXPTOTAL, design=smho.nofpc.dsgn)
cv(svytotal(~EXPTOTAL, design=smho.nofpc.dsgn))
svytotal(~SEENCNT, design=smho.nofpc.dsgn)
cv(svytotal(~SEENCNT, design=smho.nofpc.dsgn))

        # repeat using JKn to see effect of fpc
strat.rep <- c(rep(1,n.nc), 2:(2 + (n.cert-1)))
options(survey.lonely.psu="certainty")

rep.dsgn <- svydesign(ids = ~0,     
                       strata = ~strat.rep,
                       data = data.frame(sam.dat), 
                       weights = ~d)

jkn.dsgn <- as.svrepdesign(design = rep.dsgn, type = "JKn")
svytotal(~EXPTOTAL, design=jkn.dsgn)
cv(svytotal(~EXPTOTAL, design=jkn.dsgn))
svytotal(~SEENCNT, design=jkn.dsgn)
cv(svytotal(~SEENCNT, design=jkn.dsgn))
