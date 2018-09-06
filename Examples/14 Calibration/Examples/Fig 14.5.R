#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Fig 14.5.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Plots of weights for the different methods of calibration in a pps sample
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.RData")
require(sampling)
require(survey)

delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]

#*********************************************************************************************************
# Select a pps to sqrt(BEDS) sample and fit a linear model to EXPTOTAL
#*********************************************************************************************************

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

# Check to see if all hosp.type's are in sample
table(sam.dat[, "hosp.type"])
#    1  2  3  5
#   33 15 17 15


tmp <- glm(EXPTOTAL ~ SEENCNT + EOYCNT +
    #                    as.factor(FINDIRCT) +
    as.factor(hosp.type):BEDS,
           data = sam.dat
           )
summary(tmp)
rm(tmp)

#*********************************************************************************************************
# Create a design object to use for calibration (LS distance fcn)
#*********************************************************************************************************

smho.dsgn <- svydesign(ids = ~0,          # no clusters
                       strata = NULL,                  # no strata
                       data = data.frame(sam.dat),
                       weights = ~d)

# Compute pop totals of auxiliaries
# Note these are the original not the recoded x???s
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
summary(weights(smho.dsgn))


# Estimate expenditure total
svytotal(~ EXPTOTAL, sam.lin)
cv(svytotal(~ EXPTOTAL, sam.lin))

#*********************************************************************************************************
# Compare to pi-estimator
#*********************************************************************************************************

svytotal(~ EXPTOTAL, smho.dsgn)
cv(svytotal(~ EXPTOTAL, smho.dsgn))
SE(svytotal(~ EXPTOTAL, smho.dsgn))/SE(svytotal(~ EXPTOTAL, sam.lin))


#*********************************************************************************************************
# Compare to raking & bounded linear
#*********************************************************************************************************

# Linear calibration with bounds
sam.linBD <- calibrate(design = smho.dsgn,
                       formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
                       population = pop.tots,
                       bounds = c(0.4, 3),
                       calfun = "linear",
                       maxit = 50, epsilon = 1e-7
                       )
# Check controls
svyby(~BEDS, by=~as.factor(hosp.type), design=sam.linBD, FUN=svytotal)
svytotal(~BEDS, sam.linBD)
svytotal(~SEENCNT, sam.linBD)
svytotal(~EOYCNT, sam.linBD)

# raking
sam.rake <- calibrate(design = smho.dsgn,
                      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
                      population = pop.tots,
                      #      bounds = c(0.4, 3),
                      calfun = "raking",
                      maxit = 50, epsilon = 1e-7
                      )
# Check controls
svyby(~BEDS, by=~as.factor(hosp.type), design=sam.rake, FUN=svytotal)
svytotal(~SEENCNT, sam.rake)
svytotal(~EOYCNT, sam.rake)
svytotal(~ as.factor(hosp.type), sam.rake)

# Estimate expenditure totals for all design objects
svytotal(~ EXPTOTAL, smho.dsgn)
cv(svytotal(~ EXPTOTAL, smho.dsgn))
confint(svytotal(~ EXPTOTAL, smho.dsgn))

svytotal(~ EXPTOTAL, sam.lin)
cv(svytotal(~ EXPTOTAL, sam.lin))
confint(svytotal(~ EXPTOTAL, sam.lin))

svytotal(~ EXPTOTAL, sam.linBD)
cv(svytotal(~ EXPTOTAL, sam.linBD))
confint(svytotal(~ EXPTOTAL, sam.linBD))

svytotal(~ EXPTOTAL, sam.rake)
cv(svytotal(~ EXPTOTAL, sam.rake))
confint(svytotal(~ EXPTOTAL, sam.rake))

#*********************************************************************************************************
# Plot weight relationships
#*********************************************************************************************************
postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig145.eps",
           width=8,
           height=4)

par(mfrow = c(1,2),
    mar = c(4,4,2,1),
    mgp = c(2.5, 1, 0))
plot(d, weights(sam.lin),
     col = "black",
     pch = 16,
     ylab = "weight")
points(d, weights(sam.rake),
#       bg = "cyan",
       bg = "gray70",
       pch = 21)
points(d, weights(sam.linBD),
       col = "black",
       lwd = 1.5,
       #    bg = "cyan",
       pch = 2)
abline(0,1)

legend("topleft",
       legend=c("unbounded linear","unbounded raking", "bounded linear"),
       pch = c(16,21,2),
       pt.bg =c("black", "gray70", "green3"))

plot(d, weights(sam.lin)/d,
     col = "black",
     pch = 16,
     ylim = c(0, 3.1),
     ylab = "weight / d"
     )
points(d, weights(sam.rake)/d,
#       bg = "cyan",
       bg = "gray70",
       pch = 21)
points(d, weights(sam.linBD)/d,
       col = "black",
       pch = 2)
abline(h = 0.4, col = "gray70", lwd = 2)
abline(h = 3, col = "gray70", lwd = 2)

arrows(x0 = 7.5, y0 = 0.15,
       x1 = 5, y1 = -0.05,
       length = 0.125,
       lwd = 4,
       #col = "cadetblue2")
       col = "gray50")
dev.off()

#*********************************************************************************************************
# Check ranges of weight adjustments
#*********************************************************************************************************
range(weights(sam.lin)/weights(smho.dsgn))
[1] 0.04642683 2.29984987
range(weights(sam.rake)/weights(smho.dsgn))
[1] 0.5 3.0
range(weights(sam.linBD)/weights(smho.dsgn))
[1] 0.5 3.0
