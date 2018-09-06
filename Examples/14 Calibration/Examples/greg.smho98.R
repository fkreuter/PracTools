#********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\15 Calibration\Examples\greg.smho98.R
# TOPIC:    Fit model for expenditures to pop, select sample, and illustrate GREG
# DATE:     07/08/2010
# AUTHOR:   R. Valliant
#********************************************************************************************************

#    Import smho98 file into R

require(survey)
require(sampling)
require(PracTools)

#smho.N874 <- read.csv("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.csv", row.names = 1)
data(smho.N874)
dim(smho.N874)

            # Some summary statistics on the population
table(smho.N874[, "FINDIRCT"], exclude = NULL)
table(smho.N874[, "hosp.type"], exclude = NULL)
summary(smho.N874[, "EXPTOTAL"])
summary(smho.N874[, "BEDS"])

by(smho.N874[,"BEDS"], smho.N874[,"hosp.type"], mean)

            # partial care, outpatient are hosp.type = 4
            # outpatient all have beds = 0, which goofs up the calibration modeling
            #   so, drop hosp.type = 4
delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]
dim(smho)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor=1, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#    text(0.5, 0.5, txt, cex = cex.cor * r)
    text(0.5, 0.5, txt, cex =  2)
}

            # scatterplot matrix
pairs(smho[, c("EXPTOTAL",
                     "BEDS",
                     "SEENCNT",
                     "EOYCNT")],
      lower.panel = panel.cor,
      cex.axis = 1.2
      )

            # plots by hospital type
par(mfrow=c(2,2),
    mar = c(4,4,2,1),
    mgp = c(2,1,0)
)
hosp.labs <- c("psychiatric",
               "residential or veterans",
               "general",
               "partial care, outpatient",
               "multi-service, substance abuse"
               )

for (h in c(1,2,3,5)){
    pick <- smho$hosp.type == h
    x <- smho[pick, "BEDS"]
    y <- smho[pick, "EXPTOTAL"]/(10^6)
    if (h==1) {ylabel <- "Expenditures (millions)"}
        else {ylabel <- ""}
    plot(x, y ,pch = 16,
         ylim = range(smho[, "EXPTOTAL"]/(10^6)),
         xlab = "Beds",
         ylab = ylabel,
         cex.axis = 1.2)
    lines(lowess(x,y), col="red")
    title(main = hosp.labs[h])
}

            # linear model with different slope on beds in each hosp type
m1 <- glm(EXPTOTAL ~ BEDS + SEENCNT + EOYCNT +
                    as.factor(FINDIRCT) +
                    as.factor(hosp.type),
     data = smho
)
summary(m1)

            # Separate slope on beds in each hosp type
m2 <- glm(EXPTOTAL ~ SEENCNT + EOYCNT +
                    as.factor(FINDIRCT) +
                    as.factor(hosp.type):BEDS,
     data = smho
)
summary(m2)

            # Separate slope on beds in each hosp type + factors for hosp type
m3 <- glm(EXPTOTAL ~ SEENCNT + EOYCNT +
                    as.factor(FINDIRCT) +
                    as.factor(hosp.type)*BEDS,
     data = smho
)
summary(m3)


par(mfrow=c(1,1))
plot(smho$BEDS, rstudent(m2),
     pch = 21,
     cex = 1.4,
#     bg = c("red", "green3", "blue", "orange", "yellow")[unclass(smho$hosp.type)],
     bg = c("gray0", "gray40", "gray80", "gray80", "gray100")[unclass(smho$hosp.type)],
     xlab = "Beds",
     ylab = "Studentized residuals"
)
abline(h = c(-3,3))
legend("topright",
        legend = hosp.labs[-4],
#          pt.bg = c("red", "green3", "blue", "yellow"),
        pt.bg = c("gray0", "gray40", "gray80", "gray100"),
        pch=21,
        cex = 1.2
)

rm(m1, m2)

##################################################################################
### Select a pps to sqrt(BEDS) sample and fit a linear model to EXPTOTAL
##################################################################################

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

pairs(sam.dat[, c("EXPTOTAL",
                     "BEDS",
                     "SEENCNT",
                     "EOYCNT")],
      lower.panel = panel.cor
      )
by(sam.dat[,"EXPTOTAL"], sam.dat[,"hosp.type"], mean)

##################################################################################
### Create a design object to use for calibration (LS distance fcn)
##################################################################################

smho.dsgn <- svydesign(ids = ~0,          # no clusters
          strata = NULL,                  # no strata
          data = data.frame(sam.dat),
          weights = ~d)

                        # Compute pop totals of auxiliaries
                        # Note these are the original not the recoded x’s
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
#(Intercept)     SEENCNT      EOYCNT    x.beds.1    x.beds.2    x.beds.3    x.beds.5
#        725     1349241      505345       37978       13066        9573       10077

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
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#-0.3983  5.7470  8.8320  9.0620 10.9300 33.8300

summary(weights(smho.dsgn))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  2.714   5.693   8.150   8.763  10.090  33.680


                        # Estimate expenditure total
svytotal(~ EXPTOTAL, sam.lin)
cv(svytotal(~ EXPTOTAL, sam.lin))

##################################################################################
### Compare to pi-estimator
##################################################################################

svytotal(~ EXPTOTAL, smho.dsgn)
cv(svytotal(~ EXPTOTAL, smho.dsgn))
SE(svytotal(~ EXPTOTAL, smho.dsgn))/SE(svytotal(~ EXPTOTAL, sam.lin))


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

            # Linear calibration with bounds
sam.linBD <- calibrate(design = smho.dsgn,
      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
      population = pop.tots,
      bounds = c(0.4, 3),
      calfun = "linear",
      maxit = 50, epsilon = 1e-7
          )
summary(weights(sam.linBD))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  1.296   5.778   8.911   9.063  10.920  33.920

            # Check controls
svyby(~BEDS, by=~as.factor(hosp.type), design=sam.linBD, FUN=svytotal)
svytotal(~BEDS, sam.linBD)
svytotal(~SEENCNT, sam.linBD)
svytotal(~EOYCNT, sam.linBD)

            # raking
sam.rake <- calibrate(design = smho.dsgn,
      formula = ~ SEENCNT + EOYCNT + as.factor(hosp.type):BEDS,
      population = pop.tots,
      bounds = c(0.4, 3),
      calfun = "raking",
      maxit = 100, epsilon = 1e-4
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

##################################################################################
### Plot weight relationships
##################################################################################

par(mfrow = c(1,2),
    mar = c(4,4,2,1),
    mgp = c(2.5, 1, 0))
plot(d, weights(sam.lin),
     col = "black",
     pch = 16,
     ylab = "weight")
points(d, weights(sam.rake),
    bg = "cyan",
    pch = 21)
points(d, weights(sam.linBD),
    col = "black",
    lwd = 1.5,
#    bg = "cyan",
    pch = 2)
abline(0,1)

legend("topleft",
       legend=c("unbounded linear","bounded raking", "bounded linear"),
       pch = c(16,21,2),
       pt.bg =c("black", "cyan", "green3"))

plot(d, weights(sam.lin)/d,
     col = "black",
     pch = 16,
     ylim = c(0, 3.1),
     ylab = "weight / d"
     )
points(d, weights(sam.rake)/d,
     bg = "cyan",
    pch = 21)
points(d, weights(sam.linBD)/d,
    col = "black",
    pch = 2)
abline(h = 0.4, col = "hotpink", lwd = 2)
abline(h = 3, col = "hotpink", lwd = 2)

arrows(x0 = 7.5, y0 = 0.15,
       x1 = 5, y1 = -0.05,
       length = 0.125,
       lwd = 4,
       col = "cadetblue2")


##################################################################################
### Check ranges of weight adjustments
##################################################################################
> range(weights(sam.lin)/weights(smho.dsgn))
[1] -0.09021926  1.79568380
> range(weights(sam.rake)/weights(smho.dsgn))
[1] 0.2351695 1.9366216
> range(weights(sam.linBD)/weights(smho.dsgn))
[1] 0.400000 1.856136
