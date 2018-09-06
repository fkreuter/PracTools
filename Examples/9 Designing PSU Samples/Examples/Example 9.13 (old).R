#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.13.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     12/31/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for a 2-stage design with either tract of block groups as PSU's.
#           Note that these measures of homogeneity apply to an srs/srs design since the delta reduces
#           to the formula used below only
#*********************************************************************************************************

require(lme4)
attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData")
    # create var that is combo of TRACT/BLKGROUP
trtBG <- with(MDarea.pop, 10*TRACT + BLKGROUP)

    # values of Nbar are very large so that expression for delta that includes
    # Nbar will be nearly equal to the simpler formula, sigma2.a / (sigma2.a + sigma2.e)
N <- table(MDarea.pop$TRACT)
Nbar.trt <- mean(N)
N <- table(trtBG)
Nbar.BG <- mean(N)

del <- matrix(0, ncol=2, nrow=5)

#----------------------------------------------------------------------------------------
    # PSUs as clusters
m.y1 <- lmer(y1 ~ (1 | PSU), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
    #************** As of lme version 1.1-6 slotNames and tt@REmat no longer works **************
#slotNames(tt)
#vmat <- data.frame(tt@REmat)
    # numbers in vmat are stored as factors. next some awkward syntax to
    # to convert them to numbers.
#vc <- as.numeric(as.character(vmat[,3]))

VarCorr(m.y1)   # give sqrt's of varcomps
# Groups   Name        Std.Dev.
# PSU      (Intercept)  6.0664
# Residual             84.0963
vc <- as.data.frame(VarCorr(m.y1))

del[1,1] <- vc[1,4] / sum(vc[,4])

m.y2 <- lmer(y2 ~ (1 | PSU), data = MDarea.pop)
tt <- summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[2,1] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[3,1] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[4,1] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[5,1] <- vc[1] / sum(vc)


#----------------------------------------------------------------------------------------
    # SSU as clusters
m.y1 <- lmer(y1 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y1)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[1,2] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[2,2] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[3,2] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[4,2] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[5,2] <- vc[1] / sum(vc)

round(del,4)

#----------------------------------------------------------------------------------------

# TRACTS as clusters
m.y1 <- lmer(y1 ~ (1 | TRACT), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
slotNames(tt)
vmat <- data.frame(tt@REmat)
    # numbers in vmat are stored as factors. next some awkward syntax to
    # to convert them to numbers.
vc <- as.numeric(as.character(vmat[,3]))
del[1,1] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | TRACT), data = MDarea.pop)
tt <- summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[2,1] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[3,1] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[4,1] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[5,1] <- vc[1] / sum(vc)


#----------------------------------------------------------------------------------------
    # trtBG as clusters
m.y1 <- lmer(y1 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y1)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[1,2] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[2,2] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[3,2] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[4,2] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
del[5,2] <- vc[1] / sum(vc)

round(del,4)

#--------------------------------------------------------------------------------------------------
# code below from C:\Projects\Variance components\Two-stage\moh2.calc.R
#--------------------------------------------------------------------------------------------------
