#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.13.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     12/31/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for a 2-stage design with either tract of block groups as PSU's.
#           Note that these measures of homogeneity apply to an srs/srs design. Compare model correlations
#           to moh's computed with formulas that account for varying sizes of PSUs.
#       Note: lme4 package switched from S4 objects back to S3 objectsw in v.1.1-6.  Syntax changed for
#       retrieving varcomps.
# REVISED:  06/23/2017 to extract variance components with VarCorr
#           lme4 version 1.1-13
#*********************************************************************************************************
require(lme4)
require(PracTools)
data(MDarea.pop)
#attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData")
    # create var that is combo of TRACT/BLKGROUP
trtBG <- with(MDarea.pop, 10*TRACT + BLKGROUP)

    # values of Nbar are very large so that expression for delta that includes
    # Nbar will be nearly equal to the simpler formula, sigma2.a / (sigma2.a + sigma2.e)
N <- table(MDarea.pop$TRACT)
Nbar.trt <- mean(N)
N <- table(trtBG)
Nbar.BG <- mean(N)

moh <- matrix(0, ncol=6, nrow=5)
dimnames(moh)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(moh)[[2]] <- c("As2a", "As2e", "Adel","Bs2a", "Bs2e", "Bdel")


#----------------------------------------------------------------------------------------
    # PSUs as clusters
m.y1 <- lmer(y1 ~ (1 | PSU), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
    #************** As of lme version 1.1-6 slotNames and tt@REmat no longer works **************
    #  lme4 version 1.1-13 used
#slotNames(tt)
#vmat <- data.frame(tt@REmat)
    # numbers in vmat are stored as factors. next some awkward syntax to
    # to convert them to numbers.
#vc <- as.numeric(as.character(vmat[,3]))

VarCorr(m.y1)   # gives sqrt's of varcomps
# Groups   Name        Std.Dev.
# PSU      (Intercept)  6.0664
# Residual             84.0963
vc <- as.data.frame(VarCorr(m.y1))

moh[1,1:2] <- vc[,4]
moh[1,3] <- vc[1,4] / sum(vc[,4])   # intracluster correlation

m.y2 <- lmer(y2 ~ (1 | PSU), data = MDarea.pop)
tt <- summary(m.y2)
vc <- as.data.frame(VarCorr(m.y2))
moh[2,1:2] <- vc[,4]
moh[2,3] <- vc[1,4] / sum(vc[,4])

m.y3 <- lmer(y3 ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.y3)
vc <- as.data.frame(VarCorr(m.y3))
moh[3,1:2] <- vc[,4]
moh[3,3] <- vc[1,4] / sum(vc[,4])

m.inscov <- lmer(ins.cov ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.inscov)
vc <- as.data.frame(VarCorr(m.inscov))
moh[4,1:2] <- vc[,4]
moh[4,3] <- vc[1,4] / sum(vc[,4])

m.hospstay <- lmer(hosp.stay ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.hospstay)
vc <- as.data.frame(VarCorr(m.hospstay))
moh[5,1:2] <- vc[,4]
moh[5,3] <- vc[1,4] / sum(vc[,4])
moh
    # cols. 1-3 give results for PSU as cluster
#                  As2a         As2e        Adel Bs2a Bs2e Bdel
#y1        3.680073e+01 7072.1795382 0.005176654    0    0    0
#y2        3.945020e-01   59.1967510 0.006620133    0    0    0
#y3        6.842498e+01  802.6752946 0.078550057    0    0    0
#ins.cov   7.288814e-04    0.1634383 0.004439872    0    0    0
#hosp.stay 7.762711e-05    0.0666997 0.001162477    0    0    0

#----------------------------------------------------------------------------------------
    # SSU as clusters

m.y1 <- lmer(y1 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y1)
vc <- as.data.frame(VarCorr(m.y1))
moh[1,4:5] <- vc[,4]
moh[1,6] <- vc[1,4] / sum(vc[,4])

m.y2 <- lmer(y2 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y2)
vc <- as.data.frame(VarCorr(m.y2))
moh[2,4:5] <- vc[,4]
moh[2,6] <- vc[1,4] / sum(vc[,4])

m.y3 <- lmer(y3 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y3)
vc <- as.data.frame(VarCorr(m.y3))
moh[3,4:5] <- vc[,4]
moh[3,6] <- vc[1,4] / sum(vc[,4])

m.inscov <- lmer(ins.cov ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.inscov)
vc <- as.data.frame(VarCorr(m.inscov))
moh[4,4:5] <- vc[,4]
moh[4,6] <- vc[1,4] / sum(vc[,4])

m.hospstay <- lmer(hosp.stay ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.hospstay)
vc <- as.data.frame(VarCorr(m.hospstay))
moh[5,4:5] <- vc[,4]
moh[5,6] <- vc[1,4] / sum(vc[,4])

round(moh,4)
mohPSU.SSU <- moh
moh
    # cols. 4-6 give results for SSU as cluster
#                  As2a         As2e        Adel         Bs2a         Bs2e        Bdel
#y1        3.680073e+01 7072.1795382 0.005176654 1.706683e+02 6.938277e+03 0.024007538
#y2        3.945020e-01   59.1967510 0.006620133 9.330379e-01 5.865560e+01 0.015657983
#y3        6.842498e+01  802.6752946 0.078550057 1.399717e+02 7.306227e+02 0.160777117
#ins.cov   7.288814e-04    0.1634383 0.004439872 1.877574e-03 1.622852e-01 0.011437268
#hosp.stay 7.762711e-05    0.0666997 0.001162477 2.231569e-04 6.655376e-02 0.003341827

#----------------------------------------------------------------------------------------

# TRACTS as clusters
moh <- matrix(0, ncol=6, nrow=5)
dimnames(moh)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(moh)[[2]] <- c("As2a", "As2e", "Adel","Bs2a", "Bs2e", "Bdel")

m.y1 <- lmer(y1 ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.y1)
vc <- as.data.frame(VarCorr(m.y1))
moh[1,1:2] <- vc[,4]
moh[1,3] <- vc[1,4] / sum(vc[,4])

m.y2 <- lmer(y2 ~ (1 | TRACT), data = MDarea.pop)
tt <- summary(m.y2)
vc <- as.data.frame(VarCorr(m.y2))
moh[2,1:2] <- vc[,4]
moh[2,3] <- vc[1,4] / sum(vc[,4])

m.y3 <- lmer(y3 ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.y3)
vc <- as.data.frame(VarCorr(m.y3))
moh[3,1:2] <- vc[,4]
moh[3,3] <- vc[1,4] / sum(vc[,4])

m.inscov <- lmer(ins.cov ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.inscov)
vc <- as.data.frame(VarCorr(m.inscov))
moh[4,1:2] <- vc[,4]
moh[4,3] <- vc[1,4] / sum(vc[,4])

m.hospstay <- lmer(hosp.stay ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.hospstay)
vc <- as.data.frame(VarCorr(m.hospstay))
moh[5,1:2] <- vc[,4]
moh[5,3] <- vc[1,4] / sum(vc[,4])
moh
    # cols. 1-3 give results for TRACT as cluster
#                  As2a         As2e        Adel Bs2a Bs2e Bdel
#y1        5.868048e+01 7.065664e+03 0.008236615    0    0    0
#y2        7.678631e-01 5.897041e+01 0.012853788    0    0    0
#y3        1.326880e+02 7.664789e+02 0.147567666    0    0    0
#ins.cov   1.255525e-03 1.630575e-01 0.007641056    0    0    0
#hosp.stay 1.060850e-04 6.667642e-02 0.001588515    0    0    0

#----------------------------------------------------------------------------------------
    # trtBG as clusters
m.y1 <- lmer(y1 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y1)
vc <- as.data.frame(VarCorr(m.y1))
moh[1,4:5] <- vc[,4]
moh[1,6] <- vc[1,4] / sum(vc[,4])

m.y2 <- lmer(y2 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y2)
vc <- as.data.frame(VarCorr(m.y2))
moh[2,4:5] <- vc[,4]
moh[2,6] <- vc[1,4] / sum(vc[,4])

m.y3 <- lmer(y3 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y3)
vc <- as.data.frame(VarCorr(m.y3))
moh[3,4:5] <- vc[,4]
moh[3,6] <- vc[1,4] / sum(vc[,4])

m.inscov <- lmer(ins.cov ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.inscov)
vc <- as.data.frame(VarCorr(m.inscov))
moh[4,4:5] <- vc[,4]
moh[4,6] <- vc[1,4] / sum(vc[,4])

m.hospstay <- lmer(hosp.stay ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.hospstay)
vc <- as.data.frame(VarCorr(m.hospstay))
moh[5,4:5] <- vc[,4]
moh[5,6] <- vc[1,4] / sum(vc[,4])

round(moh,4)
mohTrt.BG <- moh
moh
    # cols. 4-6 give results for trtBG as cluster
#                  As2a         As2e        Adel         Bs2a         Bs2e        Bdel
#y1        5.868048e+01 7.065664e+03 0.008236615 8.323922e+01 7.036262e+03 0.011691720
#y2        7.678631e-01 5.897041e+01 0.012853788 1.023460e+00 5.859697e+01 0.017166261
#y3        1.326880e+02 7.664789e+02 0.147567666 1.670016e+02 7.091072e+02 0.190617441
#ins.cov   1.255525e-03 1.630575e-01 0.007641056 2.363402e-03 1.618584e-01 0.014391522
#hosp.stay 1.060850e-04 6.667642e-02 0.001588515 2.120892e-04 6.656254e-02 0.003176195

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Evaluate delta formulas that are EM[B2 / (B2+W2)] in srs/srs sampling

del <- matrix(0, ncol=2, nrow=5)
dimnames(del)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(del)[[2]] <- c("PSU.del", "SSU.del")

    # PSUs as clusters
N <- table(MDarea.pop$PSU)
Nbar.trt <- mean(N)
Nbar <- Nbar.trt
v2N <- var(N)/Nbar^2

mu <- mean(MDarea.pop$y1)
B <- (mohPSU.SSU[1,1] + mu^2)*v2N + mohPSU.SSU[1,1] + mohPSU.SSU[1,2]/Nbar^2
BW <- (mohPSU.SSU[1,1] + mu^2)*v2N + mohPSU.SSU[1,1] + mohPSU.SSU[1,2]*(1 + v2N + 1/Nbar^2)
del[1,1] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (mohPSU.SSU[2,1] + mu^2)*v2N + mohPSU.SSU[2,1] + mohPSU.SSU[2,2]/Nbar^2
BW <- (mohPSU.SSU[2,1] + mu^2)*v2N + mohPSU.SSU[2,1] + mohPSU.SSU[2,2]*(1 + v2N + 1/Nbar^2)
del[2,1] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (mohPSU.SSU[3,1] + mu^2)*v2N + mohPSU.SSU[3,1] + mohPSU.SSU[3,2]/Nbar^2
BW <- (mohPSU.SSU[3,1] + mu^2)*v2N + mohPSU.SSU[3,1] + mohPSU.SSU[3,2]*(1 + v2N + 1/Nbar^2)
del[3,1] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (mohPSU.SSU[4,1] + mu^2)*v2N + mohPSU.SSU[4,1] + mohPSU.SSU[4,2]/Nbar^2
BW <- (mohPSU.SSU[4,1] + mu^2)*v2N + mohPSU.SSU[4,1] + mohPSU.SSU[4,2]*(1 + v2N + 1/Nbar^2)
del[4,1] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (mohPSU.SSU[5,1] + mu^2)*v2N + mohPSU.SSU[5,1] + mohPSU.SSU[5,2]/Nbar^2
BW <- (mohPSU.SSU[5,1] + mu^2)*v2N + mohPSU.SSU[5,1] + mohPSU.SSU[5,2]*(1 + v2N + 1/Nbar^2)
del[5,1] <- B / BW

round(del,4)
#          PSU.del SSU.del
#y1         0.0052       0
#y2         0.0066       0
#y3         0.0786       0
#ins.cov    0.0044       0
#hosp.stay  0.0012       0

    # SSUs as clusters
N <- table(MDarea.pop$SSU)
Nbar <- mean(N)
v2N <- var(N)/Nbar^2

mu <- mean(MDarea.pop$y1)
B <- (mohPSU.SSU[1,4] + mu^2)*v2N + mohPSU.SSU[1,4] + mohPSU.SSU[1,5]/Nbar^2
BW <- (mohPSU.SSU[1,4] + mu^2)*v2N + mohPSU.SSU[1,4] + mohPSU.SSU[1,5]*(1 + v2N + 1/Nbar^2)
del[1,2] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (mohPSU.SSU[2,4] + mu^2)*v2N + mohPSU.SSU[2,4] + mohPSU.SSU[2,5]/Nbar^2
BW <- (mohPSU.SSU[2,4] + mu^2)*v2N + mohPSU.SSU[2,4] + mohPSU.SSU[2,5]*(1 + v2N + 1/Nbar^2)
del[2,2] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (mohPSU.SSU[3,4] + mu^2)*v2N + mohPSU.SSU[3,4] + mohPSU.SSU[3,5]/Nbar^2
BW <- (mohPSU.SSU[3,4] + mu^2)*v2N + mohPSU.SSU[3,4] + mohPSU.SSU[3,5]*(1 + v2N + 1/Nbar^2)
del[3,2] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (mohPSU.SSU[4,4] + mu^2)*v2N + mohPSU.SSU[4,4] + mohPSU.SSU[4,5]/Nbar^2
BW <- (mohPSU.SSU[4,4] + mu^2)*v2N + mohPSU.SSU[4,4] + mohPSU.SSU[4,5]*(1 + v2N + 1/Nbar^2)
del[4,2] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (mohPSU.SSU[5,4] + mu^2)*v2N + mohPSU.SSU[5,4] + mohPSU.SSU[5,5]/Nbar^2
BW <- (mohPSU.SSU[5,4] + mu^2)*v2N + mohPSU.SSU[5,4] + mohPSU.SSU[5,5]*(1 + v2N + 1/Nbar^2)
del[5,2] <- B / BW

round(del,4)    # srs/srs with PSU or SSU var as psu's
#          PSU.del SSU.del
#y1         0.0052  0.0240
#y2         0.0066  0.0157
#y3         0.0786  0.1608
#ins.cov    0.0044  0.0114
#hosp.stay  0.0012  0.0033

#----------------------------------------------------------------------------------------
# Evaluate delta formulas that are EM[B2 / (B2+W2)] in srs/srs sampling

del <- matrix(0, ncol=2, nrow=5)
dimnames(del)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(del)[[2]] <- c("Trt.del", "BG.del")

    # tracts as clusters
N <- table(MDarea.pop$TRACT)
Nbar.trt <- mean(N)
v2N <- var(N)/Nbar.trt^2

mu <- mean(MDarea.pop$y1)
B <- (mohTrt.BG[1,1] + mu^2)*v2N + mohTrt.BG[1,1] + mohTrt.BG[1,2]/Nbar.trt^2
BW <- (mohTrt.BG[1,1] + mu^2)*v2N + mohTrt.BG[1,1] + mohTrt.BG[1,2]*(1 + v2N + 1/Nbar.trt^2)
del[1,1] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (mohTrt.BG[2,1] + mu^2)*v2N + mohTrt.BG[2,1] + mohTrt.BG[2,2]/Nbar.trt^2
BW <- (mohTrt.BG[2,1] + mu^2)*v2N + mohTrt.BG[2,1] + mohTrt.BG[2,2]*(1 + v2N + 1/Nbar.trt^2)
del[2,1] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (mohTrt.BG[3,1] + mu^2)*v2N + mohTrt.BG[3,1] + mohTrt.BG[3,2]/Nbar.trt^2
BW <- (mohTrt.BG[3,1] + mu^2)*v2N + mohTrt.BG[3,1] + mohTrt.BG[3,2]*(1 + v2N + 1/Nbar.trt^2)
del[3,1] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (mohTrt.BG[4,1] + mu^2)*v2N + mohTrt.BG[4,1] + mohTrt.BG[4,2]/Nbar.trt^2
BW <- (mohTrt.BG[4,1] + mu^2)*v2N + mohTrt.BG[4,1] + mohTrt.BG[4,2]*(1 + v2N + 1/Nbar.trt^2)
del[4,1] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (mohTrt.BG[5,1] + mu^2)*v2N + mohTrt.BG[5,1] + mohTrt.BG[5,2]/Nbar.trt^2
BW <- (mohTrt.BG[5,1] + mu^2)*v2N + mohTrt.BG[5,1] + mohTrt.BG[5,2]*(1 + v2N + 1/Nbar.trt^2)
del[5,1] <- B / BW

round(del,4)
#          Trt.del BG.del
#y1         0.1306      0
#y2         0.1790      0
#y3         0.6908      0
#ins.cov    0.4454      0
#hosp.stay  0.0173      0

    # BGs as clusters
N <- table(trtBG)
Nbar.BG <- mean(N)
v2N <- var(N)/Nbar.BG^2

mu <- mean(MDarea.pop$y1)
B <- (mohTrt.BG[1,4] + mu^2)*v2N + mohTrt.BG[1,4] + mohTrt.BG[1,5]/Nbar.BG^2
BW <- (mohTrt.BG[1,4] + mu^2)*v2N + mohTrt.BG[1,4] + mohTrt.BG[1,5]*(1 + v2N + 1/Nbar.BG^2)
del[1,2] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (mohTrt.BG[2,4] + mu^2)*v2N + mohTrt.BG[2,4] + mohTrt.BG[2,5]/Nbar.BG^2
BW <- (mohTrt.BG[2,4] + mu^2)*v2N + mohTrt.BG[2,4] + mohTrt.BG[2,5]*(1 + v2N + 1/Nbar.BG^2)
del[2,2] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (mohTrt.BG[3,4] + mu^2)*v2N + mohTrt.BG[3,4] + mohTrt.BG[3,5]/Nbar.BG^2
BW <- (mohTrt.BG[3,4] + mu^2)*v2N + mohTrt.BG[3,4] + mohTrt.BG[3,5]*(1 + v2N + 1/Nbar.BG^2)
del[3,2] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (mohTrt.BG[4,4] + mu^2)*v2N + mohTrt.BG[4,4] + mohTrt.BG[4,5]/Nbar.BG^2
BW <- (mohTrt.BG[4,4] + mu^2)*v2N + mohTrt.BG[4,4] + mohTrt.BG[4,5]*(1 + v2N + 1/Nbar.BG^2)
del[4,2] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (mohTrt.BG[5,4] + mu^2)*v2N + mohTrt.BG[5,4] + mohTrt.BG[5,5]/Nbar.BG^2
BW <- (mohTrt.BG[5,4] + mu^2)*v2N + mohTrt.BG[5,4] + mohTrt.BG[5,5]*(1 + v2N + 1/Nbar.BG^2)
del[5,2] <- B / BW

round(del,4)    # srs/srs with TRACT or trtBG var as psu's
#          Trt.del BG.del
#y1         0.1306 0.1561
#y2         0.1790 0.2115
#y3         0.6908 0.7464
#ins.cov    0.4454 0.4970
#hosp.stay  0.0173 0.0222

#----------------------------------------------------------------------------------------
# Evaluate delta formulas that are EM[B2 / (B2+W2)] in ppswr/srs sampling

#--------------------------------------------------------------------------------------------------
del <- matrix(0, ncol=2, nrow=5)
dimnames(del)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(del)[[2]] <- c("Trt.del", "BG.del")

    # tracts as clusters
trt <- unique(MDarea.pop$TRACT)
M <- length(trt)
N <- table(MDarea.pop$TRACT)
Nbar <- mean(N)
v2N <- var(N)/Nbar^2

B <- mohTrt.BG[1,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + mohTrt.BG[1,2]/Nbar
BW <- B + mohTrt.BG[1,2]
del[1,1] <- B / BW

B <- mohTrt.BG[2,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + mohTrt.BG[2,2]/Nbar
BW <- B + mohTrt.BG[2,2]
del[2,1] <- B / BW

B <- mohTrt.BG[3,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + mohTrt.BG[3,2]/Nbar
BW <- B + mohTrt.BG[3,2]
del[3,1] <- B / BW

B <- mohTrt.BG[4,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + mohTrt.BG[4,2]/Nbar
BW <- B + mohTrt.BG[4,2]
del[4,1] <- B / BW

B <- mohTrt.BG[5,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + mohTrt.BG[5,2]/Nbar
BW <- B + mohTrt.BG[5,2]
del[5,1] <- B / BW

round(del,4)
#          Trt.del BG.del
#y1         0.0083      0
#y2         0.0127      0
#y3         0.1444      0
#ins.cov    0.0077      0
#hosp.stay  0.0018      0

    # BGs as clusters
BG <- unique(trtBG)
M <- length(BG)
N <- table(trtBG)
Nbar.BG <- mean(N)
v2N <- var(N)/Nbar.BG^2

B <- mohTrt.BG[1,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + mohTrt.BG[1,5]/Nbar.BG
BW <- B + mohTrt.BG[1,5]
del[1,2] <- B / BW

B <- mohTrt.BG[2,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + mohTrt.BG[2,5]/Nbar.BG
BW <- B + mohTrt.BG[2,5]
del[2,2] <- B / BW

B <- mohTrt.BG[3,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + mohTrt.BG[3,5]/Nbar.BG
BW <- B + mohTrt.BG[3,5]
del[3,2] <- B / BW

B <- mohTrt.BG[4,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + mohTrt.BG[4,5]/Nbar.BG
BW <- B + mohTrt.BG[4,5]
del[4,2] <- B / BW

B <- mohTrt.BG[5,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + mohTrt.BG[5,5]/Nbar.BG
BW <- B + mohTrt.BG[5,5]
del[5,2] <- B / BW

round(del,4)
#          Trt.del BG.del
#y1         0.0083 0.0123
#y2         0.0127 0.0178
#y3         0.1444 0.1898
#ins.cov    0.0077 0.0150
#hosp.stay  0.0018 0.0039
