#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.13.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     12/31/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for a 2-stage design with either tract of block groups as PSU's.
#           Note that these measures of homogeneity apply to an srs/srs design. Compare model correlations 
#           to moh's computed with formulas that account for varying sizes of PSUs.
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

moh <- matrix(0, ncol=6, nrow=5)
dimnames(moh)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(moh)[[2]] <- c("As2a", "As2e", "Adel","Bs2a", "Bs2e", "Bdel")


#----------------------------------------------------------------------------------------
    # PSUs as clusters
m.y1 <- lmer(y1 ~ (1 | PSU), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
slotNames(tt)
vmat <- data.frame(tt@REmat)
    # numbers in vmat are stored as factors. next some awkward syntax to
    # to convert them to numbers.
vc <- as.numeric(as.character(vmat[,3]))
moh[1,1:2] <- vc
moh[1,3] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | PSU), data = MDarea.pop)
tt <- summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[2,1:2] <- vc
moh[2,3] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[3,1:2] <- vc
moh[3,3] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[4,1:2] <- vc
moh[4,3] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | PSU), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[5,1:2] <- vc
moh[5,3] <- vc[1] / sum(vc)


#----------------------------------------------------------------------------------------
    # SSU as clusters
m.y1 <- lmer(y1 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y1)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[1,4:5] <- vc
moh[1,6] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[2,4:5] <- vc
moh[2,6] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[3,4:5] <- vc
moh[3,6] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[4,4:5] <- vc
moh[4,6] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | SSU), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[5,4:5] <- vc
moh[5,6] <- vc[1] / sum(vc)

round(moh,4)
mohPSU.SSU <- moh

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
moh[1,1:2] <- vc
moh[1,3] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | TRACT), data = MDarea.pop)
tt <- summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[2,1:2] <- vc
moh[2,3] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[3,1:2] <- vc
moh[3,3] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[4,1:2] <- vc
moh[4,3] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | TRACT), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[5,1:2] <- vc
moh[5,3] <- vc[1] / sum(vc)


#----------------------------------------------------------------------------------------
    # trtBG as clusters
m.y1 <- lmer(y1 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y1)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[1,4:5] <- vc
moh[1,6] <- vc[1] / sum(vc)

m.y2 <- lmer(y2 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[2,4:5] <- vc
moh[2,6] <- vc[1] / sum(vc)

m.y3 <- lmer(y3 ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[3,4:5] <- vc
moh[3,6] <- vc[1] / sum(vc)

m.inscov <- lmer(ins.cov ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[4,4:5] <- vc
moh[4,6] <- vc[1] / sum(vc)

m.hospstay <- lmer(hosp.stay ~ (1 | trtBG), data = MDarea.pop)
tt <-summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
moh[5,4:5] <- vc
moh[5,6] <- vc[1] / sum(vc)

round(moh,4)
mohTrt.BG <- moh

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Evaluate delta formulas that are EM[B2 / (B2+W2)] in srs/srs sampling

del <- matrix(0, ncol=2, nrow=5)
dimnames(del)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(del)[[2]] <- c("Trt.del", "BG.del")

    # PSUs as clusters
N <- table(MDarea.pop$PSU)
Nbar.trt <- mean(N)
Nbar <- Nbar.trt
v2N <- var(N)/Nbar^2

mu <- mean(MDarea.pop$y1)
B <- (moh[1,1] + mu^2)*v2N + moh[1,1] + moh[1,2]/Nbar^2
BW <- (moh[1,1] + mu^2)*v2N + moh[1,1] + moh[1,2]*(1 + v2N + 1/Nbar^2)
del[1,1] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (moh[2,1] + mu^2)*v2N + moh[2,1] + moh[2,2]/Nbar^2
BW <- (moh[2,1] + mu^2)*v2N + moh[2,1] + moh[2,2]*(1 + v2N + 1/Nbar^2)
del[2,1] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (moh[3,1] + mu^2)*v2N + moh[3,1] + moh[3,2]/Nbar^2
BW <- (moh[3,1] + mu^2)*v2N + moh[3,1] + moh[3,2]*(1 + v2N + 1/Nbar^2)
del[3,1] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (moh[4,1] + mu^2)*v2N + moh[4,1] + moh[4,2]/Nbar^2
BW <- (moh[4,1] + mu^2)*v2N + moh[4,1] + moh[4,2]*(1 + v2N + 1/Nbar^2)
del[4,1] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (moh[5,1] + mu^2)*v2N + moh[5,1] + moh[5,2]/Nbar^2
BW <- (moh[5,1] + mu^2)*v2N + moh[5,1] + moh[5,2]*(1 + v2N + 1/Nbar^2)
del[5,1] <- B / BW

round(del,4)

    # SSUs as clusters
N <- table(MDarea.pop$SSU)
Nbar <- mean(N)
v2N <- var(N)/Nbar^2

mu <- mean(MDarea.pop$y1)
B <- (moh[1,4] + mu^2)*v2N + moh[1,4] + moh[1,5]/Nbar^2
BW <- (moh[1,4] + mu^2)*v2N + moh[1,4] + moh[1,5]*(1 + v2N + 1/Nbar^2)
del[1,2] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (moh[2,4] + mu^2)*v2N + moh[2,4] + moh[2,5]/Nbar^2
BW <- (moh[2,4] + mu^2)*v2N + moh[2,4] + moh[2,5]*(1 + v2N + 1/Nbar^2)
del[2,2] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (moh[3,4] + mu^2)*v2N + moh[3,4] + moh[3,5]/Nbar^2
BW <- (moh[3,4] + mu^2)*v2N + moh[3,4] + moh[3,5]*(1 + v2N + 1/Nbar^2)
del[3,2] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (moh[4,4] + mu^2)*v2N + moh[4,4] + moh[4,5]/Nbar^2
BW <- (moh[4,4] + mu^2)*v2N + moh[4,4] + moh[4,5]*(1 + v2N + 1/Nbar^2)
del[4,2] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (moh[5,4] + mu^2)*v2N + moh[5,4] + moh[5,5]/Nbar^2
BW <- (moh[5,4] + mu^2)*v2N + moh[5,4] + moh[5,5]*(1 + v2N + 1/Nbar^2)
del[5,2] <- B / BW

round(del,4)

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
B <- (moh[1,1] + mu^2)*v2N + moh[1,1] + moh[1,2]/Nbar.trt^2
BW <- (moh[1,1] + mu^2)*v2N + moh[1,1] + moh[1,2]*(1 + v2N + 1/Nbar.trt^2)
del[1,1] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (moh[2,1] + mu^2)*v2N + moh[2,1] + moh[2,2]/Nbar.trt^2
BW <- (moh[2,1] + mu^2)*v2N + moh[2,1] + moh[2,2]*(1 + v2N + 1/Nbar.trt^2)
del[2,1] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (moh[3,1] + mu^2)*v2N + moh[3,1] + moh[3,2]/Nbar.trt^2
BW <- (moh[3,1] + mu^2)*v2N + moh[3,1] + moh[3,2]*(1 + v2N + 1/Nbar.trt^2)
del[3,1] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (moh[4,1] + mu^2)*v2N + moh[4,1] + moh[4,2]/Nbar.trt^2
BW <- (moh[4,1] + mu^2)*v2N + moh[4,1] + moh[4,2]*(1 + v2N + 1/Nbar.trt^2)
del[4,1] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (moh[5,1] + mu^2)*v2N + moh[5,1] + moh[5,2]/Nbar.trt^2
BW <- (moh[5,1] + mu^2)*v2N + moh[5,1] + moh[5,2]*(1 + v2N + 1/Nbar.trt^2)
del[5,1] <- B / BW

round(del,4)

    # BGs as clusters
N <- table(trtBG)
Nbar.BG <- mean(N)
v2N <- var(N)/Nbar.BG^2

mu <- mean(MDarea.pop$y1)
B <- (moh[1,4] + mu^2)*v2N + moh[1,4] + moh[1,5]/Nbar.BG^2
BW <- (moh[1,4] + mu^2)*v2N + moh[1,4] + moh[1,5]*(1 + v2N + 1/Nbar.BG^2)
del[1,2] <- B / BW

mu <- mean(MDarea.pop$y2)
B <- (moh[2,4] + mu^2)*v2N + moh[2,4] + moh[2,5]/Nbar.BG^2
BW <- (moh[2,4] + mu^2)*v2N + moh[2,4] + moh[2,5]*(1 + v2N + 1/Nbar.BG^2)
del[2,2] <- B / BW

mu <- mean(MDarea.pop$y3)
B <- (moh[3,4] + mu^2)*v2N + moh[3,4] + moh[3,5]/Nbar.BG^2
BW <- (moh[3,4] + mu^2)*v2N + moh[3,4] + moh[3,5]*(1 + v2N + 1/Nbar.BG^2)
del[3,2] <- B / BW

mu <- mean(MDarea.pop$ins.cov)
B <- (moh[4,4] + mu^2)*v2N + moh[4,4] + moh[4,5]/Nbar.BG^2
BW <- (moh[4,4] + mu^2)*v2N + moh[4,4] + moh[4,5]*(1 + v2N + 1/Nbar.BG^2)
del[4,2] <- B / BW

mu <- mean(MDarea.pop$hosp.stay)
B <- (moh[5,4] + mu^2)*v2N + moh[5,4] + moh[5,5]/Nbar.BG^2
BW <- (moh[5,4] + mu^2)*v2N + moh[5,4] + moh[5,5]*(1 + v2N + 1/Nbar.BG^2)
del[5,2] <- B / BW

round(del,4)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Evaluate delta formulas that are EM[B2 / (B2+W2)] in ppswr/srs sampling

del <- matrix(0, ncol=2, nrow=5)
dimnames(del)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(del)[[2]] <- c("Trt.del", "BG.del")

    # PSUs as clusters
PSU <- unique(MDarea.pop$PSU)
M <- length(PSU)
N <- table(MDarea.pop$PSU)
Nbar <- mean(N)
v2N <- var(N)/Nbar^2

B <- moh[1,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[1,2]/Nbar
BW <- B + moh[1,2]
del[1,1] <- B / BW

B <- moh[2,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[2,2]/Nbar
BW <- B + moh[2,2]
del[2,1] <- B / BW

B <- moh[3,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[3,2]/Nbar
BW <- B + moh[3,2]
del[3,1] <- B / BW

B <- moh[4,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[4,2]/Nbar
BW <- B + moh[4,2]
del[4,1] <- B / BW

B <- moh[5,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[5,2]/Nbar
BW <- B + moh[5,2]
del[5,1] <- B / BW

round(del,3)

    # SSUs as clusters
SSU <- unique(MDarea.pop$SSU)
M <- length(SSU)
N <- table(MDarea.pop$SSU)
Nbar <- mean(N)
v2N <- var(N)/Nbar^2

B <- moh[1,4] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[1,5]/Nbar
BW <- B + moh[1,5]
del[1,2] <- B / BW

B <- moh[2,4] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[2,5]/Nbar
BW <- B + moh[2,5]
del[2,2] <- B / BW

B <- moh[3,4] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[3,5]/Nbar
BW <- B + moh[3,5]
del[3,2] <- B / BW

B <- moh[4,4] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[4,5]/Nbar
BW <- B + moh[4,5]
del[4,2] <- B / BW

B <- moh[5,4] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[5,5]/Nbar
BW <- B + moh[5,5]
del[5,2] <- B / BW

round(del,4)



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

B <- moh[1,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[1,2]/Nbar
BW <- B + moh[1,2]
del[1,1] <- B / BW

B <- moh[2,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[2,2]/Nbar
BW <- B + moh[2,2]
del[2,1] <- B / BW

B <- moh[3,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[3,2]/Nbar
BW <- B + moh[3,2]
del[3,1] <- B / BW

B <- moh[4,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[4,2]/Nbar
BW <- B + moh[4,2]
del[4,1] <- B / BW

B <- moh[5,1] * (1 - 1/M*(2-1/Nbar)*(v2N+1)) + moh[5,2]/Nbar
BW <- B + moh[5,2]
del[5,1] <- B / BW

round(del,3)

    # BGs as clusters
BG <- unique(trtBG)
M <- length(BG)
N <- table(trtBG)
Nbar.BG <- mean(N)
v2N <- var(N)/Nbar.BG^2

B <- moh[1,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + moh[1,5]/Nbar.BG
BW <- B + moh[1,5]
del[1,2] <- B / BW

B <- moh[2,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + moh[2,5]/Nbar.BG
BW <- B + moh[2,5]
del[2,2] <- B / BW

B <- moh[3,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + moh[3,5]/Nbar.BG
BW <- B + moh[3,5]
del[3,2] <- B / BW

B <- moh[4,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + moh[4,5]/Nbar.BG
BW <- B + moh[4,5]
del[4,2] <- B / BW

B <- moh[5,4] * (1 - 1/M*(2-1/Nbar.BG)*(v2N+1)) + moh[5,5]/Nbar.BG
BW <- B + moh[5,5]
del[5,2] <- B / BW

round(del,3)
