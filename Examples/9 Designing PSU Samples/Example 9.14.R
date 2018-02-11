#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.14.R
# PROJECT:  Variance component estimation, 3-stage sampling
# DATE:     12/07/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for a 3-stage design with either tract of block groups as PSU's.
#*********************************************************************************************************

require(lme4)
require(PracTools)
#attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData")
    # create var that is combo of TRACT/BLKGROUP
data(MDarea.pop)

mu <- c(mean(MDarea.pop$y1),
        mean(MDarea.pop$y2),
        mean(MDarea.pop$y3),
        mean(MDarea.pop$ins.cov),
        mean(MDarea.pop$hosp.stay))

del.cal <- function(mo, mu, pp){
#browser()
    B <- mo[1]*sum(Qi^2*(1-pp)/pp) + mo[2]*sum(Ni*Qbar.i^2*(rvQi + 1)*(1-pp)/pp) + mo[3]*sum(Qi*(1-pp)/pp) +
             mu^2*(sum(Qi^2/pp) - Q^2)
    W <- (mo[2]+mo[3]) * sum(Qi^2/pp)
    del1 <- B / (B + W)
    W2 <- (mo[1] + mu^2)*sum(Ni^2*Qbar.i^2*rvQi/pp) + mo[2]*sum(Ni^2*Qbar.i^2*(rvQi + 1)/pp) + mo[3]*sum(Ni*Qi/pp)
    W3 <- mo[3]*sum(Ni^2*Qbar.i^2*(rvQi + 1)/pp)
    del2 <- W2 / (W2 + W3)
    c(del1,del2)
}

trtBG <- with(MDarea.pop, 10*TRACT + BLKGROUP)


    # matrix for estimates of model rho's
moh <- matrix(0, ncol=5, nrow=5)
dimnames(moh)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(moh)[[2]] <- c("sig2a", "sig2b", "sig2e", "rho1", "rho2")

#**************************************************************************************************
#   srs/srs/srs
#**************************************************************************************************
    # PSUs as clusters
    # SSU's as 2nd stage units

    # Compute relvariances of PSU and SSU sizes
rm(Qi,Qij,Q,xx,Ni,M,Qbar.i,S2Qi,rvQi,rvQi,Qbar,S2Q,rvQ)

Qi <- as.vector(table(MDarea.pop$PSU))
Qij <- as.vector(table(MDarea.pop$SSU))
Q <- sum(Qi)

        # extract first ssu in each psu and count ssu's per psu
        # identify psu IDs for 1st instance of each ssuID
first <- do.call("rbind",list(by(MDarea.pop$PSU,MDarea.pop$SSU,head,1)))
        # identify  first row for each ssu
xx <- do.call("rbind",list(by(1:nrow(MDarea.pop),MDarea.pop$SSU,head,1)))
Ni <- table(first)
N <- sum(Ni)
M <- length(unique(MDarea.pop$PSU))

Qbar.i <- as.vector(Qi/Ni)
S2Qi <- by(Qij, INDICES=first, var)
rvQi <- as.vector(S2Qi/Qbar.i^2)
Qbar <- sum(Qi)/M
S2Q <- var(Qi)
rvQ <- S2Q / Qbar^2

    # model var comps
m.y1 <- lmer(y1 ~ (1 | PSU) + (1 | SSU), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
    # order of varcomps in vc is SSU, PSU, Residual
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[1,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.y2 <- lmer(y2 ~ (1 | PSU) + (1 | SSU), data = MDarea.pop)
tt <- summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[2,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.y3 <- lmer(y3 ~ (1 | PSU) + (1 | SSU), data = MDarea.pop)
tt <- summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[3,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.inscov <- lmer(ins.cov ~ (1 | PSU) + (1 | SSU), data = MDarea.pop)
tt <- summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[4,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.hospstay <- lmer(hosp.stay ~ (1 | PSU) + (1 | SSU), data = MDarea.pop)
tt <- summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[5,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

    # model correlations within PSUs and SSUs
round(moh,4)
modcorr.PSU.SSU <- moh

#--------------------------------------------------------------------------------------------------
# Evaluate delta formulas that are EM[B2 / (B2+W2)] in srs/srs/srs sampling

del <- matrix(0, ncol=2, nrow=5)
dimnames(del)[[1]] <- c("y1","y2","y3","ins.cov","hosp.stay")
dimnames(del)[[2]] <- c("PSU.del", "SSU.del")

    # PSUs as clusters
    # SSUs as 2nd stage units

pp <- rep(1/M,M)
for (i in 1:5){
    del[i,] <- del.cal(modcorr.PSU.SSU[i,], mu=mu[i], pp=pp)
}
    # PSU, SSU; srs/srs/srs design
round(del,4)
del.PSU.SSU.srs <- del

#--------------------------------------------------------------------------------------------------
    # TRACTS as PSU's, trtBG's as SSU's
rm(Qi,Qij,Q,xx,Ni,M,Qbar.i,S2Qi,rvQi,rvQi,Qbar,S2Q,rvQ)

del <- matrix(0, ncol=2, nrow=5)

Qi <- as.vector(table(MDarea.pop$TRACT))
Qij <- as.vector(table(trtBG))
Q <- sum(Qi)
xx <- unique(trtBG)
trt <- floor(xx/10)
Ni <- by(rep(1,length(xx)), INDICES=trt,sum)
Ni <- as.vector(Ni)
M <- length(unique(MDarea.pop$TRACT))

Qbar.i <- as.vector(Qi/Ni)
S2Qi <- by(Qij, INDICES=trt, var)
rvQi <- as.vector(S2Qi/Qbar.i^2)
miss <- is.na(rvQi)
rvQi[miss] <- 0
Qbar <- sum(Qi)/M
S2Q <- var(Qi)
rvQ <- S2Q / Qbar^2

moh <- matrix(0, ncol=5, nrow=5)

m.y1 <- lmer(y1 ~ (1 | TRACT) + (1 | trtBG), data = MDarea.pop)
tt <- summary(m.y1)
    # extract var comps and compute delta
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[1,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.y2 <- lmer(y2 ~ (1 | TRACT) + (1 | trtBG), data = MDarea.pop)
tt <- summary(m.y2)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[2,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.y3 <- lmer(y3 ~ (1 | TRACT) + (1 | trtBG), data = MDarea.pop)
tt <- summary(m.y3)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[3,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.inscov <- lmer(ins.cov ~ (1 | TRACT) + (1 | trtBG), data = MDarea.pop)
tt <- summary(m.inscov)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[4,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

m.hospstay <- lmer(hosp.stay ~ (1 | TRACT) + (1 | trtBG), data = MDarea.pop)
tt <- summary(m.hospstay)
vmat <- data.frame(tt@REmat)
vc <- as.numeric(as.character(vmat[,3]))
rho1 <- vc[2] / sum(vc)
rho2 <- vc[1]/ (vc[1] + vc[3])
moh[5,] <- c(vc[2], vc[1], vc[3], rho1, rho2)

round(moh,4)
modcorr.trtBG <- moh

    # psu = TRACT, ssu = trtBG; design = srs/srs/srs
pp <- rep(1/M,M)
for (i in 1:5){
    del[i,] <- del.cal(modcorr.trtBG[i,], mu=mu[i], pp=pp)
}
    # TRACT, BG; srs/srs/srs design
round(del,4)

del.trtBG.srs <- del


#**************************************************************************************************
#   ppswr/srs/srs
#**************************************************************************************************
    # PSUs as clusters
    # SSU's as 2nd stage units
 # matrix for estimates of model rho's

del <- matrix(0, ncol=2, nrow=5)

    # Compute relvariances of PSU and SSU sizes
rm(Qi,Qij,Q,xx,Ni,M,Qbar.i,S2Qi,rvQi,rvQi,Qbar,S2Q,rvQ)

Qi <- as.vector(table(MDarea.pop$PSU))
Qij <- as.vector(table(MDarea.pop$SSU))
Q <- sum(Qi)

        # extract first ssu in each psu and count ssu's per psu
        # identify psu IDs for 1st instance of each ssuID
first <- do.call("rbind",list(by(MDarea.pop$PSU,MDarea.pop$SSU,head,1)))
        # identify  first row for each ssu
xx <- do.call("rbind",list(by(1:nrow(MDarea.pop),MDarea.pop$SSU,head,1)))
Ni <- table(first)
N <- sum(Ni)
M <- length(unique(MDarea.pop$PSU))
Nbar <- N/M

Qbar.i <- as.vector(Qi/Ni)
S2Qi <- by(Qij, INDICES=first, var)
rvQi <- as.vector(S2Qi/Qbar.i^2)
Qbar <- sum(Qi)/M
S2Q <- var(Qi)
rvQ <- S2Q / Qbar^2


    # PSU, SSU; ppswr/srs/srs
pp <- Qi/Q
for (i in 1:5){
    del[i,] <- del.cal(modcorr.PSU.SSU[i,], mu=mu[i], pp=pp)
}

round(del,4)

del.PSU.SSU.pps <- del

#--------------------------------------------------------------------------------------------------
    # TRACTS as PSU's, trtBG's as SSU's

del <- matrix(0, ncol=2, nrow=5)
rm(Qi,Qij,Q,xx,Ni,M,Qbar.i,S2Qi,rvQi,rvQi,Qbar,S2Q,rvQ)

Qi <- as.vector(table(MDarea.pop$TRACT))
Qij <- as.vector(table(trtBG))
Q <- sum(Qi)
xx <- unique(trtBG)
trt <- floor(xx/10)
Ni <- by(rep(1,length(xx)), INDICES=trt,sum)
Ni <- as.vector(Ni)
M <- length(unique(MDarea.pop$TRACT))

Qbar.i <- as.vector(Qi/Ni)
S2Qi <- by(Qij, INDICES=trt, var)
rvQi <- as.vector(S2Qi/Qbar.i^2)
miss <- is.na(rvQi)
rvQi[miss] <- 0
Qbar <- sum(Qi)/M
S2Q <- var(Qi)
rvQ <- S2Q / Qbar^2

    # TRACT, BG; ppswr/srs/srs
pp <- Qi/Q
for (i in 1:5){
    del[i,] <- del.cal(modcorr.trtBG[i,], mu=mu[i], pp=pp)
}

round(del,4)
del.trtBG.pps <- del

    # combine moh's from designs and psu's/ssu's
rho.mod <- round(cbind(modcorr.PSU.SSU[,4:5], modcorr.trtBG[,4:5]), 4)
dimnames(rho.mod)[[2]] <- c("PSU.SSU rho1", "PSU.SSU rho2",
            "trtBG rho1", "trtBG rho2")
rho.mod

    # combine moh's from designs and psu's/ssu's
moh.mat <- round(
    cbind(del.PSU.SSU.srs, del.trtBG.srs,
          del.PSU.SSU.pps, del.trtBG.pps)
, 4)
dimnames(moh.mat)[[2]] <-
          c("PSU.SSU.srs del1", "PSU.SSU.srs del2",
            "trtBG.srs del1", "trtBG.srs del2",
            "PSU.SSU.pps del1", "PSU.SSU.pps del2",
            "trtBG.pps del1", "trtBG.pps del2")
moh.mat
