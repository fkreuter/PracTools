#***************************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\12 Solution Area Sample\Anne Arundel.MD.analysis.R   
# TOPIC:    Chapter 12 solution to area sampling problem                                                        
# DATE:     10/30/2010                                                                                          
# AUTHOR:   R. Valliant                                                                                         
# PURPOSE:  Solution of Project #2: select area sample from Anne Arundel County                                 
#***************************************************************************************************************

require(MASS)
require(doBy)
require(sampling)

anne.trt <- read.csv("C:\\Projects\\Practical Tools Book\\Book Chapters\\12 Solution Area Sample\\Anne Arundel.MD.solution.tracts.csv",
                  colClasses = c(rep("numeric",4),
                                 "character",
                                 rep("numeric",20))
)
anne.bg <- read.csv("C:\\Projects\\Practical Tools Book\\Book Chapters\\12 Solution Area Sample\\Anne Arundel.MD.solution.BGs.csv",
                  colClasses = c(rep("numeric",4),
                                 "character",
                                 rep("numeric",20))
)

dimnames(anne.trt)[[2]]
dimnames(anne.bg)[[2]]


        # summaries for tract pop counts and S(i)
rbind(
summary(anne.trt$pop.18to24yrs),
summary(anne.trt$pop.25to44yrs),
summary(anne.trt$pop.45to54.yrs),
summary(anne.trt$pop.55to64.yrs),
summary(anne.trt$pop.65plus),
summary(anne.trt$Composite.MOS)
)

        # summaries for tract workloads by age group
round(rbind(
summary(anne.trt$Tract.prob),
summary(anne.trt$Wrkld.18to24.yrs),
summary(anne.trt$Wrkld.25to44yrs),
summary(anne.trt$Wrkld.45to54yrs),
summary(anne.trt$Wrkld.55to64yrs),
summary(anne.trt$Wrkld.65plus),
summary(anne.trt$Total.wrkld)
), 3)

        # summaries for BG pop counts and S(i)
rbind(
summary(anne.bg$pop.18to24yrs),
summary(anne.bg$pop.25to44yrs),
summary(anne.bg$pop.45to54.yrs),
summary(anne.bg$pop.55to64.yrs),
summary(anne.bg$pop.65plus),
summary(anne.bg$Composite.MOS)
)

        # summaries for BG workloads by age group
round(rbind(
summary(anne.bg$BG.prob),
summary(anne.bg$Wrkld.18to24.yrs),
summary(anne.bg$Wrkld.25to44yrs),
summary(anne.bg$Wrkld.45to54yrs),
summary(anne.bg$Wrkld.55to64yrs),
summary(anne.bg$Wrkld.65plus),
summary(anne.bg$Total.wrkld)
), 3)

        # check whether expected workload in each domain is <= pop size in domain
test.wkld <- anne.bg[, c("Wrkld.18to24.yrs",
                         "Wrkld.25to44.yrs",
                         "Wrkld.45to54.yrs",
                         "Wrkld.55to64.yrs",
                         "Wrkld.65plus")] <=
             anne.bg[, c("pop.18to24.yrs",
                         "pop.25to44.yrs",
                         "pop.45to54.yrs",
                         "pop.55to64.yrs",
                         "pop.65plus")]   
test.wkld[is.na(test.wkld)] <- 0
small.bgs <- (1:nrow(test.wkld))[apply(test.wkld,1,sum) != 5]
small.bgs <- anne.bg[small.bgs, ]
small.bgs[order(small.bgs$TRACT),]

        # list the ingredients for Table 12.2               
small.bgs[, c("TRACT","BG", 
              "pop.18to24.yrs",
              "pop.25to44.yrs",
              "pop.45to54.yrs",
              "pop.55to64.yrs",
              "pop.65plus")]
small.bgs[, c("TRACT","BG", 
              "Wrkld.18to24.yrs",
              "Wrkld.25to44.yrs",
              "Wrkld.45to54.yrs",
              "Wrkld.55to64.yrs",
              "Wrkld.65plus")]
               
        # combine small BGs
anne.bg$trtbg <- anne.bg$TRACT + as.numeric(anne.bg$BG)/10
anne.bg$trtbg[anne.bg$trtbg == 701400.3] <- 701400.2
anne.bg$trtbg[anne.bg$trtbg == 740602.1] <- 740602.2
anne.bg$trtbg[anne.bg$trtbg == 740603.1] <- 740603.2
anne.bg$trtbg[anne.bg$trtbg == 740603.3] <- 740603.2
anne.bg$trtbg[anne.bg$trtbg == 741100.1] <- 740603.2
anne.bg$trtbg[anne.bg$trtbg == 750700.2] <- 750700.1
anne.bg$trtbg[anne.bg$trtbg == 750801.5] <- 750801.4

        # combine small tract in Fort Meade with adjacent one
anne.trt$TRACT[anne.trt$TRACT == 741100] <- 740603

        # compute MOS's for combined BGs and tracts
bg.mos <- summaryBy(Composite.MOS ~ trtbg, data = anne.bg, FUN = sum)
        # attach tract number to bg.mos
bg.mos$tract <- floor(bg.mos[,1])
trt.mos <- summaryBy(Composite.MOS ~ TRACT, data = anne.trt, FUN = sum)
summary(bg.mos$Composite.MOS.sum)
summary(trt.mos$Composite.MOS.sum)


        # compute overall tract and BG selection probs
ell <- 25
pi.i <- ell * trt.mos$Composite.MOS.sum / sum(trt.mos$Composite.MOS.sum)
pi.ij <- ell * bg.mos$Composite.MOS.sum / sum(bg.mos$Composite.MOS.sum)
        # summarize sel probs and wts
summary(pi.i)
summary(pi.ij)
summary(1/pi.i)
summary(1/pi.ij)

        # select a sample of tracts and BGs
set.seed(-741881304)
        # select tracts
trt.sam <- UPsampford(pik = pi.i,eps=1e-6)
sum(trt.sam)
trt.sam <- (1:nrow(trt.mos))[trt.sam == 1]
sample.tracts <- trt.mos[trt.sam,][,1]
sample.tracts

        # select BG's within sample tracts
bgsub <- bg.mos[,"tract"] %in% sample.tracts
bgsub.mos <- bg.mos[bgsub,]
trt.sums <- summaryBy(Composite.MOS.sum ~ tract, data = bgsub.mos, FUN = sum)

bgsub.frm <- merge(x=trt.sums, y=bgsub.mos, by="tract")
pi.j.i <- bgsub.frm$Composite.MOS.sum / bgsub.frm$Composite.MOS.sum.sum

sample.BGs <- NULL
for (i in 1:ell){
    pick <- bgsub.frm[, "tract"] == sample.tracts[i] 
    prob <- pi.j.i[pick]
    sam <- UPrandomsystematic(pik = prob)
    sam <- bgsub.frm[pick,][sam == 1,]
    sample.BGs <- rbind(sample.BGs, sam)
}

        # object with sample tract and BG ID's
sample.BGs <- cbind(sample.tracts, sample.BGs[,3])

wklds <- anne.bg[anne.bg$trtbg %in% sample.BGs[,2],
                    c("trtbg",
                      "Wrkld.18to24.yrs",
                      "Wrkld.25to44.yrs",
                      "Wrkld.45to54.yrs",
                      "Wrkld.55to64.yrs",
                      "Wrkld.65plus",
                      "Total.wrkld") ]
wklds

            # Table 12.4
cbind(sample.BGs, round(wklds[, 2:7], 1))

#______Quality control checks_____
summaryBy
yij.d <- anne.bg[anne.bg[,"trtbg"] %in% sample.BGs[, 2],]
yij.d <- yij.d[, c("pop.18to24.yrs",
                   "pop.25to44.yrs",  
                   "pop.45to54.yrs",
                   "pop.55to64.yrs",
                   "pop.65plus")]

Sij.bgsam <- bgsub.frm[bgsub.frm$trtbg %in% sample.BGs[,2],]$Composite.MOS.sum
sam.pi.ij <- ell * Sij.bgsam / sum(anne.bg$Composite.MOS)
fd <- c(0.005069966, 0.001242699, 0.002791074, 0.004382409, 0.004101302)

tot.ij.d <- as.matrix(yij.d) %*% fd / sam.pi.ij
tot.ij.d

        # est of total across all sample tracts, BGs
sum(tot.ij.d)
        # pi-est of total for each domain
tot.d <- apply(as.matrix(yij.d) / sam.pi.ij, 2, sum)
        # pi-est of total for all domains
sum(tot.d)
