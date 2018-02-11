#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.4.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/05/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for a ppswr/srs design
#*********************************************************************************************************

#attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)
data(MDarea.pop)

trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
pp.PSU <- table(MDarea.pop$PSU) / nrow(MDarea.pop)
pp.SSU <- table(MDarea.pop$SSU) / nrow(MDarea.pop)

pp.trt <- table(MDarea.pop$TRACT) / nrow(MDarea.pop)
pp.BG <- table(trtBG) / nrow(MDarea.pop)


        # PSUs and SSUs as clusters
BW <- rbind(
            c(BW2stagePPS(MDarea.pop$y1, pp=pp.PSU, psuID=MDarea.pop$PSU),
              BW2stagePPS(MDarea.pop$y1, pp=pp.SSU, psuID=MDarea.pop$SSU)),

            c(BW2stagePPS(MDarea.pop$y2, pp=pp.PSU, psuID=MDarea.pop$PSU),
              BW2stagePPS(MDarea.pop$y2, pp=pp.SSU, psuID=MDarea.pop$SSU)),

            c(BW2stagePPS(MDarea.pop$y3, pp=pp.PSU, psuID=MDarea.pop$PSU),
              BW2stagePPS(MDarea.pop$y3, pp=pp.SSU, psuID=MDarea.pop$SSU)),

            c(BW2stagePPS(MDarea.pop$ins.cov, pp=pp.PSU, psuID=MDarea.pop$PSU),
              BW2stagePPS(MDarea.pop$ins.cov, pp=pp.SSU, psuID=MDarea.pop$SSU)),

            c(BW2stagePPS(MDarea.pop$hosp.stay, pp=pp.PSU, psuID=MDarea.pop$PSU),
              BW2stagePPS(MDarea.pop$hosp.stay, pp=pp.SSU, psuID=MDarea.pop$SSU))
)

round(BW,4)

        # TRACTS and BGs as clusters
BW <- rbind(
            c(BW2stagePPS(MDarea.pop$y1, pp=pp.trt, psuID=MDarea.pop$TRACT),
              BW2stagePPS(MDarea.pop$y1, pp=pp.BG, psuID=trtBG)),

            c(BW2stagePPS(MDarea.pop$y2, pp=pp.trt, psuID=MDarea.pop$TRACT),
              BW2stagePPS(MDarea.pop$y2, pp=pp.BG, psuID=trtBG)),

            c(BW2stagePPS(MDarea.pop$y3, pp=pp.trt, psuID=MDarea.pop$TRACT),
              BW2stagePPS(MDarea.pop$y3, pp=pp.BG, psuID=trtBG)),

            c(BW2stagePPS(MDarea.pop$ins.cov, pp=pp.trt, psuID=MDarea.pop$TRACT),
              BW2stagePPS(MDarea.pop$ins.cov, pp=pp.BG, psuID=trtBG)),

            c(BW2stagePPS(MDarea.pop$hosp.stay, pp=pp.trt, psuID=MDarea.pop$TRACT),
              BW2stagePPS(MDarea.pop$hosp.stay, pp=pp.BG, psuID=trtBG))
)

round(BW,4)

    # test with srs probs for PSUs. This matches BW2stageSRS reesult closely.
BW2stagePPS(MDarea.pop$y1, pp=rep(1/95,95), psuID=MDarea.pop$TRACT)
BW2stageSRS(MDarea.pop$y1, psuID=MDarea.pop$TRACT)
