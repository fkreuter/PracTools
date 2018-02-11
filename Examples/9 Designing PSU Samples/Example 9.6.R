#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.6.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/05/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an ppswr/srs/srs design for a ratio estimate.
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)

trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
pp.PSU <- table(MDarea.pop$PSU) / nrow(MDarea.pop)
pp.trt <- table(MDarea.pop$TRACT) / nrow(MDarea.pop)

        # PSUs, SSUs
BW1 <- rbind(
  BW3stagePPS(X=MDarea.pop$y1, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$y2, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$y3, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$ins.cov, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$hosp.stay, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU)
)

round(BW1,4)
 
        # TRACTs, BGs
BW2 <- rbind(
  BW3stagePPS(X=MDarea.pop$y1, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$y2, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$y3, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$ins.cov, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$hosp.stay, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG)
)

round(BW2,4)
 
BW3stagePPS(X=MDarea.pop$Hispanic-1, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG)
BW3stagePPS(X=MDarea.pop$Gender-1, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG)
BW3stagePPS(X=MDarea.pop$Age, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG)
