#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.5.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     12/27/2011
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an srs/srs/srs design.
#*********************************************************************************************************

#   attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)
require(PracTools)
data(MDarea.pop)

M <- length(unique(MDarea.pop$PSU))
pp.PSU <- rep(1/M,M)

M <- length(unique(MDarea.pop$TRACT))
trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
pp.trt <- rep(1/M,M)

BW1 <- rbind(
  BW3stagePPS(X=MDarea.pop$y1, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$y2, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$y3, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$ins.cov, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU),
  BW3stagePPS(X=MDarea.pop$hosp.stay, pp=pp.PSU, psuID=MDarea.pop$PSU, ssuID=MDarea.pop$SSU)
)

round(BW1,4)

M <- length(unique(MDarea.pop$TRACT))
trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP
pp.trt <- rep(1/M,M)

BW2 <- rbind(
  BW3stagePPS(X=MDarea.pop$y1, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$y2, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$y3, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$ins.cov, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDarea.pop$hosp.stay, pp=pp.trt, psuID=MDarea.pop$TRACT, ssuID=trtBG)
)

round(BW2,4)
