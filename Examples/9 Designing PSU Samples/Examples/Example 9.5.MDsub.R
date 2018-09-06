#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.5.MDsub.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     01/02/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an srs/srs/srs design. Use a subset of MDarea.pop 
#           that omits 3 small tracts and the one largest tract.
#*********************************************************************************************************

pick <- MDarea.pop$TRACT %in% c(741100,702800,750600,702202)
MDsub <- MDarea.pop[!pick,]
dim(MDsub)

M <- length(unique(MDsub$TRACT))
trtBG <- 10*MDsub$TRACT + MDsub$BLKGROUP
pp.trt <- rep(1/M,M)

BW <- rbind(
  BW3stagePPS(X=MDsub$y1, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$y2, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$y3, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$ins.cov, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$hosp.stay, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG)
)

round(BW,4)
#           B       W     W2      W3 delta1 delta2
# [1,] 0.1832  1.7203 0.2761  1.9933 0.0961 0.1211
# [2,] 0.1835  1.1969 0.2662  1.3789 0.1327 0.1613
# [3,] 0.1835  0.1192 0.2648  0.1273 0.6062 0.6754
# [4,] 0.1878  0.3055 0.2633  0.3530 0.3806 0.4270
# [5,] 0.2296 15.3690 0.3236 17.7434 0.0145 0.0172

pp.trt <- table(MDsub$TRACT) / nrow(MDsub)

BW <- rbind(
  BW3stagePPS(X=MDsub$y1, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$y2, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$y3, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$ins.cov, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG),
  BW3stagePPS(X=MDsub$hosp.stay, pp=pp.trt, psuID=MDsub$TRACT, ssuID=trtBG)
)

round(BW,4)
#           B       W     W2      W3 delta1 delta2
# [1,] 0.0095  1.4507 0.2583  1.6912 0.0062 0.1319
# [2,] 0.0110  1.0099 0.2461  1.1717 0.0106 0.1731
# [3,] 0.0139  0.1008 0.2456  0.1086 0.1211 0.6934
# [4,] 0.0019  0.2587 0.2394  0.3017 0.0069 0.4422
# [5,] 0.0231 12.8902 0.2816 14.9728 0.0016 0.0177