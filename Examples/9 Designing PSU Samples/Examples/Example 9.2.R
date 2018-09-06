#*********************************************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples\Examples\Example 9.2.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     08/05/2011
# REVISED:  02/26/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Compute variance components for an srs/srs/ design
#           Revised to also use PSU and SSU variables for comparison.
#*********************************************************************************************************

# attach("C:\\Projects\\Practical Tools Book\\Data\\MDarea.pop.RData", pos=2)
data(MDarea.pop)
trtBG <- 10*MDarea.pop$TRACT + MDarea.pop$BLKGROUP

BW1 <- rbind(
            c(BW2stageSRS(abs(MDarea.pop$Hispanic-2), psuID=MDarea.pop$PSU),
              BW2stageSRS(abs(MDarea.pop$Hispanic-2), psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$Gender-1, psuID=MDarea.pop$PSU),
              BW2stageSRS(MDarea.pop$Gender-1, psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$Age, psuID=MDarea.pop$PSU),
              BW2stageSRS(MDarea.pop$Age, psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$y1, psuID=MDarea.pop$PSU),
              BW2stageSRS(MDarea.pop$y1, psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$y2, psuID=MDarea.pop$PSU),
              BW2stageSRS(MDarea.pop$y2, psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$y3, psuID=MDarea.pop$PSU),
            BW2stageSRS(MDarea.pop$y3, psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$ins.cov, psuID=MDarea.pop$PSU),
              BW2stageSRS(MDarea.pop$ins.cov, psuID=MDarea.pop$SSU)),

            c(BW2stageSRS(MDarea.pop$hosp.stay, psuID=MDarea.pop$PSU),
              BW2stageSRS(MDarea.pop$hosp.stay, psuID=MDarea.pop$SSU))
        )

BW2 <- rbind(
            c(BW2stageSRS(abs(MDarea.pop$Hispanic-2), psuID=MDarea.pop$TRACT),
              BW2stageSRS(abs(MDarea.pop$Hispanic-2), psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$Gender-1, psuID=MDarea.pop$TRACT),
              BW2stageSRS(MDarea.pop$Gender-1, psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$Age, psuID=MDarea.pop$TRACT),
              BW2stageSRS(MDarea.pop$Age, psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$y1, psuID=MDarea.pop$TRACT),
              BW2stageSRS(MDarea.pop$y1, psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$y2, psuID=MDarea.pop$TRACT),
              BW2stageSRS(MDarea.pop$y2, psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$y3, psuID=MDarea.pop$TRACT),
            BW2stageSRS(MDarea.pop$y3, psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$ins.cov, psuID=MDarea.pop$TRACT),
              BW2stageSRS(MDarea.pop$ins.cov, psuID=trtBG)),

            c(BW2stageSRS(MDarea.pop$hosp.stay, psuID=MDarea.pop$TRACT),
              BW2stageSRS(MDarea.pop$hosp.stay, psuID=trtBG))
        )

dimnames(BW1)[[1]] <-
dimnames(BW2)[[1]] <-
    c("Hispanic", "Gender", "Age",
                        "y1", "y2", "y3",
                        "ins.cov", "hosp.stay")
round(BW1,4)
round(BW2,4)
