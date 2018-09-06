#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Fig 14.4.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Plot studentized residuals vs. beds
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.RData")

delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]

# Separate slope on beds in each hosp type
m2 <- glm(EXPTOTAL ~ SEENCNT + EOYCNT + 
    as.factor(FINDIRCT) +
    as.factor(hosp.type):BEDS,
          data = smho
          )
summary(m2)

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig 14.4.eps")

par(mfrow=c(1,1))
plot(smho$BEDS, rstudent(m2),
     pch = 21,
     cex = 1.4,
     cex.lab = 1.3,
     cex.axis = 1.3,
     #     bg = c("red", "green3", "blue", "orange", "yellow")[unclass(smho$hosp.type)],
     bg = c("gray0", "gray40", "gray80", "gray80", "gray100")[unclass(smho$hosp.type)],
     xlab = "Beds",
     ylab = "Studentized residuals"
     )
abline(h = c(-3,3))
legend("topright", 
       legend = hosp.labs[-4], 
       #          pt.bg = c("red", "green3", "blue", "yellow"),
       pt.bg = c("gray0", "gray40", "gray80", "gray100"),
       pch=21,
       cex = 1.2
       )

dev.off()