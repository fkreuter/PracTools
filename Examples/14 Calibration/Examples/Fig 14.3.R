#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Fig 14.3.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Plot of EXPTOTAL vs. BEDS for 4 hospital types
#*********************************************************************************************************


require(survey)
require(PracTools)

data(smho.N874)
delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]
dim(smho)

# plots by hospital type
#postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig 14.3.eps")
pdf("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig143.pdf")

par(mfrow=c(2,2),
    mar = c(4,4,2,1),
    mgp = c(2,1,0)
    )
hosp.labs <- c("psychiatric",
               "residential or veterans",
               "general",
               "partial care, outpatient",
               "multi-service, substance abuse"
               )

for (h in c(1,2,3,5)){
    pick <- smho$hosp.type == h
    x <- smho[pick, "BEDS"]
    y <- smho[pick, "EXPTOTAL"]/(10^6)
    if (h==1) {ylabel <- "Expenditures (millions)"}
    else {ylabel <- ""}
    plot(x, y ,pch = 16,
         ylim = range(smho[, "EXPTOTAL"]/(10^6)),
         xlab = "Beds",
         ylab = ylabel,
         cex.axis = 1.3,
         cex.lab = 1.3)
    grid(col = "grey40", lwd = 2)
    lines(lowess(x,y), col="gray70", lwd=2)
    title(main = hosp.labs[h])
}

dev.off()
