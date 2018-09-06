#*********************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\14 Calibration\Examples\Fig 14.2.R
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples
# DATE:     03/24/2012
# AUTHOR:   R. Valliant
# PURPOSE:  Scatterplot matrix of smho.N874 variables
#*********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\smho.N874.RData")

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#    if(missing(cex.cor)) cex.cor <- 1
    text(0.5, 0.5, txt, cex = cex.cor)
}

delete <- smho.N874$hosp.type == 4
smho <- smho.N874[!delete, ]


postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\14 Calibration\\Examples\\Fig 14.2.eps")
           
pairs(smho[,1:4],
      lower.panel=panel.cor,
      cex.cor=2,
      cex.axis=1.4)

dev.off()
