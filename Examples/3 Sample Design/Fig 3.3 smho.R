#***************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\3 Sample Design\Examples\Fig 3.3 smho.R                                                    
# PROJECT: Practical Tools book                                              
# DATE:    03/21/2012                                                        
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Plot SMHO hospitals for ones with nonzero beds
#***************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\smho98.RData")

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\3 Sample Design\\Examples\\Fig 3.3.eps")

tmp <- smho98[order(smho98[, "BEDS"]),]
tmp <- tmp[tmp[, "BEDS"] > 0, ]
plot(tmp[,"BEDS"], tmp[,"EXPTOTAL"]/1000000,
     pch = 16,
     xlab = "Beds",
     ylab = "Total expenditures (millions)")

grid(col = "gray70", 
     lty = 1)

lines(lowess(tmp[, "BEDS"], tmp[, "EXPTOTAL"]/10^6, 
      f = 0.25),
      col = "gray50",
      lwd = 2)
arrows(x0 = 2250, y0 = 475,
       x1 = 2340, y1 = 500,
       length = 0.125,
       lwd = 4,
#       col = "cadetblue2")
       col = "gray70")
  
dev.off()
