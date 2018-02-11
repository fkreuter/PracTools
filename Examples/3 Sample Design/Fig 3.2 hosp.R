#***************************************************************************************************
# FILE:    C:\Projects\Practical Tools Book\Book Chapters\3 Sample Design\Examples\Fig 3.2 hosp.R                                                    
# PROJECT: Practical Tools book                                              
# DATE:    03/21/2012                                                        
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Plot sample of 10 from hospitals
#***************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\hospital.RData")

sam <- c(7, 17, 30, 33, 62, 111, 139, 247, 370, 393)

postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\3 Sample Design\\Examples\\Fig 3.2.eps")

plot(hospital[sam,"x"], hospital[sam,"y"],
     pch = 16,
     xlab = "x",
     ylab = "y")
grid(col = "gray70", 
     lty = 1)

dev.off()
