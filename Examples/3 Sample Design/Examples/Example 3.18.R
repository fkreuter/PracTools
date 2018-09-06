######################################################################
# Program: Example 3.18.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    07/09/10
# Purpose: Produce results for Example 3.18
# Revised: 
######################################################################
                                                #Set working directory
rm(list=ls(all=TRUE))
setwd("//Jpsmnds/Users/Share/Practical tools/Book/Book Chapters/3 Sample Design/")
 
require(foreign)                                     #Load R libraries
require(pps)

set.seed(4297005)                    #Random seed for sample selection

smho98 <- read.xport("smho98.xpt")            #Load SAS transport file
dim(smho98)

                                      #Eliminate outpatient facilities
smho98 <- smho98[smho98$BEDS > 0,]
dim(smho98)

                           #Create 5-level stratum variable and verify
smho98$stratum5 <- 0.

smho98[( 1<=smho98$STRATUM & smho98$STRATUM<=2), "stratum5"] <- 1.
smho98[( 3<=smho98$STRATUM & smho98$STRATUM<=4), "stratum5"] <- 2.
smho98[( 5<=smho98$STRATUM & smho98$STRATUM<=8), "stratum5"] <- 3.
smho98[( 9<=smho98$STRATUM & smho98$STRATUM<=13),"stratum5"] <- 4.
smho98[(14<=smho98$STRATUM & smho98$STRATUM<=16),"stratum5"] <- 5.

table(smho98$stratum5)
                                                  #Create size measure
smho98$sqrt.Beds <- sqrt(smho98$BEDS)
summary(smho98$BEDS)
summary(smho98$sqrt.Beds)

                                    #Approx. proportional sample sizes
smp.size <- 50
(strat.cts <- as.numeric(table(smho98$stratum5)))
(strat.ps  <- strat.cts / sum(strat.cts))
sum(strat.ps)

(smp.size.h <- round(strat.ps * smp.size,0))
sum(smp.size.h)

                                    #Sort data file by sampling strata
smho98 <- smho98[order(smho98$stratum5),]

smp.IDs  <- ppssstrat(sizes = smho98$sqrt.Beds,
                      strat = smho98$stratum5,
                      n     = smp.size.h)

                                        #Identify duplicate selections
length(smp.IDs)
length(unique(smp.IDs))
                                            #Subset to sampled records
smp.data <- smho98[smp.IDs,]
table(smp.data$stratum5)
                                              #Verify selection counts
as.numeric(table(smp.data$stratum5)) - smp.size.h
