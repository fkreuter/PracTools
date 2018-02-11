######################################################################
# Program: Example 3.18.R
# Name:    J.Dever
# Project: Practical Tools for Designing and Weighting Survey Samples
# Date:    07/09/10
# Purpose: Select a stratied sample (stsrswor)
######################################################################

	#Load R libraries
require(foreign)
require(sampling)
	#Random seed for sample selection
set.seed(82841)
	#Load SAS transport file and examine

smho98 <- read.xport("smho98.xpt")
dim(smho98)
[1] 875 378

smho98[1:5,1:5]
  STRATUM BEDS EXPTOTAL SEENCNT EOYCNT
1       1   81  9066430    1791    184
2       1   80  9853392    1870    244
3       1   26  3906074    1273      0
4       1   90  9853392    1781    154
5       1   71  9853392    1839    206


	#Create 6-level stratum variable and verify
smho98$stratum6 <- 0
smho98[( 1<=smho98$STRATUM & smho98$STRATUM<=2), "stratum6"] <- 1
smho98[( 3<=smho98$STRATUM & smho98$STRATUM<=4), "stratum6"] <- 2
smho98[( 5<=smho98$STRATUM & smho98$STRATUM<=8), "stratum6"] <- 3
smho98[( 9<=smho98$STRATUM & smho98$STRATUM<=10),"stratum6"] <- 4
smho98[(11<=smho98$STRATUM & smho98$STRATUM<=13),"stratum6"] <- 5
smho98[(14<=smho98$STRATUM & smho98$STRATUM<=16),"stratum6"] <- 6

table(smho98$stratum6,smho98$STRATUM)
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
1 151 64 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2 0 0 43 22 0 0 0 0 0 0 0 0 0 0 0 0
3 0 0 0 0 150 23 65 14 0 0 0 0 0 0 0 0
4 0 0 0 0 0 0 0 0 38 12 0 0 0 0 0 0
5 0 0 0 0 0 0 0 0 0 0 13 77 59 0 0 0
6 0 0 0 0 0 0 0 0 0 0 0 0 0 86 39 19

 table(smho98$stratum6)
1 2 3 4 5 6
215 65 252 50 149 144

	#Select 10 units by srswor per stratum
smp.IDs <- strata(data = smho98,
		stratanames = "stratum6",
		size = rep(10,6),
		method = "srswor")

	#Pull sampled records and verify sample counts
sample1 <- getdata(smho98,smp.IDs)
table(sample1$stratum6)
1 2 3 4 5 6
10 10 10 10 10 10