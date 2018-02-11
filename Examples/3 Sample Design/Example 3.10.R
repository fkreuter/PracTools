#****************************************************************************
# FILE:    Example 3.10.R                                                    
# PROJECT: Practical Tools book                                              
# DATE:    08/19/2012                                                        
# AUTHOR:  R. Valliant                                                       
# PURPOSE: Stratum allocations for comapring stratum means
#****************************************************************************


Nh <- c(215, 65, 252, 50, 149, 144)
Sh <- c(26787207, 10645109, 6909676, 11085034, 9817762, 44553355)

#	minimize average variance of the difference between all H (H-1)/2 pairs of strata
Sh / sum(Sh)

#	variance of difference in any two stratum means be the same	
Sh^2 / sum(Sh^2)
