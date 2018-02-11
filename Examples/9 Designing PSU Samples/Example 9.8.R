#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples 
#               \Examples\Example 9.8.R                                              
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     03/08/2012                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Compute optimal value of number of PSUs, SSUs, and elements in 3-stage sampling.           
#**********************************************************************************

clusOpt3(unit.cost=c(500, 100, 120), 
         delta1=0.01, delta2=0.10, 
         unit.rv=1, 
         tot.cost=100000,
         cal.sw=1)

clusOpt3(unit.cost=c(500, 100, 120), 
         delta1=c(0.01,0.05,0.10), delta2=0.10, 
         unit.rv=2, 
         tot.cost=100000,
         cal.sw=1)   
