#**********************************************************************************
# FILE:     C:\Projects\Practical Tools Book\Book Chapters\9 Designing PSU Samples 
#               \Examples\Example 9.7.R                                              
# PROJECT:  Practical Tools for Designing and Weighting Survey Samples             
# DATE:     03/08/2012                                                             
# PGMR:     R. Valliant                                                            
# PURPOSE:  Compute optimal value of number of elements, n.opt, optimal            
#           value of number of sample PSUs, and CVs in 2-stage sampling.           
#**********************************************************************************

clusOpt2(C1=750, C2=100, 
         delta=0.05, 
         unit.rv=1, 
         tot.cost=100000, 
         cal.sw=1)

clusOpt2(C1=750, C2=100, 
         delta=c(0.01, 0.05, 0.10, 0.20), 
         unit.rv=1, 
         tot.cost=100000, 
         cal.sw=1)
