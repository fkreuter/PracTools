##################################################################
## Program: RPART Example.R
## Name:    J.Dever
## Project: SURV699E - A Practical Course in Sampling and Weighting
## Date:    10/23/07
## Purpose: Example rpart program to demonstrate regression tree
##          technique available in R.
## Revised: 
##################################################################
                                                  #Set working directory
rm(list=ls(all=TRUE))

                                                  #Load R libraries
require(MASS)
library(rpart) 
# library(datasets) 

##################################################################
## Description of Data
##################################################################
## From R help page: "A population of women who were at least 21
##   years old, of Pima Indian heritage and living near Phoenix, 
##   Arizona, was tested for diabetes according to WHO criteria. 
##   The data were collected by the US National Institute of 
##   Diabetes and Digestive and Kidney Diseases. We used the 532 
##   complete records after dropping the (mainly missing) data on
##   serum insulin." 
##
## Variables:
##    npreg	number of pregnancies 
##    glu	plasma glucose conc. in an oral tolerance test 
##    bp	diastolic blood pressure (mm Hg) 
##    skin	triceps skin fold thickness (mm) 
##    bmi	body mass index (weight in kg/(height in m)^2) 
##    ped	diabetes pedigree function 
##    age	age in years 
##    type	Yes or No, for diabetic according to WHO criteria 

##################################################################
## Examine analysis data file
##################################################################
names(Pima.tr)
names(Pima.te)
                                                  #Combine ds and examine
Diabetes <- rbind(Pima.tr, Pima.te)
Diabetes[c(1:5,450:458),]

summary(Diabetes)
table(Diabetes$npreg, exclude=NULL)

##################################################################
## Regression tree (default minimum node sizes)
##################################################################

                                                  #Model
Diabetes.tree1 <- rpart(type ~ npreg+glu+bp+skin+bmi+ped+age, 
                       data   =Diabetes, 
                       method ='class') 

                                                  #Plot regression tree
plot(Diabetes.tree1, uniform=TRUE, , cex = 0.8,
     main="Regression Tree for WHO Diabetes Study (n=532) - Default Node Size") 
text(Diabetes.tree1) 
                                                  #Details of results
# summary(Diabetes.tree1)
                                                  #Number and size of nodes
length(unique(Diabetes.tree1$where))
table(Diabetes.tree1$where)

##################################################################
## Regression tree (minimum node size = 30)
##################################################################

                                                  #Model
Diabetes.tree2 <- rpart(type ~ npreg+glu+bp+skin+bmi+ped+age, 
                       data   =Diabetes, 
                       method ='class', 
                       control=rpart.control(minbucket=30)) 

                                                  #Plot regression tree
x11()
plot(Diabetes.tree2, uniform=TRUE, cex = 0.8,
     main="Regression Tree for WHO Diabetes Study (n=532) - Node Size >= 30") 
text(Diabetes.tree2) 
                                                  #Details of results
summary(Diabetes.tree2)
                                                  #Number and size of nodes
length(unique(Diabetes.tree2$where))
table(Diabetes.tree2$where)

##################################################################
