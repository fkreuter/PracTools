##################################################################
## Program: randomForest Example.R
## Name:    R. Valliant
## Project: SURV699E - A Practical Course in Sampling and Weighting
## Date:    10/17/2008
## Purpose: Example randomForest program to demonstrate regression tree
##          technique available in R.
## Revised: 
##################################################################

set.seed(71)
library(randomForest)

nhis.rf <- randomForest(RESP ~ AGE_P + ORIGIN_I + RACRECI2 + PARENTS + EDUC_R1, 
            data = nhis, 
            sampsize = 500,
            importance=TRUE, proximity=TRUE)
print(nhis.rf)

X <- c("AGE_P", "ORIGIN_I", "RACRECI2", "PARENTS", "EDUC_R1")
nhis.rf <- randomForest(x = nhis[, X], 
            y = nhis[, "RESP"],
            sampsize = 200,
            importance=TRUE, proximity=TRUE)
print(nhis.rf)

names(nhis.rf)

varImpPlot(nhis.rf)
getTree(nhis.rf, labelVar = TRUE)
treesize(nhis.rf)
varUsed(nhis.rf)

nhis.cl <- agnes(nhis.rf$proximity, metric = "manhattan", stand = TRUE)

Try dist and then hclust


nhis.clus <- hclust(as.dist(1 - nhis.rf$proximity))
sub1 <- cutree(nhis.clus, 5)
tmp <- table(nhis[,"RESP"],sub1)
tmp2 <- rbind(table(nhis[,"RESP"],sub1), apply(tmp,2,sum))
tmp2[2,]/tmp2[3,]
plot(sub1)

rm(sub1, tmp, tmp2)
