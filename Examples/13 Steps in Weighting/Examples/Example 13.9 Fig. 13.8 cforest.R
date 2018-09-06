#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\13 Steps in Weighting\Examples\Example 13.9 Fig. 13.8 cforest.R
# Project:  Practical Tools book
# Date: 	06/27/2017
# Revised:
# Author:	R. Valliant
# Purpose:	Illustrate use of R party package, cforest function
*--------------------------------------------------------------------------------------------------

require(PracTools)
require(party)
    # use entire nhis pop with resp as the R/NR indicator
data(nhis)

crf.nhis <- cforest(as.factor(resp) ~ age + as.factor(hisp) +
                        as.factor(race) +
                        as.factor(parents_r) + as.factor(educ_r),
                        control = cforest_control(ntree = 500,
                        mincriterion = qnorm(0.8), trace = TRUE),
                        data=nhis)

crfnhis.prob <- predict(crf.nhis,newdata=nhis,type="prob")
crf.prob <- matrix(unlist(crfnhis.prob), ncol=2, byrow=TRUE)

apply(crf.prob,2,mean)
#[1]  0.2958413 0.7041587


#--------------------------------------------------------------------------------------------------
#   plot rpart predictions vs. cforest
#--------------------------------------------------------------------------------------------------
require(rpart)

data(nhis)
table(nhis$resp)/nrow(nhis)
#        0         1
#0.3098952 0.6901048

set.seed(15097)
            # run tree
t1 <- rpart(resp ~ age + as.factor(hisp) + as.factor(race) + as.factor(parents_r) + as.factor(educ_r),
      method = "class",
      control = rpart.control(minbucket = 50, cp=0),
      data = nhis)
print(t1, digits=4)
rpart.prob <- predict(object = t1, newdata = nhis, type = "prob")

pdf("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig138.pdf")
plot(rpart.prob[,2], crf.prob[,2],
    xlim = range(rpart.prob[,2], crf.prob[,2]),
    ylim = range(rpart.prob[,2], crf.prob[,2]),
    xlab = "rpart propensities",
    ylab = "cforest propensities",
    pch = 21, bg = "gray60", cex = 1.2)
abline(0,1)
dev.off()

reg1 <- glm(crf.prob[,2] ~ rpart.prob[,2])
summary(reg1)
#Call:
#glm(formula = crfprob[, 2] ~ rpart.prob[, 2])
#
#Deviance Residuals:
#     Min        1Q    Median        3Q       Max
#-0.39618  -0.09891   0.00363   0.08814   0.31075
#
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
#(Intercept)     0.008882   0.024596   0.361    0.718
#rpart.prob[, 2] 0.987060   0.035520  27.789   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.01606977)
#
#    Null deviance: 75.226  on 3910  degrees of freedom
#Residual deviance: 62.817  on 3909  degrees of freedom
#AIC: -5052.7

    # mean cforest propensities by rpart class
tab <- round(cbind(by(rpart.prob[,2], INDICES=t1$where, mean),
        by(crf.prob[,2], INDICES=t1$where, mean)),
        4)
colnames(tab) <- c("rpart", "cforest")
tab
   rpart cforest
3 0.5935  0.6279
6 0.4627  0.5689
7 0.6429  0.6423
8 0.6915  0.6970
9 0.7314  0.7425
