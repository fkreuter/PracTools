#***********************************************************************************************************
# FILE:     C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples
#               \\Example 13.7, Fig 13.5.R
# TOPIC:    Fit binary reg models to response in NHIS data set
# DATE:     04/03/2010
# AUTHOR:   R. Valliant
# REVISED   02/19/2012 Save figure 13.5 as postscript
#***********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.RData", pos=2)
#---------------------------------------------------------------------------------
#  unweigted logistic model for use in plotting
#---------------------------------------------------------------------------------

# Use collapsed cats of parents, educ

#---------------------------------------------------------------------------------
#   Repeat unwtd model in Example 13.6
#  Recoded levels
#   parents_r     1 = either mother, father, or both present
#                 2 = neither present
#   educ_r        1 = HS, GED, or less
#                 5 = some college
#                 6 = Bachelor's or AA degree
#                 9 = Master's, professional, doctoral
#---------------------------------------------------------------------------------

glm.logit <- glm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "logit"),
           data = nhis)

L.hat <- glm.logit$linear.predictors
                    # transform link values to probability scale
pred.logit <- exp(L.hat) / (1 + exp(L.hat) )

#---------------------------------------------------------------------------------
#  probit model
#---------------------------------------------------------------------------------

glm.probit <- glm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "probit"),
           data = nhis)

L.hat <- glm.probit$linear.predictors

pred.probit <- pnorm(L.hat)

#---------------------------------------------------------------------------------
#  c-log-log model
#---------------------------------------------------------------------------------

glm.cloglog <- glm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "cloglog"),
           data = nhis)

L.hat <- glm.cloglog$linear.predictors

pred.cloglog <- 1- exp(-exp(L.hat) )

t1 <- c( rep("logistic", length(pred.logit)),
     rep("probit", length(pred.probit)),
     rep("cloglog", length(pred.cloglog))
     )
t2 <- c(pred.logit,
    pred.probit,
    pred.cloglog)

tmp <- data.frame( I(t1), t2)

#---------------------------------------------------------------------------------
#  Use survey weights to do same regs
#---------------------------------------------------------------------------------

require(survey)
nhis.dsgn <- svydesign(ids = ~psu,
          strata = ~stratum,
          data = nhis,
          nest = TRUE,
          weights = ~svywt)

glm.logit <- svyglm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "logit"),
           design = nhis.dsgn)
summary(glm.logit)

L.hat <- glm.logit$linear.predictors
                    # transform link values to probability scale
dpred.logit <- exp(L.hat) / (1 + exp(L.hat) )

            # probit model
glm.probit <- svyglm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "probit"),
           design = nhis.dsgn)
summary(glm.probit)

L.hat <- glm.probit$linear.predictors
                    # transform link values to probability scale
dpred.probit <- pnorm(L.hat)

            # c-log-log model
glm.cloglog <- svyglm(resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           family=binomial(link = "cloglog"),
           design = nhis.dsgn)
summary(glm.cloglog)

L.hat <- glm.cloglog$linear.predictors
                    # transform link values to probability scale
dpred.cloglog <- 1- exp(-exp(L.hat) )

#------------------------------------------------------------------------------------
#   Fig 13.5
#------------------------------------------------------------------------------------
# postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig 13.5.eps",
#           width = 10,
#           height = 3)

par(mfrow = c(1,3),
    mar=c(4,4,1,1),
    mgp = c(2, 0.8, 0))

plot(pred.logit, dpred.logit,
     pch = 16,
     cex = 0.5,
     xlab = "Unweighted predicted probabilities",
     ylab = "Survey weighted predicted probabilities"
     )
title(main = "Logistic", line = -3)
abline(0,1, col = "gray50", lwd = 3)

plot(pred.probit, dpred.probit,
     pch = 16,
     cex = 0.5,
     xlab = "Unweighted predicted probabilities",
     ylab = "Survey weighted predicted probabilities"
     )
title(main = "Probit", line = -3)
abline(0,1, col = "gray50", lwd = 3)

plot(pred.cloglog, dpred.cloglog,
     pch = 16,
     cex = 0.5,
     xlab = "Unweighted predicted probabilities",
     ylab = "Survey weighted predicted probabilities"
     )
title(main = "Complementary log-log", line = -3)
abline(0,1, col = "gray50", lwd = 3)
#dev.off()

##################################################################################
###  Compare survey weighted predictions with each other
##################################################################################

par(mfrow = c(1,2),
    mar=c(4,4,1,1),
    mgp = c(2, 0.8, 0))

plot(dpred.logit, dpred.probit,
     pch = 16,
     cex = 0.5
     )
abline(0,1, col = "red", lwd = 1)

plot(dpred.logit, dpred.cloglog,
     pch = 16,
     cex = 0.5
     )
abline(0,1, col = "red", lwd = 1)
