#***********************************************************************************************************
# FILE:     C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in
Weighting\\Examples
#               \\Example 13.6, Fig 13.4.R
# TOPIC:    Fit binary reg models to response in NHIS data set
# DATE:     04/03/2010
# AUTHOR:   R. Valliant
# REVISED   02/19/2012 Save figure 13.4 as postscript
#***********************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.RData", pos=2)
dim(nhis)

#---------------------------------------------------------------------------------
#  Unweighted and weighted response rates
#---------------------------------------------------------------------------------
mean(as.numeric(nhis[,"resp"]))
weighted.mean(as.numeric(nhis[,"resp"]), w=nhis[,"svywt"])
#---------------------------------------------------------------------------------
#  logistic model
#---------------------------------------------------------------------------------

glm.logit <- glm(resp ~ age +
           as.factor(sex) +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents) +
           as.factor(educ),
           family=binomial(link = "logit"),
           data = nhis)
summary(glm.logit)

# Results of logistic model:
#   sex non-sig
#   parents2, parents3 non-sig
#   educ[3], educ[9] non-sig

# Use collapsed cats of parents, educ

#---------Example 13.6------------------------------------------------------------
#---------------------------------------------------------------------------------
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
summary(glm.logit)

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

summary(glm.probit)
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

summary(glm.cloglog)
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
# plot boxplots and scatterplots of predicted probabilities
#   Fig 13.4
#---------------------------------------------------------------------------------

#postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig 13.4.eps")
pdf("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig134.pdf")

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(mar=c(4,4,1,1),
    mgp = c(2, 0.8, 0))

boxplot(t2 ~ t1, data = tmp,
    boxwex = 0.3,
#    col = "peachpuff")
    col = "gray90")

plot(pred.logit, pred.probit,
     pch = 16,
     cex = 0.6,
     xlab = "logistic predictions",
     ylab = "probit predictions")
#abline(0,1, col = "blue")
abline(0,1, col = "gray50", lwd=2)
grid(col = "grey40", lwd = 2)

plot(pred.logit, pred.cloglog,
     pch = 16,
     cex = 0.6,
     xlab = "logistic predictions",
     ylab = "c-log-log predictions")
#abline(0,1, col = "blue")
abline(0,1, col = "gray50", lwd=2)
grid(col = "black", lwd = 2)

dev.off()
