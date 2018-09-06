#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Stata Weighting Book\program files\3 Nonresponse\NR.adj.options.R
# Project:  Stata weighting book
# Date: 	11/08/2015
# Revised:
# Author:	R. Valliant
# Purpose:	Illustrate CART, random forests for NR adjustment; compare to logistic prediction
#--------------------------------------------------------------------------------------------------

set.seed(-1373438940)

require(PracTools)
require(rpart)
require(rpart.plot)
require(tree)
require(randomForest)
require(gbm)
data(nhis)
attach(nhis)

nhis$resp <- factor(nhis$resp)
nhis$hisp <- factor(nhis$hisp)
nhis$race <- factor(nhis$race)
nhis$parents_r <- factor(nhis$parents_r)
nhis$educ_r <- factor(nhis$educ_r)

train <- sample(1:nrow(nhis), nrow(nhis)/2)
nhis.train <- nhis[train,]
nhis.test <- nhis[-train,]
dim(nhis.test)
#[1] 1956   15
resp.test <- nhis$resp[-train]

#--------------------------------------------------------------------------------------------------
# logistic regression propensity classes
#--------------------------------------------------------------------------------------------------
require(PracTools)
data(nhis)
attach(nhis)

set.seed(-1373438940)
train <- sample(1:nrow(nhis), nrow(nhis)/2)
nhis.train <- nhis[train,]
nhis.test <- nhis[-train,]
resp.test <- nhis$resp[-train]

dim(nhis.train)
dim(nhis.test)

    # logistic reg on full nhis
mlogist <- glm(resp ~ age + factor(hisp) + factor(race) + factor(parents_r) + factor(educ_r),
                family = binomial(link = "logit"),
                data=nhis.train)
logist.pred <- predict(mlogist, newdata=nhis.test, type="response")
(tab <- table(logist.pred > 0.5, resp.test))
#       resp.test
#           0    1
#  FALSE    5    5
#  TRUE   614 1332
(tab[1,1] + tab[2,2])/sum(tab)

    # use pclass to create propensity classes
propen.nhis <- pclass(formula = resp ~ age + factor(hisp) + factor(race) + factor(parents_r) + factor(educ_r),
           data = nhis.test, type = "unwtd", link="logit", numcl=20)
propen.nhis <- pclass(formula = resp ~ age + factor(hisp) + factor(race) + factor(parents_r) + factor(educ_r),
           data = nhis.test, type = "unwtd", link="logit", numcl=5)

prop.pred <- propen.nhis$propensities > 0.5
(tab <- table(prop.pred, resp.test))
#         resp.test
#prop.pred    0    1
#    FALSE    9    6
#    TRUE   593 1348
(tab[1,1] + tab[2,2])/sum(tab)
#[1] 0.6937628

pclass.probs <- by(nhis.test$resp, propen.nhis$p.class, mean)
pcl.probs <- as.vector(pclass.probs)
names(pcl.probs) <- names(pclass.probs)

tmp1 <- data.frame(clnames=propen.nhis$p.class,propen.nhis$propensities)
tmp1 <- data.frame(ID=rownames(tmp1), tmp1)
head(tmp1)

tmp2 <- data.frame(pcl.probs)
tmp2 <- data.frame(clnames=rownames(tmp2), data.frame(pcl.probs))

tmp3 <- merge(tmp1, tmp2, by="clnames", sort=FALSE)
head(tmp3)
xx <- tmp3[order(tmp3$ID),]  # doesn't work

#--------------------------------------------------------------------------------------------------
#   Try to use tree as in Intro to Machine Learning book
#   This works but there appears to be no way to draw a nice picture of the tree
#--------------------------------------------------------------------------------------------------

    # predict resp using tree
tree.nhis <- tree(resp ~ age + hisp + race + parents_r + educ_r,
                    mindev = 0.001, minsize = 50,
                    data = nhis, subset=train)
tree.nhis
summary(tree.nhis)
#tree(formula = resp ~ age + hisp + race + parents_r + educ_r,
#    data = nhis, subset = train, mindev = 0.001, minsize = 50)
#Variables actually used in tree construction:
#[1] "educ_r"    "age"       "race"      "parents_r"
#Number of terminal nodes:  16
#Residual mean deviance:  1.199 = 2324 / 1939
# Misclassification error rate: 0.3105 = 607 / 1955

pr.nhis <- prune.tree(tree.nhis, method="misclass")
#$size
#[1] 16  9  5  1
#$dev
#[1] 607 607 608 610
#$k
#[1] -Inf 0.00 0.25 0.50
#$method
# [1] "misclass"
# attr(,"class")
# [1] "prune"         "tree.sequence"

    # plot size of tree vs. deviance
plot(pr.nhis$size, pr.nhis$dev, type="b")
pr.nhis <- prune.misclass(tree.nhis, best=9)
summary(pr.nhis)
plot(pr.nhis)
text(pr.nhis, pretty=3)
text(pr.nhis, all=TRUE,
     digits=4,
     cex=1,
     pretty=0,
     xpd = TRUE,
     font = 3)

nhis.pred <- predict(object = pr.nhis, newdata = nhis.test, type = "class")
(tab <- table(nhis.pred, resp.test))
#         resp.test
#nhis.pred    0    1
#        0   52  100
#        1  567 1237
(tab[1,1] + tab[2,2])/sum(tab)
#[1] 0.658998

#--------------------------------------------------------------------------------------------------
#   Back to rpart but use rpart.plot package to draw nice plot
#--------------------------------------------------------------------------------------------------
require(PracTools)
require(rpart)
require(rpart.plot)
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
apply(rpart.prob,2,mean)
#        0         1
#0.3098952 0.6901048    # mean(resp) = 0.69 also


    # color nodes that are majority NR dark red; nodes that are majority R green
#cols <- ifelse(t1$frame$yval == 1, "darkred", "green4")
cols <- ifelse(t1$frame$yval == 1, "gray50", "black")

pdf(file = "C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig137.pdf")
prp(t1, main="Tree for NR adjustment classes in NHIS",
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    branch=.5,           # change angle of branch lines
    faclen=0,            # do not abbreviate factor levels
    trace=1,             # print the automatically calculated cex
    shadow.col="gray",   # shadows under the leaves
    branch.lty=1,        # draw branches using solid lines
    branch.type=5,       # branch lines width = weight (frame$wt), no. of cases here
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col=cols, border.col=cols,   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)              # round the split box corners a tad
dev.off()
#--------------------------------------------------------------------------------------------------
# predict resp using randomForest
# randomForest apparently does not allow coercion to factor for independent vars
#--------------------------------------------------------------------------------------------------
require(PracTools)
require(randomForest)
data(nhis)
attach(nhis)

set.seed(-1373438940)
train <- sample(1:nrow(nhis), nrow(nhis)/2)
nhis.test <- nhis[-train,]

table(nhis$resp)/nrow(nhis)
#        0         1
#0.3098952 0.6901048


rf.nhis <- randomForest(as.factor(resp) ~ age + as.factor(hisp) + as.factor(race)
                        + as.factor(parents_r) + as.factor(educ_r),
                    importance = TRUE, na.action = na.omit, mtry=5,
                    ntree = 1000, classwt = c(0.31, 0.69),
                        # cycled through mtry =1,...,5; the lower mtry is, the worse are the predicted probs
                    data = nhis)
#                    data = nhis.train)
#                    data = nhis, subset=train)

rf.nhis
varImpPlot(rf.nhis)
rf.pred <- predict(object = rf.nhis, newdata = nhis.test, type = "class")
(tab <- table(rf.pred, resp.test))
#       resp.test
#rf.pred    0    1
#      0  227   87
#      1  392 1250

(tab[1,1] + tab[2,2])/sum(tab)
#[1]  0.7551125

mean((nhis.test$resp - as.numeric(rf.pred))^2)

    # use type = "prob" to get predicted probabilities of response
rfnhis.prob <- predict(object = rf.nhis, newdata = nhis.test, type = "prob")
rfnhis.prob <- predict(object = rf.nhis, newdata = nhis.train, type = "prob")
rfnhis.prob <- predict(object = rf.nhis, newdata = nhis, type = "prob")
apply(rfnhis.prob,2,mean)
    # this is with mtry=5 (all variables --> this is bagging)
#        0         1
#0.2182442 0.7817558

#**************************************************************************************************
    # make some dummies by hand to check that randomForest handles factors OK
hisp.f1 <- nhis$hisp == 1
hisp.f2 <- nhis$hisp == 2
race.f1 <- nhis$race == 1
race.f2 <- nhis$race == 2
race.f3 <- nhis$race == 3
par.f1 <- nhis$parents_r == 1
par.f2 <- nhis$parents_r == 2
educ.f1 <- nhis$educ_r == 1
educ.f2 <- nhis$educ_r == 2
educ.f3 <- nhis$educ_r == 3
educ.f4 <- nhis$educ_r == 4

rf.nhisA <- randomForest(as.factor(resp) ~ age + hisp.f2 +
                        race.f2 + race.f3 +
                        par.f2 +
                        educ.f2 + educ.f3 + educ.f4,
                    importance = TRUE, na.action = na.omit, mtry=5,
                    ntree = 500, classwt = c(0.31, 0.69),
                    data = nhis)
rf.nhisA
rfnhisA.prob <- predict(object = rf.nhis, newdata = nhis, type = "prob")
apply(rfnhisA.prob,2,mean)
#        0         1
#0.1120834 0.8879166        this is not close to mean(resp)
#**************************************************************************************************


    # plot randomForest preds vs. rpart preds
    # lowess line is not on 45 deg ???
plot(rpart.pred, rfnhis.prob)
lines(lowess(rpart.pred, rfnhis.prob))
abline(0,1,col="red")

plot(logist.pred, rfnhis.prob[-train,2])
lines(lowess(logist.pred, rfnhis.prob[-train,2]))
abline(0,1,col="red")

plot(rpart.prob[,2], rf.prob[,2])
lines(lowess(rpart.prob[,2], rnhis.prob[,2]))
abline(0,1,col="red")

rtree.nhis2 <- randomForest(as.factor(resp) ~ age + as.factor(hisp) + as.factor(race)
                        + as.factor(parents_r) + as.factor(educ_r),
                    importance = TRUE, na.action = na.omit, mtry=3,
                    data = nhis)
rnhis.prob2 <- predict(object = rtree.nhis2, newdata = nhis, type = "prob")
apply(rnhis.prob,2,mean)

#rtree.nhis
#summary(rtree.nhis)
#importance(rtree.nhis)
#                  0         1 MeanDecreaseAccuracy MeanDecreaseGini
#age       -5.101265 21.457684            17.508115        84.591005
#hisp      -6.720225 13.958053            10.058983         9.381160
#race      -1.026759 11.645916            10.039277        16.805746
#parents_r -7.961616 24.012577            21.314011         9.900282
#educ_r    -2.354374  8.799267             6.475369        22.822879

varImpPlot(rtree.nhis)
rnhis.pred <- predict(object = rtree.nhis, newdata = nhis.test, type = "class")
(tab <- table(rnhis.pred, resp.test))
#          resp.test
#rnhis.pred    0    1
#         0   14   20
#         1  605 1317
(tab[1,1] + tab[2,2])/sum(tab)
#[1] 0.6804703

#--------------------------------------------------------------------------------------------------
# Compare rpart and random forest on a test dataset
#--------------------------------------------------------------------------------------------------
data(kyphosis)
table(kyphosis[,1])/81
#   absent   present
#0.7901235 0.2098765

rpart.fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
rpart.prob <- predict(object = rpart.fit, newdata = kyphosis, type = "prob")
apply(rpart.prob,2,mean)
#   absent   present
#0.7901235 0.2098765

rf.fit <- randomForest(Kyphosis ~ Age + Number + Start, data = kyphosis)
rf.prob <- predict(object = rf.fit, newdata = kyphosis, type = "prob")
apply(rf.prob,2,mean)
#   absent   present
#0.7986914 0.2013086

    # on average predicted are about same but there are only 6 distinct values for rpart
plot(rpart.prob, rf.prob)
lines(lowess(rpart.prob, rf.prob))
abline(0,1,col="red")

#   ----> this comparison is fine but all x's are continous. What's wrong with nhis example?


#--------------------------------------------------------------------------------------------------
# predict resp using gradient boosting
#--------------------------------------------------------------------------------------------------

btree.nhis <- gbm(resp ~ age + hisp + race + parents_r + educ_r,
#                    distribution = "bernoulli",    # bernoulli produces nothing
                    distribution = "adaboost",
                    n.trees=5000, interaction.depth=3,
                    shrinkage=0.05, n.minobsinnode=50, bag.fraction=0.5,
                    train.fraction = 0.5,
#                    verbose = TRUE,
                    data = nhis[train,])
btree.nhis
summary(btree.nhis)
#                var  rel.inf
#age             age 31.16310
#race           race 26.98010
#educ_r       educ_r 23.03514
#hisp           hisp 14.73620
#parents_r parents_r  4.08546

bnhis.pred <- predict.gbm(object = btree.nhis, newdata = nhis.test, type = "response")

(tab <- table(bnhis.pred, resp.test))
#          resp.test
#bnhis.pred    0    1
#         1  619 1337
 1-619/( 619 + 1337)
#[1] 0.6835378      # So, this is no beeter than just using the original RR = 2699/(1212+2699) = 0.6901048
