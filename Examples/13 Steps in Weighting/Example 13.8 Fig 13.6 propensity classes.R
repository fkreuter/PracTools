#**************************************************************************************************************
# FILE :   C:\Projects\Practical Tools Book\Book Chapters\14 Steps in Weighting\Examples\
#			Example 13.8 Fig 13.6 propensity.classes.R
# PROJECT: Practical Tools for Designing and Weighting Survey Samples
# AUTHOR:  R. Valliant
# PURPOSE: Predict response propensities with logistic regn.
#           Form 5 classes and check on balance within classes
# DATE:     04/04/10
# PGMR:     R. Valliant
# REVISED:  11/19/2010
#           Revised code to use 3,911 record NHIS file
#**************************************************************************************************************

attach("C:\\Projects\\Practical Tools Book\\Data\\nhis.RData", pos=2)
#attach("I:\\Projects\\Practical Tools\\Data\\nhis.RData",pos=2)

    # Note that regressions in "Example 13.6, Fig 13.4.R" and "Example 13.7 Fig 13.5.R" must be run
    # to create pred.logit, etc before running code below

#-----------Example 13.8-----------------------------------------------------------------------
            # Determine quintiles of response propensities
quintiles <- quantile(pred.logit, probs = seq(0,1,0.2))
            # Create a factor to hold the class IDs
            # include.lowest=TRUE makes sure the smallest propensity
            #    is assigned to a class
p.class <- cut(round(pred.logit,3), breaks = quintiles,
            include.lowest=TRUE)

table(p.class, useNA="always")

            # boxplot of propensites from logistic by class
            # Fig 13.6
postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig 13.6.eps")
par(mfrow = c(1,1))
boxplot(pred.logit ~ p.class,
        col = "gray90",
        boxwex = 0.25)
grid(col = "gray50")
dev.off()


#p.class
#(0.453,0.631] (0.631,0.677] (0.677,0.714] (0.714,0.752] (0.752,0.818]
#          778           773           788           786           786

#----------------------------------------------------------------------------------------------
#   Compare 5 ways of assigning class probs
#----------------------------------------------------------------------------------------------

                # (1) Unweighted avg response propensity
by(data = pred.logit, p.class, mean)

                # (2) Weighted response propensity
by(data = data.frame(pred.logit, wt = nhis[,"svywt"]), p.class,
   function(x) {weighted.mean(x$pred.logit, x$wt)}
)

                # (3) Unweighted response rate
by(as.numeric(nhis[, "resp"]), p.class, mean)

                # (4) Weighted response rate
by(data = data.frame(resp = as.numeric(nhis[,"resp"]), wt = nhis[,"svywt"]),
   p.class,
   function(x) {weighted.mean(x$resp, x$wt)}
)
                # (5) Median response propensity
by(pred.logit, p.class, median)


# Stmts below work but there are many strata with 1 PSU
#require(survey)
#nhis.dsgn1 <- svydesign(ids = ~PSU,
#          strata = ~STRATUM,
#          data = data.frame(nhis, pred.logit, p.class),
#          nest = TRUE,
#          weights = ~WTFA)
#
#svyby(~pred.logit, by = p.class, design = nhis.dsgn1, FUN = svymean,
#      drop.empty.groups = TRUE)

##################################################################################
### Check balance on covariates
##################################################################################

chk1 <- glm(age ~ p.class + resp + p.class*resp, data = nhis)
summary(chk1)
chk2 <- glm(age ~ p.class, data = nhis)
anova(chk2, chk1, test="F")


new.hisp <- abs(nhis$hisp-2)
chk1 <- glm(new.hisp ~ p.class + resp + p.class*resp,
            family=binomial(link = "logit"),
        data = nhis)
summary(chk1)
chk2 <- glm(new.hisp ~ p.class,
            family=binomial(link = "logit"),
        data = nhis)
anova(chk2, chk1, test="Chisq")

                # Recode race to 2 cats
new.race <- nhis[, "race"]
new.race[new.race==3] <- 2
table(new.race)
chk <- glm(new.race-1 ~ p.class + resp + p.class*resp,
            family=binomial(link = "logit"),
        data = nhis)
summary(chk)

            # There is a problem with this model. SAS says: "Quasi-complete separation
            #  of data points detected. MLE may not exist."
            # Running with epsilon = 1e-8 and 1e-12 shows that p.class(0.752,0.819]
            # is diverging.
new.par <- abs(nhis$parents_r-2)
chk <- glm(new.par ~ p.class + resp + p.class*resp,
            family=binomial(link = "logit"),
        data = nhis)
summary(chk)
table(new.par, p.class)

chk <- glm(parents ~ p.class + resp + p.class*resp,
            family=binomial(link = "logit"),
            control = glm.control(epsilon = 1e-8),
        data = nhis)
summary(chk)
chk <- glm(parents ~ p.class + resp + p.class*resp,
            family=binomial(link = "logit"),
            control = glm.control(epsilon = 1e-12),
        data = nhis)
summary(chk)

table(p.class, parents)

                # educ has 4 categories
                # R recodes the last 3 together to create dichotomous var
chk <- glm(educ ~ p.class + RESP + p.class*RESP,
            family=binomial(link = "logit"),
        data = nhis)
summary(chk)
