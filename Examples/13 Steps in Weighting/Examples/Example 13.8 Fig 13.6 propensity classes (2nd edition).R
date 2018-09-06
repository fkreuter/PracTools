#**************************************************************************************************************
# FILE :   C:\Projects\Practical Tools Book\Book Chapters\13 Steps in Weighting\Examples\
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

require(survey)
require(PracTools)
data(nhis)

#-----------Example 13.8-----------------------------------------------------------------------
p.class <- pclass(formula = resp ~ age +
           as.factor(hisp) +
           as.factor(race) +
           as.factor(parents_r) +
           as.factor(educ_r),
           type = "unwtd", data = nhis, link="logit", numcl=5)

table(p.class$p.class, useNA="always")
#[0.453,0.631] (0.631,0.677] (0.677,0.714] (0.714,0.752] (0.752,0.818]          <NA>
          790           784           775           780           782             0
            # boxplot of propensites from logistic by class
            # Fig 13.6
#postscript("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig 13.6.eps")
pdf("C:\\Projects\\Practical Tools Book\\Book Chapters\\13 Steps in Weighting\\Examples\\Fig136.pdf")
par(mfrow = c(1,1))
boxplot(p.class$propensities ~ p.class$p.class,
        col = "gray90",
        boxwex = 0.25)
grid(col = "gray40", lwd = 2)
    # Redraw so that grid lines are behind boxes
boxplot(p.class$propensities ~ p.class$p.class,
        col = "gray90",
        boxwex = 0.25,
        add = TRUE)
dev.off()

#----------------------------------------------------------------------------------------------
#   Compare 5 ways of assigning class probs
#----------------------------------------------------------------------------------------------

                # (1) Unweighted avg response propensity
by(data = p.class$propensities, p.class$p.class, mean)

                # (2) Weighted response propensity
by(data = data.frame(preds = p.class$propensities, wt = nhis[,"svywt"]), p.class$p.class,
   function(x) {weighted.mean(x$preds, x$wt)}
)

                # (3) Unweighted response rate
by(as.numeric(nhis[, "resp"]), p.class$p.class, mean)

                # (4) Weighted response rate
by(data = data.frame(resp = as.numeric(nhis[,"resp"]), wt = nhis[,"svywt"]),
   p.class$p.class,
   function(x) {weighted.mean(x$resp, x$wt)}
)
                # (5) Median response propensity
by(p.class$propensities, p.class$p.class, median)


##################################################################################
### Check balance on covariates
##################################################################################

p.class <- p.class$p.class

chk1 <- glm(age ~ p.class + resp + p.class*resp, data = nhis)
summary(chk1)
chk2 <- glm(age ~ p.class, data = nhis)
anova(chk2, chk1, test="F")

new.hisp <- abs(nhis$hisp-2)
chk1 <- glm(new.hisp ~ p.class + resp + p.class*resp,
            family=quasibinomial(link = "logit"),
        data = nhis)
summary(chk1)
chk2 <- glm(new.hisp ~ p.class,
            family=quasibinomial(link = "logit"),
        data = nhis)
anova(chk2, chk1, test="Chisq")

                # Recode race to 2 cats
new.race <- nhis[, "race"]
new.race[new.race==3] <- 2
chk <- glm(new.race ~ p.class + resp + p.class*resp,
            family=quasibinomial(link = "logit"),
        data = nhis)
summary(chk)

            # There is a problem with this model. SAS says: "Quasi-complete separation
            #  of data points detected. MLE may not exist."
            # Running with epsilon = 1e-8 and 1e-12 shows that p.class(0.752,0.819]
            # is diverging.
new.par <- abs(nhis$parents_r-2)
chk <- glm(new.par ~ p.class + resp + p.class*resp,
            family=quasibinomial(link = "logit"),
        data = nhis)
summary(chk)
table(new.par, p.class)

chk <- glm(parents ~ p.class + resp + p.class*resp,
            family=quasibinomial(link = "logit"),
            control = glm.control(epsilon = 1e-8),
        data = nhis)
summary(chk)
chk <- glm(parents ~ p.class + resp + p.class*resp,
            family=quasibinomial(link = "logit"),
            control = glm.control(epsilon = 1e-12),
        data = nhis)
summary(chk)

table(p.class, parents)

                # educ has 4 categories
                # R recodes the last 3 together to create dichotomous var
chk <- glm(educ ~ p.class + RESP + p.class*RESP,
            family=quasibinomial(link = "logit"),
        data = nhis)
summary(chk)
