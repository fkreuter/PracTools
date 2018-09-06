#****************************************************************************************
# FILE:     C:\Projects\Stata Weighting Book\Program files\18 Nonprobability samples\nnmatch.nhis.R
# PROJECT:  Practical Tools book, 2nd edition
# DATE:     10/03/2017
# AUTHOR:   R. Valliant
# PURPOSE:  Select subsamples of nhis.large for reference and nonprob samples to use in
#			nearest neighbor matching
#****************************************************************************************

require(PracTools)
require(sampling)
require(MatchIt)

data(nhis.large)

    # drop cases with missing values in vars to be used for nearest neighbor
keep.cols <- c("ID","stratum","psu","svywt","sex","age.grp","hisp","parents",
               "race","delay.med","doc.visit","medicaid","notcov")

drop.sw <- is.na(nhis.large$sex) | is.na(nhis.large$age.grp) | is.na(nhis.large$hisp) |
            is.na(nhis.large$parents) | is.na(nhis.large$race) | is.na(nhis.large$delay.med) |
            is.na(nhis.large$doc.visit) | is.na(nhis.large$medicaid) | is.na(nhis.large$notcov)

nhis.sub <- nhis.large[!drop.sw, keep.cols]
dim(nhis.sub)
#[1] 21056    13

sum(nhis.sub$svywt)
#[1] 65061755
table(nhis.sub$age.grp)
(pop.prop <- round(table(nhis.sub$age.grp)/sum(table(nhis.sub$age.grp)), 3))
#    1     2     3     4     5
#0.279 0.093 0.283 0.232 0.114

    # select a "nonprob" sample with age dist skewed  toward middle age groups
set.seed(246690786)
n <- 200
sam.prop <- c(0.1, 0.3, 0.3, 0.2, 0.1)

nh <- round(n*sam.prop, 0)

    # sort nhis.sub by age group
    # strata fcn requires population to be sorted by stratum variable
    # sampling package will not tell you if pop is not sorted!!
nhis.sub <- nhis.sub[order(nhis.sub$age.grp),]
sam <- strata(nhis.sub, stratanames="age.grp", size=nh, method="srswor")
np.nhis <- getdata(nhis.sub, sam)

(round(table(np.nhis$age.grp)/sum(table(np.nhis$age.grp)), 3))
rr <- rownames(np.nhis)

ref.nhis <- nhis.sub[!(rownames(nhis.sub) %in% rr), ]
dim(ref.nhis)
#[1] 20856     13

sum(ref.nhis$svywt)
#[1] 64406327
sum(nhis.sub$svywt)
#[1] 65061755

np.nhis$svywt <- 1
np.nhis$in.np <- 1
ref.nhis$in.np <- 0

keep.cols <- colnames(ref.nhis)
np.nhis <- np.nhis[, keep.cols]
combined <- rbind(np.nhis, ref.nhis)

m.sam <- matchit(in.np ~ sex + age.grp + hisp + race, data = combined, method = "nearest", subclass=10)
#m.sam <- matchit(in.np ~ age.grp, data = combined, method = "nearest")
    # m.sam$match.matrix has rownames of treatments, column gives rownames of controls
names(m.sam)

r <- m.sam$match.matrix
np.wts <- ref.nhis[rownames(ref.nhis) %in% r[,1], ]$svywt
length(np.wts)
#[1] 200

(N.hat <- sum(np.wts)*nrow(ref.nhis)/nrow(np.nhis))
#[1] 68362735
sum(ref.nhis$svywt)
#[1] 64406327
N.hat / sum(ref.nhis$svywt)
#[1] 1.061429

np.nhis <- cbind(np.nhis, np.wts)
head(np.nhis)

#--------------------------------------------------------------------------------------------------
    # load survey to avoid conflict between strata fcns in sampling package and in survival package
require(survey)
np.dsgn <- svydesign(ids = ~0, strata = NULL, weights = ~np.wts, data = np.nhis)
    # wtd age group distn of NP ssample
svymean(~as.factor(age.grp), design = np.dsgn)
#                        mean     SE
#as.factor(age.grp)1 0.090932 0.0208
#as.factor(age.grp)2 0.347234 0.0382
#as.factor(age.grp)3 0.277582 0.0340
#as.factor(age.grp)4 0.193092 0.0295
#as.factor(age.grp)5 0.091161 0.0211

    # proportion not covered by health insurance in NP sample
svymean(~as.factor(notcov), design = np.dsgn)
#                     mean    SE
#as.factor(notcov)1 0.1964 0.033
#as.factor(notcov)2 0.8036 0.033

    # unwtd mean of NP sample
mean(np.nhis$notcov==1)
#[1] 0.175
    # proportion of persons who delayed medical care because of cost
svymean(~as.factor(delay.med), design = np.dsgn)
#                          mean     SE
#as.factor(delay.med)1 0.077964 0.0208
#as.factor(delay.med)2 0.922036 0.0208

    # receive medicaid
svymean(~as.factor(medicaid), design = np.dsgn)
#                         mean     SE
#as.factor(medicaid)1 0.098519 0.0224
#as.factor(medicaid)2 0.901481 0.0224

svymean(~as.factor(doc.visit), design = np.dsgn)
#                         mean     SE
#as.factor(doc.visit)1 0.16968 0.0282
#as.factor(doc.visit)2 0.83032 0.0282

#--------------------------------------------------------------------------------------------------
#   estimated pop proportions for comparison
    # sample design based on nhis.sub
sdsgn <- svydesign(ids = ~0, strata=NULL, weights=~svywt, data=nhis.sub)
    # age distribution
svymean(~as.factor(age.grp), design=sdsgn)
#                       mean     SE
#as.factor(age.grp)1 0.25403 0.0032
#as.factor(age.grp)2 0.10010 0.0024
#as.factor(age.grp)3 0.28462 0.0034
#as.factor(age.grp)4 0.23926 0.0032
#as.factor(age.grp)5 0.12200 0.0025

    # proportion not covered by health insurance in nhis.sub
svymean(~as.factor(notcov), design = sdsgn)
#                     mean     SE
#as.factor(notcov)1 0.1474 0.0025
#as.factor(notcov)2 0.8526 0.0025

    # unwtd proportion in pop
mean(nhis.sub$notcov==1)
#[1] 0.1716375

    # proportion not covered by age grp
round(svyby(~as.factor(notcov), by = ~age.grp, design = sdsgn, FUN = svymean),4)
#  age.grp as.factor(notcov)1 as.factor(notcov)2 se.as.factor(notcov)1 se.as.factor(notcov)2
#1       1             0.0950             0.9050                0.0038                0.0038
#2       2             0.2969             0.7031                0.0111                0.0111
#3       3             0.2232             0.7768                0.0056                0.0056
#4       4             0.1199             0.8801                0.0048                0.0048
#5       5             0.0109             0.9891                0.0021                0.0021

svymean(~as.factor(delay.med), design = sdsgn)
#                          mean     SE
#as.factor(delay.med)1 0.072533 0.0019
#as.factor(delay.med)2 0.927467 0.0019
svymean(~as.factor(medicaid), design = sdsgn)
#                         mean    SE
#as.factor(medicaid)1 0.089237 0.002
#as.factor(medicaid)2 0.910763 0.002
svymean(~as.factor(doc.visit), design = sdsgn)
#                         mean     SE
#as.factor(doc.visit)1 0.16217 0.0028
#as.factor(doc.visit)2 0.83783 0.0028

#--------------------------------------------------------------------------------------------------
#   unweighted proportions from NP sample

prop.table(table(np.nhis$age.grp))
#  1   2   3   4   5
#0.1 0.3 0.3 0.2 0.1
mean(abs(np.nhis$notcov-2))
#[1] 0.175
mean(abs(np.nhis$delay.med-2))
#[1] 0.075
mean(abs(np.nhis$medicaid-2))
#[1] 0.1
mean(abs(np.nhis$doc.visit-2))
#[1] 0.18
