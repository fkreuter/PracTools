#--------------------------------------------------------------------------------------------------
# File: 	C:\Projects\Practical Tools Book\Book Chapters\18 Nonprobability sampling\Examples\Example 18.6 rstanarm.R
# Project:  Practical Tools book
# Date: 	10/18/2017
# Author:	R. Valliant
# Purpose:	Extract MI BRFSS cases that have Internet at home, take sample, and estimate means
#           using Bayesian prediction model
#           Same sample used as in Example 18.3.
#--------------------------------------------------------------------------------------------------

require(survey)
require(PracTools)
require(sampling)
require(dplyr)
require(rstanarm)
set.seed(-643998832)

attach("C:\\Projects\\Stata Weighting Book\\Data\\mibrfss\\mibrfss.RData")
    # bootstrap to larger pop
N <- 20000
bsam <- sample(1:nrow(mibrfss), N, replace=TRUE)
bpop <- mibrfss[bsam,]
    # select a sample from the internet cases
internet <- bpop[bpop$INETHOME==1,]
    # select a sample from the internet cases using AGECAT as strata. Rates are set so
    # that younger people are much more likely to be sampled.
nh <- c(24, 62, 37, 41, 27, 9)
    # sampling rates

internet <- internet[order(internet$AGECAT),]
table(internet$AGECAT)
sam <- strata(internet, stratanames="AGECAT", size=nh, method="srswor")
internet <- getdata(internet, sam)

    # recode good or better health
internet$GoB <- internet$GENHLTH <= 3
    # code covariates as factor
internet$AGECAT <- factor(internet$AGECAT)
internet$RACECAT <- factor(internet$RACECAT)
internet$EDCAT <- factor(internet$EDCAT)
internet$INCOMC3 <- factor(internet$INCOMC3)


#--------------------------------------------------------------------------------------------------
#   Append table cell ID to bpop & add cell summary stats to internet sample
    # compute pop totals from bootstrapped pop
cell_id <- with(bpop, paste0(AGECAT, RACECAT, EDCAT, INCOMC3))
bpop <- cbind(bpop, cell_id)

agg_pop <-
    xtabs(~AGECAT + RACECAT + EDCAT + INCOMC3, data=bpop) %>%
    as.data.frame() %>%
    rename(N = Freq) %>%
    mutate(
        cell_id = paste0(AGECAT, RACECAT, EDCAT, INCOMC3)
    ) %>%
    filter(cell_id %in% bpop$cell_id)
sum(agg_pop$N)
#[1] 20000


    # create object to send to stan_glmer
    # analysis var is GENHLTH. Si, et al. (2017) requires normally distributed y in order to get wts
    # GENHLTH is a 1-5 scale but, at least, is better than a 0-1 variable like SMOKE100
dat_rstanarm <- internet %>%
    mutate(
        cell_id = paste0(AGECAT, RACECAT, EDCAT, INCOMC3)
    ) %>%
    group_by(AGECAT, RACECAT, EDCAT, INCOMC3) %>%
    summarise(
        sd_cell = sd(GENHLTH),
        n = n(),
        mY = mean(GENHLTH),
        cell_id = first(cell_id)
    ) %>%
    mutate(sd_cell = if_else(is.na(sd_cell),0,sd_cell)) %>%
    left_join(agg_pop[, c("cell_id","N")], by = "cell_id")

dim(dat_rstanarm)
#[1] 89   9
print.data.frame(dat_rstanarm)


#--------------------------------------------------------------------------------------------------
#   rstanarm stan_glmer model
SEED <- 2081193216

    # default family is gaussian with identity link
fit <-
  stan_glmer(
    formula =
    mY ~ 1 + (1 | AGECAT) + (1 | RACECAT) +
                        (1 | EDCAT) + (1 | INCOMC3) +
                        (1 | AGECAT:RACECAT) + (1 | AGECAT:EDCAT) + (1 | AGECAT:INCOMC3) +
                        (1 | RACECAT:EDCAT) + (1 | RACECAT:INCOMC3) +
                        (1 | EDCAT:INCOMC3),
    data = dat_rstanarm,  iter = 5000, chains = 4,
    prior_covariance =
      rstanarm::mrp_structured(
        cell_size = dat_rstanarm$n,
        cell_sd = dat_rstanarm$sd_cell,
        group_level_scale = 1,
        group_level_df = 1
      ),
    seed = SEED,
    prior_aux = cauchy(0, 5),
    prior_intercept = normal(0, 100, autoscale = FALSE),
    adapt_delta = 0.99
  )

#--------------------------------------------------------------------------------------------------
#   compute cell weights
    # coercing obj to a matrix creates some cols that are not in names(obj). Don't understand
    # how this works, but there are cols called, e.g., "Sigma[AGECAT:INCOMC3:(Intercept),(Intercept)]"

model_based_cell_weights <- function(obj, cell_table){
    draws <- as.matrix(obj)
    Sigma <- draws[, grep("^Sigma\\[", colnames(draws)), drop = FALSE]
    sigma_theta_sq <- rowSums(Sigma)
    sigma_y_sq <- draws[, "sigma"]^2
    Ns <- cell_table[["N"]] # population cell counts
    ns <- cell_table[["n"]] # sample cell counts
    J <- nrow(cell_table)
    N <- sum(Ns)
    n <- sum(ns)
    Nsy2 <- N * sigma_y_sq
    ww <- matrix(NA, nrow = nrow(draws), ncol = J)
    for (j in 1:J) {
    ww[, j] <-
    (Nsy2 + n * Ns[j] * sigma_theta_sq) / (Nsy2 + N * ns[j] * sigma_theta_sq)
    }
return(ww)
}

cell_table <- fit$data[,c("N","n")]
wts <- model_based_cell_weights(fit, cell_table)

svywts <-
  data.frame(
    w = colMeans(wts),
    cell_id = fit$data[["cell_id"]],
    Y = fit$data[["mY"]],
    n = fit$data[["n"]]
  )

    # mean estimate
with(svywts, sum(w*Y / sum(w)))
#[1] 2.308798   GENHLTH

    # bpop mean
mean(bpop$GENHLTH)
#[1] 2.4825

sigma(fit)
#[1]  0.8676364
#  GENHLTH      # sigma(y) = sqrt(sigma(y)^2)

var.m <- sum(svywts$w * cell_table) * sigma(fit) / (sum(nh)*N)
sqrt(var.m)
#[1] 0.06086127
