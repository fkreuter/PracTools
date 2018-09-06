#   Test statements from Example 18.6 rstanarm.R
#   Discarded

#--------------------------------------------------------------------------------------------------
#   rstanarm stan_glm model
SEED <- 2081193216

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
    # this works
fit1 <- stan_glm(formula = mY ~ AGECAT + RACECAT +
                        EDCAT + INCOMC3,
                    data = dat_rstanarm,
                    family = binomial(link = "logit"),
                    prior = t_prior, prior_intercept = t_prior,
                    chains = 2, seed = SEED, iter = 2000, algorithm = "sampling")

#fit2x <- stan_glm(formula = mY ~ AGECAT + RACECAT +
#                        EDCAT + INCOMC3 +
#                        AGECAT:RACECAT + AGECAT:EDCAT + AGECAT:INCOMC3 +
#                        RACECAT:EDCAT + RACECAT:INCOMC3 +
#                        EDCAT:INCOMC3,                                                ,
#                    data = dat_rstanarm,
#                    family = gaussian,
#                    family = binomial(link = "logit"),
#                    prior = t_prior, prior_intercept = t_prior,
#                    chains = 2, seed = SEED, iter = 2000, algorithm = "sampling")

    # 95% credible intervals on model parameters
round(posterior_interval(fit1, prob = 0.95), 3)
#round(posterior_interval(fit2, prob = 0.95), 3)



#--------------------------------------------------------------------------------------------------
#   rstanarm stan_glm model

fit.glm <-
  stan_glm(
    formula =
    mY ~ 1 + AGECAT + RACECAT + EDCAT + INCOMC3 +
             AGECAT:RACECAT + AGECAT:EDCAT + AGECAT:INCOMC3 +
             RACECAT:EDCAT + RACECAT:INCOMC3 + EDCAT:INCOMC3,
    data = dat_rstanarm,  iter = 1000, chains = 4,
#    prior_covariance =
#      rstanarm::mrp_structured(
#        cell_size = dat_rstanarm$n,
#        cell_sd = dat_rstanarm$sd_cell,
#        group_level_scale = 1,
#        group_level_df = 1
#      ),
    seed = SEED,
    prior_aux = cauchy(0, 5),
    prior_intercept = normal(0, 100, autoscale = FALSE),
    adapt_delta = 0.99
  )



#--------------------------------------------------------------------------------------------------
# try glmer to get cell predictions

require(lme4)
glmer.fit <- glmer(formula = GoB ~ (1|AGECAT) + (1|RACECAT) +
                        (1|EDCAT) + (1|INCOMC3),
                    data = internet,
                    family = binomial(link = "logit"))
ranef(glmer.fit)
p.hat <- predict(glmer.fit)/(1+predict(glmer.fit))
cell_id <- with(internet, paste0(AGECAT, RACECAT, EDCAT, INCOMC3))
internet <- cbind(internet, cell_id, p.hat)
internet[order(internet$cell_id),]
int1 <- internet %>%
            group_by(AGECAT, RACECAT, EDCAT, INCOMC3) %>%
            summarise(
                n = n(),
                mY = mean(p.hat),
                cell_id = first(cell_id)
    )

    # retrieve
L.pred <- predict(glmer.fit, newdata=agg_pop)
p.hat <- exp(L.pred) / (1 + exp(L.pred))
length(p.hat)
#[1] 273 no. of non-empty combinations cell combos

agg_pop1 <- cbind(agg_pop, p.hat)
    # PS estimate of proportion GoB
sum(agg_pop1$N * agg_pop1$p.hat)/ sum(agg_pop1$N)
#[1] 0.8793104
    # unwtd sample proportion
mean(internet$GoB)
#[1] 0.9183333
    # bpop mean
bpop$GoB <- bpop$GENHLTH <= 3
bpop$GoB <- as.factor(bpop$GoB)
mean(abs(as.numeric(bpop$GoB)-1))
#[1] 0.84345


Xmatrix <- model.matrix( ~ Gender + Age + Treatment, dtL.dataNA)
#### 2
predictions <- predict(m.gls, newdata = dtL.dataNA)
# same as Xmatrix %*% lme.1$coefficients$fixed
# same as df.Pred_swabs$predicted.1
#### 3
VCOV.beta <- vcov(m.gls)
#### 4
VCOV.predictions <- Xmatrix %*% VCOV.beta %*% t(Xmatrix)
