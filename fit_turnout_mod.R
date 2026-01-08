library(tidyverse)
library(brms)

setwd('~/DropBox/house_model/')

################################################################################
#Fit Bayesian Multi-Level Model to Predict Turnout Probability

pew <- read.csv('~/DropBox/house_model/cleaned_data/pew_cleaned.csv') |> 
  select(-matches('X', ignore.case = F))

# formula <- 'likely_voter ~ census_region + race + sex + age + party + income'
# turnout_mod <- glmer(as.formula(formula), data = pew, family = 'binomial')
# 
# print(turnout_mod)
# saveRDS(turnout_mod, '~/DropBox/house_model/models/likely_voter_base.rds')

formula <- 'likely_voter ~ (1|census_region) + (1|race) + sex + (1|age) + party + (1|income)'

turnout_mod <- brm(data = pew, 
                   family = bernoulli(link = "logit"),
                   formula = as.formula(formula), 
                   iter = 5000, warmup = 1000, cores = 4, chains = 4,
                   seed = 10,
                   control = list(adapt_delta = 0.9),
                   file = 'models/likely_voter_brms4')

print(turnout_mod)
