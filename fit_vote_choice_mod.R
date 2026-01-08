library(tidyverse)
library(brms)

################################################################################
#Fit Bayesian Multi-Level Model to Predict Vote Choice

pew <- read.csv('~/DropBox/house_model/cleaned_data/pew_cleaned.csv') |> 
  select(-matches('X', ignore.case = F))

# formula <- 'vote_choice ~ census_region + race + sex + age + party + income'
# vote_mod <- multinom(as.formula(formula), data = pew)
# 
# print(vote_mod)
# saveRDS(vote_mod, '~/DropBox/house_model/models/vote_base.rds')

formula <- 'vote_choice ~ (1|census_region) + (1|race) + sex + (1|age) + party + (1|income)'

vote_mod <- brm(data = pew, 
                family = categorical(link = logit, refcat = 'Democrat'),#Verify that refcat = 1 sets the reference category to Democrat
                formula = as.formula(formula), 
                iter = 2000, warmup = 1000, cores = 4, chains = 4,
                seed = 11,
                control = list(adapt_delta = 0.9),
                file = 'models/vote_brms3')

print(vote_mod)