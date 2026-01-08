library(tidyverse)
library(mice)
library(brms)

################################################################################
#Fit Baseline Multiple Linear Regression Model

setwd('~/DropBox/house_model/')

#Note: seed the imputation of missing data in the geography column
set.seed(1234)

ddhq_data <- read.csv('cleaned_data/ddhq_data.csv') 

#Rename variables to snake case
names(ddhq_data) <- gsub('[.]', '_', tolower(names(ddhq_data)))

#Dataset containing self-tabulated features: Gallup presidential approval from 
#the last survey fielded in October, BLS Q3 inflation and GDP growth rates, 
#incumbent party, and midterm versus general election indicator variables
fundamentals <- read.csv('cleaned_data/fundamentals.csv') |> 
  mutate(
    mean_approve = mean(gallup_pres_oct_approve),
    mean_inflation = mean(inflation_q3),
    mean_gdp = mean(gdp_q3),
    #Multiplying by the incumbent party will cause clustering, so we center predictor before multiplying
    gallup_pres_oct_approve_times_inc = inc_party*(gallup_pres_oct_approve - mean_approve),
    midterm_times_inc = inc_party*midterm, 
    inflation_q3_times_inc = inc_party*(inflation_q3 - mean_inflation),
    gdp_q3_times_inc = inc_party*(gdp_q3 - mean_gdp) ) |>
  select(-mean_approve, -mean_inflation, -mean_gdp) 
ddhq_data <- ddhq_data |> left_join(fundamentals, by = 'year')

ddhq_data <- ddhq_data |>
  mutate(r_d_victory_margin = as.double(r_d_victory_margin), #NOTE: converts '???' to NA, generates warning message
         congressional_district = factor(congressional_district)) 

#Imputation Strategy
#Set geography of districts in WY (62% Urban), MT (53%), SD (57%), ND (61%), AK (65%) 
#Source: 2020 Census
#In the remaining missing cases, perform polytomous imputation of ordered factor using pop sq mile variable
ddhq_data <- ddhq_data |>
  mutate(geography = case_when(
    #NOTE: make sure to impute for all missing cases, or there will be duplicate imputations
    #Program shares imputation between unique combinations of density and district. If 
    #an imputation already exists, the row will be duplicated
    is.na(geography) & state == 'WY' ~ 'Quite Rural', 
    is.na(geography) & state == 'SD' ~ 'Quite Rural',
    is.na(geography) & state == 'MT' ~ 'Quite Rural',
    is.na(geography) & state == 'ND' ~ 'Quite Rural',
    is.na(geography) & state == 'AK' ~ 'Quite Rural',
    .default = geography
  ))

ddhq_data$geography <- ordered(ddhq_data$geography, 
                               levels = c('Extremely Rural',
                                          'Quite Rural',
                                          'Quite Urban',
                                          'Extremely Urban'),
                               labels = c('Extremely Rural',
                                          'Quite Rural',
                                          'Quite Urban',
                                          'Extremely Urban'))

#Impute geography using polytomous regression of geography on population_square_mile
#First, create id for each unique combination of congressional district and population density
ddhq_data <- ddhq_data |> 
  group_by(population_square_mile, congressional_district, state) |>
  mutate(geography_id = cur_group_id()) |> 
  ungroup()

unique_geography <- ddhq_data |> distinct(
  geography,
  population_square_mile,
  congressional_district,
  state,
  geography_id)

imputed <- mice.impute.polr(unique_geography$geography, 
                            !is.na(unique_geography$geography),  
                            unique_geography |> select(population_square_mile), 
                            is.na(unique_geography$geography),
                            nnet.maxit = 100,
                            nnet.trace = FALSE,
                            nnet.MaxNWts = 1500,
                            polr.to.loggedEvents = T)

unique_geography[which(is.na(unique_geography$geography)), 'geography_impute'] <- imputed
ddhq_data <- ddhq_data |>
  left_join(unique_geography |> select(geography_impute, geography_id), 
            by = 'geography_id') |>
  mutate(geography = coalesce(geography, geography_impute)) |>
  select(-geography_id, -geography_impute)

#Drop uncontested races, will combine predictions later
ddhq_filter <- ddhq_data |> 
  filter(`unopposed_r_` == 0 & `unopposed_d_` == 0) |>
  #select(-`unopposed_r_`, -`unopposed_d_`, -`chamber`) |>
  select(-`chamber`) |>
  select(-gallup_pres_oct_disapprove, -gallup_pres_oct_unsure) |>
  rename('unopposed_r' = 'unopposed_r_',
         'unopposed_d' = 'unopposed_d_',
         'incumbent_running' = 'incumbent_running_') 
ddhq_filter <- ddhq_filter |>
  select(r_d_victory_margin, all_of(names(ddhq_filter)))

#ddhq_data |> select('congressional_district', 'state', 'year', 'geography_impute', 'geography', 'population_square_mile') |> View()

###################
#Fit baseline model
baseline_mod <- lm(r_d_victory_margin ~ . #add all predictors
                   -year 
                   -congressional_district +  #exclude congressional district due to factor issue
                     gallup_pres_oct_approve_times_inc + #add interactions for inc_party and structural factors
                     midterm_times_inc + 
                     gdp_q3_times_inc + 
                     inflation_q3_times_inc
                   -gallup_pres_oct_approve #we predict republican vote margin, 
                   -midterm                 #so we need to multiply fundamentals in direction of incumbency
                   -inc_party
                   -inflation_q3
                   -gdp_q3 ,
                   data = ddhq_filter |>
                     filter(year < 2018) |>
                     select(-race_id))
# 
# baseline_bayes_mod <- brm(formula = 
#     r_d_victory_margin ~ .
#     - year
#     - congressional_district
#     - race_id
#     - gallup_pres_oct_approve
#     - midterm
#     - inc_party
#     - inflation_q3
#     - gdp_q3,
#   family = gaussian(),
#   data = ddhq_filter |>
#     filter(year < 2018) |>
#     select(-race_id),
#   prior = c(prior(normal(0, 5), class = "b"),
#     prior(normal(0, 10), class = "Intercept"),
#     prior(student_t(3, 0, 5), class = "sigma")),
#   chains  = 1,
#   iter    = 2000,
#   warmup = 1000,
#   seed = 1234
# )

saveRDS(baseline_mod, '~/DropBox/house_model/models/baseline_model.rds')

ddhq_filter[which(ddhq_filter$year == 2018), 'r_d_victory_margin_pred'] <- predict(baseline_mod, 
                                                                                   newdata = ddhq_filter |> 
                                                                                     filter(year == 2018) |> 
                                                                                     select(-race_id))

#Add uncontested races
ddhq_data <- ddhq_data |>
  left_join(ddhq_filter |> select(race_id, r_d_victory_margin_pred), by = 'race_id') |>
  mutate(r_d_victory_margin_pred = case_when(
    year == 2018 & unopposed_r_ == 1 ~ 100,
    year == 2018 & unopposed_d_ == 1 ~ -100,
    .default = r_d_victory_margin_pred
  )) |> 
  mutate(r_d_victory_margin = coalesce(r_d_victory_margin, r_d_victory_margin_pred)) |>
  rename('district' = 'congressional_district')

write.csv(ddhq_data, 'cleaned_data/baseline_mod_predictions.csv') 
ddhq_data <- ddhq_data |> 
  select(-r_d_victory_margin_pred)

#Compare forecasted outcome to actual result of 235D-199R
ddhq_data |> filter(year == 2018) |> summarize(dem = sum(r_d_victory_margin <= 0),
                                               gop = sum(r_d_victory_margin > 0))

