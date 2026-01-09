library(tidyverse)
library(brms)
library(stringr)
#library(nnet)#for multinomial regression

################################################################################
#Compute Posterior Distribution of District-Level Vote Shares

#mem.maxVSize(1e9) #Avoid memory error
FILTER_COL <-'state_district'
N_DRAWS <- 1000#Default number of posterior draws in epred_mat is 4000 (set to 100 to save memory)
poststrat_file <- 'cleaned_data/poststrat_district_2016.csv'
vote_choice_model_file <- 'models/vote_brms3.rds'
likely_voter_model_file <- 'models/likely_voter_brms4.rds'
poststrat_df <- read.csv(poststrat_file) |> select(-matches('X', ignore.case = F))

#To override predictions in uncontested races
BASELINE_NO_CONTEST <- read.csv('cleaned_data/baseline_mod_predictions.csv')

################################################################################
# Get matrix of posterior predictions (rows = posterior samples, cols = areas)
mrp_posterior <- function(fit, poststrat, n_posterior_draws = 4000){
  # Get posterior estimates for each subgroup in poststrat table
  #if posterior_epred gives error 'function 'cholmod_factor_ldetA' not provided by', reinstall lme4: install.packages("lme4", type = "source")
  epred_mat <- posterior_epred(fit, newdata = poststrat, draws = n_posterior_draws)#default number of draws?
  return (epred_mat)
}

# Write predictions for each poststratification cell to a file in a directory 
# corresponding to the model
mrp_diagnostic <- function(poststrat, poststrat_file, model_file, index = FALSE, epred_mat){
  
  mrp_estimates_vector <- epred_mat %*% poststrat$n / sum(poststrat$n)
  mrp_estimate <- c(mean(mrp_estimates_vector), sd(mrp_estimates_vector))
  
  #browser()
  debug_info <- cbind(poststrat, colMeans(epred_mat), t(epred_mat))
  
  debug_dir <- paste0('debug/', gsub('.rds', '', model_file), index)
  if (index){ debug_dir <- paste0(debug_dir, index) }
  version <- strsplit(poststrat_file, '/')
  version <- gsub('.csv', '', version[[1]][length(version[[1]])])
  debug_file <- paste0(debug_dir, '/', version, '_epred.csv')
  
  if (!dir.exists(debug_dir)) { 
    dir.create(debug_dir, recursive = TRUE)
  }
  write.csv(debug_info, debug_file)
  
  return (debug_info)
}

#Function to create a data frame with one entry per area in post-stratification table
get_area_df <- function(poststrat){
  area_df <- data.frame(
    area = poststrat |> dplyr::rename(area = !!rlang::sym(FILTER_COL)) |> 
      select(area) |> unique(),
    mrp_estimate = NA,
    mrp_estimate_se = NA)
  area_df <- area_df |> mutate()
  
  return (area_df)
}

mrp_estimate <- function(poststrat, epred_mat){
  area_df <- get_area_df(poststrat)
  #create matrix to store posterior distribution for each area
  area_posterior_mat <- matrix(nrow = nrow(area_df), ncol = nrow(epred_mat)) 
  
  # Loop to populate the dataframe
  for(i in 1:nrow(area_df)) { 
    # Currently, the matrix epred_mat and the poststratification table contain 150,000
    # rows. We need to filter the ones that correspond to county in row i. We do so 
    # by defining the following condition:
    filtering_condition <- which(poststrat[ ,FILTER_COL] == area_df$area[i])
    state_epred_mat <- epred_mat[ ,filtering_condition]
    k_filtered <- poststrat[filtering_condition, ]$n
    
    tryCatch({
      mrp_estimates_vector_sub <- state_epred_mat %*% k_filtered / sum(k_filtered)
      area_posterior_mat[i, ] <- mrp_estimates_vector_sub
    }, error = function(msg){
      browser()
      return(NA)
    })
    
    # MRP estimate for state in row i 
    area_df$mrp_estimate[i] <- mean(mrp_estimates_vector_sub)
    area_df$mrp_estimate_se[i] <- sd(mrp_estimates_vector_sub)
  }
  
  #browser()
  #Return all posterior estimates
  area_df <- cbind(area_df, area_posterior_mat)
  
  return(area_df)
}

###################################
#(1) Compute district turnout probabilities (binomial/continuous response)
bin_cont_mrp <- function(posterior, poststrat_full){
  cat(paste0('Estimating MRP for binomial/continuous response...\n'))
  
  #Undo compression of post-stratification table
  posterior_expand <- left_join(poststrat_full, 
                                cbind(POSTSTRAT_UNIQUE, t(posterior)), 
                                by = predictors)
  posterior_expand <- posterior_expand |> 
    select(matches('[0-9]+')) |> 
    as.matrix() |>
    t()
  
  # mrp_diagnostic(poststrat_full = poststrat_full, 
  #                poststrat_file = poststrat_file, 
  #                model_file = model_file,
  #                epred_mat = posterior_expand)
  mrp <- mrp_estimate(poststrat = poststrat_full, epred_mat = posterior_expand)
  return(mrp)
}
  
###################################
#For polytomous response (voter choice and turnout-adjusted voter choice)
polytomous_mrp <- function(posterior, poststrat_full){
  if (!(length(dim(posterior)) > 2)){
    stop('Error: 2D posterior matrix does not appear to be polytomous.')
  }
  
  #3-D array to hold MRP posterior distribution by district by response category
  mrp_array <- array(NA, dim = c(get_area_df(poststrat_full) |> nrow(), 
                                 N_DRAWS, dim(posterior)[3]))
  
  for (i in 1:dim(posterior)[3]){
    cat(paste0('Estimating MRP for polytomous response level ', i, '...\n'))
    
    #Undo compression of post-stratification table
    posterior_expand <- left_join(poststrat_full, 
                                  cbind(POSTSTRAT_UNIQUE, t(posterior[,,i])), 
                                  by = predictors)
    posterior_expand <- posterior_expand |> 
      select(matches('[0-9]+')) |> 
      as.matrix() |>
      t()
    
    # mrp_diagnostic(poststrat_full = poststrat_full, 
    #                poststrat_file = poststrat_file, 
    #                index = i, 
    #                model_file = model_file, 
    #                epred_mat = posterior_expand)
  
    mrp <- mrp_estimate(poststrat = poststrat_full, epred_mat = posterior_expand)
    mrp_array[,,i] <- mrp |> select(matches('[0-9]+')) |> as.matrix()
    
  }
  
  #Get sum of each district/area posterior sample over 3 choices
  #Normalize probability of each choice so that they sum to 1
  totals <- mrp_array |> aperm() |> colSums()
  mrp_array <- sweep(mrp_array, c(1, 2), t(totals), `/`)
  
  r_d_margin <- mrp_array[,,2] - mrp_array[,,1]
  margin_table <- data.frame(area = mrp$area) 
  margin_table <- cbind(margin_table, r_d_margin)
  margin_table <- append_posterior_stats(margin_table)

  return(list('mrp_array' = mrp_array, 'margin_table' = margin_table))
}

#Override model predictions when district is uncontested
override_no_contest <- function(df){
  
  df <- df |>
  mutate(state = substring(area, 1, 2),
         district = substring(area, 4),
         year = 2018,
         district = as.integer(district)) |>
    select(area, state, district, year, all_of(names(df))) |>
    left_join(BASELINE_NO_CONTEST |> 
                select(unopposed_d_, unopposed_r_, state, district, year),
              by = c('state', 'district', 'year')) |>
    mutate(across(matches('[0-9]+'), ~ case_when(
      unopposed_d_ == 1 ~ -1,
      unopposed_r_ == 1 ~  1,
      TRUE              ~ .
    )))
  
  df <- df |>
    mutate(mean = df |> select(matches('[0-9]+')) |> rowMeans(),
           r_d_victory_margin_mrp = mean)
  
  return (df)
}

#Take dataframe of posterior draws for each district and add mean and sd of draws as column
append_posterior_stats <- function(df){
  df <- df |> 
    mutate(mean = rowMeans(df |> select(-area)),
           sd = apply(df |> select(-area), MARGIN = 1, sd)) |> 
    select(area, mean, sd, all_of(colnames(df)))
  
  return (df)
}

#Re-weight MRP estimates such that sum of vote shares across districts equal 
#national vote share projected by Pew Research
reweight_mrp <- function(mrp_array, margin_table){
  pew <- read.csv('cleaned_data/pew_cleaned.csv') |> 
    select(-matches('X', ignore.case = F))
  
  house_pv_target <- pew |> group_by(vote_choice) |> 
    reframe(voteshr = sum(weight_w), weight_w = weight_w) |>
    mutate(voteshr = voteshr/sum(weight_w)) |> 
    group_by(vote_choice) |> 
    reframe(voteshr = voteshr) |> 
    unique()
  
  mrp_dem_pv <- mrp_array[,,1] |> rowMeans() |> mean()
  mrp_gop_pv <- mrp_array[,,2] |> rowMeans() |> mean()
  mrp_unsure_pv <- mrp_array[,,3] |> rowMeans() |> mean()
  
  house_pv_target <- house_pv_target |> mutate(
    voteshr_mrp = c(mrp_dem_pv, mrp_gop_pv, mrp_unsure_pv),
    weight = voteshr / voteshr_mrp
  )
  
  mrp_array[,,1] <- mrp_array[,,1] * as.double(house_pv_target[1, 'weight'])
  mrp_array[,,2] <- mrp_array[,,2] * as.double(house_pv_target[2, 'weight'])
  mrp_array[,,3] <- mrp_array[,,3] * as.double(house_pv_target[3, 'weight'])

  r_d_margin <- mrp_array[,,2] - mrp_array[,,1]
  margin_table <- margin_table |> select(area) 
  margin_table <- cbind(margin_table, r_d_margin)
  margin_table <- append_posterior_stats(margin_table)
  
  return(list('mrp_array' = mrp_array, 'margin_table' = margin_table))
}

##################################
#Compute (1) district turnout probabilities, (2) district vote share estimates, 
#(3) district vote share estimates (weighted by cell-level turnout)


########################################################
#(1) Compute district turnout probabilities
likely_voter_fit <- readRDS(likely_voter_model_file)
predictors <- str_extract_all(as.character(likely_voter_fit$formula), '(?<= )[a-zA-Z_]*(?=[)]| )')[[1]]
#In order to speed up computation of posterior matrix, I compressed the post-stratification table 
#to contain unique combinations of predictor values, to be merged with full table later
POSTSTRAT_UNIQUE <- poststrat_df |> select(all_of(predictors)) |> unique()

likely_voter_posterior <- mrp_posterior(likely_voter_fit, POSTSTRAT_UNIQUE)[1:N_DRAWS, ]
likely_voter_mrp <- bin_cont_mrp(posterior = likely_voter_posterior, 
                                 poststrat_full = poststrat_df)


########################################################
#(2) District vote share estimates
vote_choice_fit <- readRDS(vote_choice_model_file)
predictors <- str_extract_all(as.character(likely_voter_fit$formula), '(?<= )[a-zA-Z_]*(?=[)]| )')[[1]]
#In order to speed up computation of posterior matrix, I compressed the post-stratification table 
#to contain unique combinations of predictor values, to be merged with full table later
POSTSTRAT_UNIQUE <- poststrat_df |> select(all_of(predictors)) |> unique()

vote_choice_posterior <- mrp_posterior(vote_choice_fit, POSTSTRAT_UNIQUE)[1:N_DRAWS, , ]
mrp_list <- polytomous_mrp(posterior = vote_choice_posterior,
                                  poststrat_full = poststrat_df)

vote_choice_array <- mrp_list$mrp_array
vote_margin <- mrp_list$margin_table
vote_margin <- override_no_contest(vote_margin)

#Re-weight MRP estimates such that sum of vote shares across districts equal 
#national vote share projected by Pew Research
mrp_list <- reweight_mrp(vote_choice_array, vote_margin)
vote_array_weighted <- mrp_list$mrp_array
vote_margin_weighted <- mrp_list$margin_table
vote_margin_weighted <- override_no_contest(vote_margin_weighted)

write.csv(vote_margin, 'cleaned_data/mrp_margin.csv')
write.csv(vote_margin_weighted, 'cleaned_data/mrp_margin_weight.csv')

########################################################
#(3) District vote share estimates (weighted by turnout) 
#element-wise posterior matrix multiplication of 2D matrix by each level of 3D matrix
adj_vote_choice_posterior <- sweep(vote_choice_posterior, c(1, 2), likely_voter_posterior, `*`)
mrp_list <- polytomous_mrp(posterior = adj_vote_choice_posterior,
                                      poststrat_full = poststrat_df)

adj_vote_choice_array <- mrp_list$mrp_array
adj_vote_margin <- mrp_list$margin_table
adj_vote_margin <- override_no_contest(adj_vote_margin)

#Re-weight MRP estimates such that sum of vote shares across districts equal 
#national vote share projected by Pew Research
mrp_list <- reweight_mrp(adj_vote_choice_array, adj_vote_margin)
adj_vote_array_weighted <- mrp_list$mrp_array
adj_vote_margin_weighted <- mrp_list$margin_table
adj_vote_margin_weighted <- override_no_contest(adj_vote_margin_weighted)

write.csv(adj_vote_margin, 'cleaned_data/mrp_margin_turnout.csv')
write.csv(adj_vote_margin_weighted, 'cleaned_data/mrp_margin_turnout_weight.csv')
