library(tidyverse)
library(ggplot2)
library(stringr)
library(xtable)
library(patchwork)
library(stargazer)

setwd('~/DropBox/house_model/')

ENSEMBLE_WT_MRP <- 0.33
ENSEMBLE_WT_BASELINE <- 0.67

#Read MIT Data Lab congressional election returns for 2018 to assess models
house <- read.csv('cleaned_data/1976-2024-house.tab') |>
  select(-state) |>
  rename('state' = 'state_po') |>
  #filter(year == 2018) |>
  mutate(district = as.integer(district)) |>
  filter(party == 'DEMOCRAT' | party == 'REPUBLICAN') |>
  mutate(rvote = case_when(
    party == 'REPUBLICAN' ~ candidatevotes,
    .default = 0
  ),
  dvote = case_when(
    party == 'DEMOCRAT' ~ candidatevotes,
    .default = 0
  )) |>
  group_by(district, state, year) |> 
  summarize(dvoteshr = sum(dvote)/sum(dvote + rvote),
            rvoteshr = sum(rvote)/sum(dvote + rvote)) |>
  mutate(
    r_d_victory_margin_mit = rvoteshr - dvoteshr
  )
  
baseline_pred <- read.csv('cleaned_data/baseline_mod_predictions.csv') |>
  mutate(r_d_victory_margin_baseline = r_d_victory_margin_pred/100)
  
#Check that mit victory margins agree with DDHQ's
# x <- left_join(baseline_pred, house, by = c('state','year', 'district')) |> 
#   select(r_d_victory_margin, r_d_victory_margin_mit, state, year, district)
# plot(x$r_d_victory_margin, x$r_d_victory_margin_mit)

mrp <- read.csv('cleaned_data/mrp_margin_turnout_weight.csv') |>
  mutate(r_d_victory_margin_mrp = mean) |> 
  mutate(district = case_when(
    state %in% c('AK', 'DE', 'MT', 'ND', 'SD', 'VT', 'WY') ~ 0,
    .default = district
  ))

combined <- baseline_pred |> 
  mutate(r_d_victory_margin = case_when(
    year == 2018 ~ NA,
    .default = r_d_victory_margin
  )) |>
  left_join(mrp |> select(state, district, year, r_d_victory_margin_mrp, matches('[^q][0-9]+')), 
            by = c('state', 'district', 'year')) |>
  left_join(house |> select(state, district, year, r_d_victory_margin_mit),
            by = c('state', 'district', 'year')) |>
  mutate(r_d_victory_margin = case_when(
    year == 2018 ~ r_d_victory_margin_mit,
    .default = r_d_victory_margin
  )) |> 
  mutate(r_d_victory_margin_ensemble = 
           as.double(r_d_victory_margin_mrp)*ENSEMBLE_WT_MRP + 
           as.double(r_d_victory_margin_baseline)*ENSEMBLE_WT_BASELINE) |>
  select(state, district, year, 
         r_d_victory_margin,
         r_d_victory_margin_baseline, 
         r_d_victory_margin_mrp,
         r_d_victory_margin_ensemble,
         matches('[0-9]+')
         ) |> 
  filter(year == 2018)

################################################################################
#Figure: Visualize correlation between 3 models' estimates and outcome

get_model_names <- function(vec){
  result <- case_when( 
    vec == 'r_d_victory_margin_mrp' ~ 'MRP',
    vec == 'r_d_victory_margin_baseline' ~ 'Baseline',
    vec =='r_d_victory_margin_ensemble' ~ 'Ensemble'
  )
  return (result)
}

cor_mrp <- cor(combined$r_d_victory_margin, combined$r_d_victory_margin_mrp, use = 'complete.obs')
cor_baseline <- cor(combined$r_d_victory_margin, combined$r_d_victory_margin_baseline, use = 'complete.obs')
cor_ensemble <- cor(combined$r_d_victory_margin, combined$r_d_victory_margin_ensemble, use = 'complete.obs')
cat(paste0('Corr of mrp model predictions and vote share: ', format(cor_mrp, digits = 4)))
cat(paste0('Corr of baseline model predictions and vote share: ', format(cor_baseline, digits = 4)))
cat(paste0('Corr of ensemble model predictions and vote share: ', format(cor_ensemble, digits = 4)))

cor_data <- data.frame(correlation = c(cor_mrp, cor_baseline, cor_ensemble),
                       model = c('r_d_victory_margin_mrp', 
                                 'r_d_victory_margin_baseline', 
                                 'r_d_victory_margin_ensemble'))

plot_data <- pivot_longer(combined, cols = c('r_d_victory_margin_baseline', 
                                        'r_d_victory_margin_mrp',
                                        'r_d_victory_margin_ensemble',
                                        ),
                     names_to = 'model',
                     values_to = 'estimate') |>
  mutate(area = paste0(state, '-', district)) |>
  left_join(cor_data, by = 'model') |>
  select(-matches('X', ignore.case = F)) |> 
  dplyr::rename(
    `Estimated Victory Margin` = estimate, 
    `Actual Victory Margin` = r_d_victory_margin
  ) |> 
  mutate(correlation_str = sprintf(
    "R = %1.5s",
    correlation)) |> 
  mutate(model_str = get_model_names(model))

cor_labels <- cor_data |>
  mutate(correlation_str = sprintf("R = %.3f", correlation)) |>
  mutate(model_str = get_model_names(model))
  
ggplot(
  plot_data, 
  aes(x = `Estimated Victory Margin`, y = `Actual Victory Margin` )) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ model_str, nrow = 3) +
  geom_point(
    size = 0.2,
    alpha = 0.2
    ) +
  geom_text(
    size = 2,
    label = plot_data$area,
    nudge_x = 0, nudge_y = 0, 
    check_overlap = T ) +
  geom_text(
    data = cor_labels,
    aes(x = 1, y = -0.8, label = correlation_str),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.5,
    size = 5) +
  labs(
    title = "Estimated versus Actual Victory Margins",
    x = "Estimated Victory Margin (R - D)", y = "Actual Victory Margin (R - D)") +
  theme_bw() +
  theme( plot.title = element_text(size = 14, hjust = 0.5))

###Save to file
outfile <- paste0('figures/model_corr.png')
ggsave(outfile, height = 7, width = 7)

###Compute nr misses, nr gop seats, and rmse
miss_mrp <- sum(ifelse( (combined$r_d_victory_margin > 0 & combined$r_d_victory_margin_mrp <= 0) |
                          (combined$r_d_victory_margin <= 0 & combined$r_d_victory_margin_mrp > 0), 
                        1, 0 ), na.rm = T)
miss_baseline <- sum(ifelse( (combined$r_d_victory_margin > 0 & combined$r_d_victory_margin_baseline <= 0) |
                               (combined$r_d_victory_margin <= 0 & combined$r_d_victory_margin_baseline > 0),
                             1, 0), na.rm = T)
miss_ensemble <- sum(ifelse( (combined$r_d_victory_margin > 0 & combined$r_d_victory_margin_ensemble <= 0) |
                               (combined$r_d_victory_margin <= 0 & combined$r_d_victory_margin_ensemble > 0),
                             1, 0), na.rm = T)

cat(paste0('Mrp model predictions missed races: ', format(miss_mrp, digits = 4)))
cat(paste0('Baseline model predictions missed races: ', format(miss_baseline, digits = 4)))
cat(paste0('Ensemble model predictions missed races: ', format(miss_ensemble, digits = 4)))

diff_mrp <- sum(ifelse( (combined$r_d_victory_margin_mrp >= 0), 1, 0 ), na.rm = T)
diff_baseline <- sum(ifelse( (combined$r_d_victory_margin_baseline >= 0), 1, 0 ), na.rm = T)
diff_ensemble <- sum(ifelse( (combined$r_d_victory_margin_ensemble >= 0), 1, 0 ), na.rm = T)
cat(paste0('Mrp model predicted GOP seats: ', format(diff_mrp, digits = 4)))
cat(paste0('Baseline model predicted GOP seats: ', format(diff_baseline, digits = 4)))
cat(paste0('Ensemble model predicted GOP seats: ', format(diff_ensemble, digits = 4)))

rmse_mrp <- mean((combined$r_d_victory_margin - combined$r_d_victory_margin_mrp)^2, na.rm = T)^0.5 
rmse_baseline <- mean((combined$r_d_victory_margin - combined$r_d_victory_margin_baseline)^2, na.rm = T)^0.5 
rmse_ensemble <- mean((combined$r_d_victory_margin - combined$r_d_victory_margin_ensemble)^2, na.rm = T)^0.5 
cat(paste0('Mrp model predictions RMSE: ', format(rmse_mrp, digits = 4)))
cat(paste0('Baseline model predictions RMSE: ', format(rmse_baseline, digits = 4)))
cat(paste0('Ensemble model predictions RMSE: ', format(rmse_ensemble, digits = 4)))

################################################################################
#Table: performance statistics for 3 models

results_tbl <- data.frame(model = c('MRP','Baseline','Ensemble'),
                          R = c(cor_mrp, cor_baseline, cor_ensemble),
                          Missed_Races = c(miss_mrp, miss_baseline, miss_ensemble),
                          Predicted_GOP_Seats = c(diff_mrp, diff_baseline, diff_ensemble),
                          RMSE = c(rmse_mrp, rmse_baseline, rmse_ensemble)
                          ) |> t()

latex_tbl <- xtable(results_tbl,
  caption = "Model Performance on 2018 U.S. House Elections",
  label = "tab:model_performance_2018",
  align = c("l", "l", "c", "c") )

print(latex_tbl,
  file = "tables/model_performance_2018.tex",
  include.rownames = FALSE,
  caption.placement = "top")

################################################################################
#Table: Baseline regression coefficients 

baseline_mod <- readRDS('models/baseline_model.rds')
stargazer(baseline_mod, out = 'tables/baseline_model.tex')

################################################################################
#Create posterior distribution plot of ensemble and mrp models

posterior_ensemble <- combined |> 
  select(state, district, year, r_d_victory_margin_baseline, matches('X', ignore.case = F)) |>
  mutate(across(matches('X', ignore.case = F), 
                ~ . * ENSEMBLE_WT_MRP + r_d_victory_margin_baseline * ENSEMBLE_WT_BASELINE))

seats_ensemble <- data.frame(seats_ensemble = apply(posterior_ensemble |> select(matches('X', ignore.case = F)), 
      MARGIN = 2, function(x) {sum(ifelse(x > 0, 1, 0))}) |> 
  as.vector()) 
seats_mrp <- data.frame(seats_mrp = apply(combined |> select(matches('X', ignore.case = F)), 
      MARGIN = 2, function(x) {sum(ifelse(x > 0, 1, 0))}) |> 
  as.vector())

data_post <- data.frame(seats_ensemble, seats_mrp) |>
  pivot_longer(everything(), names_to = 'Model', values_to= 'Estimate') |>
  mutate(model_str = case_when(
    Model == 'seats_ensemble' ~ 'Ensemble',
    Model == 'seats_mrp' ~ 'MRP'
  ))

plt_mrp <- ggplot(data = seats_mrp, aes(seats_mrp)) +
  geom_histogram(
    fill = "#1f78b4", 
    color = "black",
    binwidth = 3) +
  labs(x = 'Projected GOP House Seats\n (1000 Draws)', y = 'Frequency', title = 'MRP Model') + 
  theme_bw() + 
  theme(plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)) +
 scale_x_continuous(labels =c(140,150,160,170),
                    breaks =c(140,150,160,170) )
  
plt_ensemble <- ggplot(data = seats_ensemble, aes(seats_ensemble)) +
  geom_histogram(
    binwidth = 1,
    fill = "#1f78b4", 
    color = "black") +
  labs(x = 'Projected GOP House Seats\n (1000 Draws)', y = 'Frequency', title = 'Ensemble Model') +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_x_continuous(labels =c(188, 191, 194, 197, 200),
                     breaks  =c(188, 191, 194, 197, 200))

###Save to file
outfile <- paste0('figures/posterior_draws.png')
ggsave(outfile, plt_mrp | plt_ensemble, height = 7/2, width = 7)


################################################################################
##Add ensemble results to ddhq data as final prediction

final_prediction <- read.csv('cleaned_data/ddhq_data.csv') |>
    mutate(R.D.Victory.Margin = case_when(
      R.D.Victory.Margin == '???' ~ NA_character_,
      .default = R.D.Victory.Margin
    )) |> 
    mutate(R.D.Victory.Margin = as.double(R.D.Victory.Margin)) |>
    left_join(combined |> 
                select(state, year, district, r_d_victory_margin_ensemble) |>
                         rename('State' = 'state',
                                'Year' = 'year',
                                'Congressional.District' = 'district'),
              by = c('State', 'Year', 'Congressional.District')) |>
    mutate(R.D.Victory.Margin = coalesce(R.D.Victory.Margin, r_d_victory_margin_ensemble*100))
  
write.csv(final_prediction, 'cleaned_data/FINAL_PREDICTION.csv')
  

