library(tidyverse)

setwd('~/DropBox/house_model/')

pew <- read.csv('cleaned_data/pew_survey.csv',
                fileEncoding = "latin1",
                stringsAsFactors = FALSE)


pew <- pew |> select(f_reg, cong_w38, vtplan_w, 
                     f_sex, f_agecat, f_raceth, f_income, f_party_,
                     f_cregio, f_metro, 
                     weight_w) |> 
  #Avoid filtering in order to improve balance of likely v. non-likely voters
  #filter(f_reg == 'You are ABSOLUTELY CERTAIN that you are registered to vote at your current address') |>
  select(-f_reg) |>
  mutate(vote_choice = case_when(
    grepl('Republican', cong_w38) ~ 'Republican',
    grepl('Democrat', cong_w38) ~ 'Democrat',
    .default = 'Unsure/Other'
  )) |>
  mutate(likely_voter = case_when(
    vtplan_w == 'No, I do not plan to vote' ~ 0,
    vtplan_w == 'Not sure' ~ 0,
    vtplan_w == 'Refused' ~ 0, 
    vtplan_w == 'Yes, I plan to vote BEFORE Election Day' ~ 1, 
    vtplan_w == 'Yes, I plan to vote ON Election Day' ~ 1,
    .default = 0
  )) |>
  mutate(sex = case_when(
    f_sex == 'female' ~ 'Female',
    f_sex == 'male' ~ 'Male',
    .default = NA_character_
  )) |> 
  mutate(
    age = f_agecat
    )|>
  mutate(race = case_when(
    grepl('White non-Hispanic', f_raceth) ~ 'Non-Hispanic White',
    grepl('Black', f_raceth) ~ 'Black',
    .default = 'Other'
  )) |> 
  mutate(income = case_when(
    f_income == 'Less than $10,000' ~ 'less than $30k',
    f_income == '$10,000 to less than $20,000' ~ 'less than $30k',
    f_income == '$20,000 to less than $30,000' ~ 'less than $30k',
    f_income == '$30,000 to less than $40,000' ~ '$30k-$50k',
    f_income == '$40,000 to less than $50,000' ~ '$30k-$50k',
    f_income == '$50,000 to less than $75,000' ~ '$50k-$75k',
    f_income == '$75,000 to less than $100,000' ~ '$75k-$100k',
    f_income == '$100,000 to less than $150,000' ~ '$100k-$150k',
    f_income == '$150,000 or more' ~ '$150k or more',
    f_income == "Don't know/Refused" ~ '$50k-$75k'
  )) |>
  mutate(party = case_when(
    f_party_ == 'Democrat' ~ 'Democratic',
    f_party_ == 'Republican' ~ 'Republican',
    f_party_ == 'Independent' ~ 'Other',
    f_party_ == 'Something else' ~ 'Other',
    f_party_ == 'Refused' ~ 'Other'
  )) |> 
  mutate(census_region = case_when(
    f_cregio == 'Midwest' ~ 'North Central',
    .default = f_cregio
  )) |> 
  rename(
    'metro_status' = 'f_metro'
  ) |>
  select(-c(f_cregio, cong_w38, vtplan_w, 
            f_sex, f_agecat, f_raceth, f_income, f_party_))
  
write.csv(pew, 'cleaned_data/pew_cleaned.csv')

