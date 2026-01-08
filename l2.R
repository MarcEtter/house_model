# File to parse L2 Political data 
#NOTES BEFORE RUNNING THIS PROGRAM
#You will need access to the L2 Political data in order to run this script.
#NYU's copy of the L2 data exists on a shared drive, which requires NYU login credentials 
#and an NYU VPN (Cisco AnyConnect), if not on an NYU network, to access.
#Further reading: (https://www.nyu.edu/life/information-technology/infrastructure/network-services/vpn.html)
#Additionally, a distribution of 7-zip may be needed to run this program. 
#On Mac, I installed 7-zip using Brew and then appended 7-zip to my shell path using
#command [export PATH="/opt/homebrew/bin/7zz:$PATH"]
#in order to read 7-zip files from the command line using fread()

library(data.table)#fread()
library(tidyverse)

setwd('~/DropBox/house_model')

#In order to acquire the l2 data, I need to mount the l2 virtual drive to my
#computer. Unsuccessful attempt to automate this by passing my password through
#system via the input argument
# system('mount_smbfs smb://mje9832@storage.rcs.nyu.edu/l2_political l2', 
#        intern = TRUE,
#        receive.console.signals = TRUE,
#        input = ***Password***
#        )

poststrat_state <- function(data, state_to_poststrat){
  cat('Creating poststratification table for ', state_to_poststrat, '\n')
  data <- data |>
    mutate(Voters_Gender = case_when(
      Voters_Gender == 'F' ~ 'Female',
      Voters_Gender == 'M' ~ 'Male',
      .default = NA
    )) |>
    mutate(Voters_Age = case_when(
      Voters_Age < 30 ~ '18-29',
      Voters_Age < 50 ~ '30-49',
      Voters_Age < 65 ~ '50-64',
      Voters_Age >= 65 ~ '65+',
      .default = '30-49'
    )) |>
    mutate(EthnicGroups_EthnicGroup1Desc = case_when(
      EthnicGroups_EthnicGroup1Desc == 'East and South Asian' ~ 'Other',
      EthnicGroups_EthnicGroup1Desc == 'European' ~ 'Non-Hispanic White',
      EthnicGroups_EthnicGroup1Desc == 'Hispanic and Portuguese' ~ 'Other',
      EthnicGroups_EthnicGroup1Desc == 'Likely African-American' ~ 'Black',
      EthnicGroups_EthnicGroup1Desc == 'Other' ~ 'Other',
      .default = 'Other') 
    ) |>
    mutate(Parties_Description = case_when(
      Parties_Description == 'Republican' ~ 'Republican',
      Parties_Description == 'Democratic' ~ 'Democratic',
      #Parties_Description == 'Non-Partisan' ~ 'Independent',
      #Parties_Description == 'Registered Independent' ~ 'Independent',
      .default = 'Other')
    ) |>
    mutate(CommercialData_EstimatedIncomeAmount = gsub('\\$', '', CommercialData_EstimatedIncomeAmount) |> 
             as.integer(),
           income = case_when( #income categorical variable compatible with pew survey income data
             CommercialData_EstimatedIncomeAmount < 30000 ~ 'less than $30k',
             CommercialData_EstimatedIncomeAmount < 50000 ~ '$30k-$50k',
             CommercialData_EstimatedIncomeAmount < 75000 ~ '$50k-$75k',
             CommercialData_EstimatedIncomeAmount < 100000 ~ '$75k-$100k',
             CommercialData_EstimatedIncomeAmount < 150000 ~ '$100k-$150k',
             CommercialData_EstimatedIncomeAmount >= 150000 ~ '$150k or more',
             .default = '$50k-$75k'
           ) 
           ) |>
    rename(
           'district' = '2011_NEW_Congressional_District',
           'state' = 'Mailing_Addresses_State',
           'sex' = 'Voters_Gender',
           'age' = 'Voters_Age',
           'race' = 'EthnicGroups_EthnicGroup1Desc',
           'party' = 'Parties_Description') 
  
  #browser()
  cat(paste0('Randomly imputing ', data |> filter(is.na(sex)) |> nrow(), 
      ' missing observations in column "sex"\n'))
  data <- data |> 
    mutate(sex = case_when(
      is.na(sex) & runif(nrow(data)) > 0.5 ~ 'Male',
      is.na(sex) ~ 'Female',
      .default = sex
    ))
  
  cat(paste0('Dropping ', data |> filter(is.na(district)) |> nrow(), 
             ' observations missing congressional district\n'))
  data <- data |> 
    drop_na(district)

  cat(paste0('Recoding ', 
             data |> filter(state != state_to_poststrat) |> nrow(),
             ' observations in column "state"\n'))
  #Convert state of mailing address to state where registered when different 
  data <- data |> 
    mutate(state = case_when(
      'state' != state_to_poststrat ~ state_to_poststrat
      ) 
    )
  
  #browser()
  
  data <- data |> group_by(sex, age, race, income, party, district) |>
    summarize(state = unique(state), n = n()) |> 
    suppressMessages()
  
  return (data)
}

l2_dir_quote <- "~/l2/'ARCHIVE--SFTP--OLD VM1 Header Files'/"
l2_dir <- '~/l2/ARCHIVE--SFTP--OLD VM1 Header Files'
files <- list.files('~/l2')
data_year <- '2016'
#Extract files with keyword '2016' from l2 shared drive
files <- list.files(l2_dir)[grepl(data_year, list.files(l2_dir))]
variables <- c('LALVOTERID',
               'Parties_Description',
               '2011_NEW_Congressional_District',
               'Mailing_Addresses_State',
               'Voters_Gender',
               'Voters_Age',
               'EthnicGroups_EthnicGroup1Desc',
               'CommercialData_EstimatedIncomeAmount'                           
               )
file_header_substring <- 'SEQUENCE	LALVOTERID	Voters_StateVoterID	Voters_CountyVoterID	VoterTelephones_FullPhone	VoterTelephones_Phone10	VoterTelephones_TelConfidenceCode	VoterTelephones_TelCellFlag	VoterTelephones_TelSourceID	Voters_FirstName	Voters_MiddleName	Voters_LastName	Voters_NameSuffix	Residence_Addresses_AddressLine	Residence_Addresses_ExtraAddressLine	Residence_Addresses_City	Residence_Addresses_State	Residence_Addresses_Zip	Residence_Addresses_ZipPlus4	Residence_Addresses_HouseNumber	Residence_Addresses_PrefixDirection	Residence_Addresses_StreetName	Residence_Addresses_Designator	Residence_Addresses_SuffixDirection	Residence_Addresses_ApartmentNum	Residence_Addresses_ApartmentType	Residence_Addresses_CensusTract	Residence_Addresses_CensusBlockGroup	Residence_Addresses_CensusBlock	Residence_Addresses_Latitude	Residence_Addresses_Longitude	Residence_Families_FamilyID	Residence_Families_HHCount	Residence_HHGender_Description	Residence_HHParties_Description	Mailing_Addresses_AddressLine	Mailing_Addresses_ExtraAddressLine	Mailing_Addresses_City	Mailing_Addresses_State	Mailing_Addresses_Zip	Mailing_Addresses_ZipPlus4	Mailing_Addresses_HouseNumber	Mailing_Addresses_PrefixDirection	Mailing_Addresses_StreetName	Mailing_Addresses_Designator	Mailing_Addresses_SuffixDirection	Mailing_Addresses_ApartmentNum	Mailing_Addresses_ApartmentType	Mailing_Families_FamilyID	Mailing_Families_HHCount	Mailing_HHGender_Description	Mailing_HHParties_Description	Voters_Gender	Voters_Age	Voters_BirthDate	Parties_Description'

poststrat_df <- data.frame()
states <- state.abb

for (state in states){
  start <- Sys.time()
  
  state_files <- files[grepl(paste0('--',state,'--'), files)]
  state_file <- sort(decreasing = TRUE, 
                     setNames(str_extract(state_files, '20[0-9]{2}-[0-9]{2}-[0-9]{2}'), 
                              state_files)) |> names()
  state_file <- state_file[1]#choose the most recent file after sorting by date
  
  
  full_path <- paste0(l2_dir_quote, state_file)
  cat(paste0('Reading file ', full_path), '\n')
  
  if (grepl('[.]zip', full_path)){
    cmd <- paste0('unzip -p ', full_path) 
    state_voters <- fread(cmd = cmd, select = variables, fill = T, quote = '')
    
  }else if (grepl('[.]7z', full_path)){
    #Note, you need to install 7-zip and append it to your shell path
    cmd <- paste0('7zz e -so ', full_path)  
    state_voters <- fread(cmd = cmd, select = variables, skip = file_header_substring, quote = '') 

  }else{
    stop(paste0('Error: file type ', 
                substring(full_path, gregexpr('[.].*', full_path)), ' not supported.'))
  }

  poststrat_df <- rbind(poststrat_df, poststrat_state(state_voters, state))

  end <- Sys.time()
  cat(paste0('Completed ', state, ': ',
             format(as.double(difftime(end, start, unit = 'sec')), digits = 1),
             ' sec \n\n'))
}

#Add regional predictors to poststrat_df
statelvl_predictors <- read.csv('~/Dropbox/house_model/cleaned_data/statelevel_predictors.csv') |>
  select(state, census_region)
poststrat_df <- poststrat_df |> left_join(statelvl_predictors, by = 'state') 
#Add column for unique district identifier
poststrat_df <- poststrat_df |> mutate(state_district = paste0(state,'-',district))

#Write csv to file 
write.csv(poststrat_df, '~/Dropbox/house_model/cleaned_data/poststrat_district_2016.csv')

#Summary statistics:

#Partisan composition, by state
poststrat_df |> 
  group_by(state) |> 
  reframe(n_voters = sum(n), party = party, n = n) |> 
  ungroup() |> 
  group_by(state, party) |> 
  reframe(n_party = sum(n), n_voters = n_voters, pct_party = n_party/n_voters) |> 
  arrange(state, party, pct_party) |>
  unique()

#Racial composition, by state
poststrat_df |> 
  group_by(state) |> 
  reframe(n_voters = sum(n), race = race, n = n) |> 
  ungroup() |> 
  group_by(state, race) |> 
  reframe(n_race = sum(n), n_voters = n_voters, pct_race = n_race/n_voters) |> 
  arrange(race, pct_race) |>
  unique()

#Age composition, by state
poststrat_df |> 
  group_by(state) |> 
  reframe(n_voters = sum(n), age = age, n = n) |> 
  ungroup() |> 
  group_by(state, age) |> 
  reframe(n_age = sum(n), n_voters = n_voters, pct_age = n_age/n_voters) |> 
  arrange(age, pct_age) |>
  unique()

