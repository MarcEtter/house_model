The project directory is organized in the following way:

├── baseline_mod.R #R script to fit baseline multiple linear regression model
├── cleaned_data #All data go here
├── figures #Report figures
├── fit_turnout_mod.R #brms model to predict whether a Pew respondent says they will vote
├── fit_vote_choice_mod.R #brms model to predict whom a Pew respondent says they will vote for
├── info 
├── l2.R #R script to create post-stratification table from L2 (Need L2 access to run)
├── models #fitted R models
├── mrp_mod.R #R script to compute MRP posterior draws
├── old
├── pew.R #R script to process Pew survey
├── results.R #R script to create final results and figures
└── tables #Report tables

Full project directory structure:

├── baseline_mod.R
├── cleaned_data
│   ├── 1976-2024-house.tab
│   ├── FINAL_PREDICTION.csv
│   ├── baseline_mod_predictions.csv
│   ├── codebook-us-house-1976–2024.md
│   ├── ddhq_data.csv
│   ├── ddhq_data.zip
│   ├── fundamentals.csv
│   ├── mrp_margin.csv
│   ├── mrp_margin_turnout.csv
│   ├── mrp_margin_turnout_weight.csv
│   ├── mrp_margin_weight.csv
│   ├── other
│   │   ├── mrp_margin copy.csv
│   │   ├── mrp_margin_turnout copy.csv
│   │   ├── mrp_margin_turnout_weight copy.csv
│   │   ├── mrp_margin_weight copy.csv
│   │   ├── poststrat_district_2016.csv
│   │   ├── poststrat_district_2016_old.csv
│   │   └── poststrat_state_2016_old.csv
│   ├── pew_cleaned.csv
│   ├── pew_survey.csv
│   ├── pew_survey_docs
│   │   ├── 31115613.pdf
│   │   ├── 31115613_prl.pdf
│   │   └── 31115613_tpl.pdf
│   ├── poststrat_district_2016.csv
│   └── statelevel_predictors.csv
├── figures
│   ├── model_corr.png
│   └── posterior_draws.png
├── fit_turnout_mod.R
├── fit_vote_choice_mod.R
├── info
│   ├── DDHQ_Data_Exercise - December 2025.pdf
│   ├── UAS Data User Agreement.pdf
│   ├── poll_aggregation.txt
│   └── uas.txt
├── l2.R
├── models
│   ├── baseline_model.rds
│   ├── likely_voter_brms4.rds
│   ├── old
│   │   ├── likely_voter_base.rds
│   │   ├── likely_voter_brms1.rds
│   │   ├── likely_voter_brms2.rds
│   │   ├── likely_voter_brms3.rds
│   │   ├── likely_voter_brms_old.rds
│   │   ├── vote_base.rds
│   │   ├── vote_brms1.rds
│   │   └── vote_brms2.rds
│   └── vote_brms3.rds
├── mrp_mod.R
├── old
│   └── l2_2.R
├── pew.R
├── results.R
└── tables
    └── model_performance_2018.tex