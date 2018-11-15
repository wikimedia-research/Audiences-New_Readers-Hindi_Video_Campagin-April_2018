# Understand the impact of video campaign on traffic to Hindi Wikipedia

This is the analysis codebase of [T204275](https://phabricator.wikimedia.org/T204275). The full report can be viewed at: https://wikimedia-research.github.io/Audiences-New_Readers-Hindi_Video_Campagin-April_2018/

- `data.R` contains the query for fetching the data. 
- `refine.R` is the script to clean and organize the data. 
- `eda.R` generates graphs for exploratory data analysis.
- `functions.R` contains functions used in the causal impact estimation.
- `online_model_pv.R`, `online_model_pv_external.R`, `TV_model_pv.R`, `TV_model_pv_external.R`, `TV_model_ud.R` and `TV_model_ud_first_visit.R` repeat the workflow for different test time series: 1) use 10 fold cross validation to choose the best model, 2) test the model on a imaginary intervention, 3) apply the chosen model and estimate the causal impact.