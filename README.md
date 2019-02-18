# Understand the impact of video campaign on traffic to Hindi Wikipedia

This is the analysis codebase of [T204275](https://phabricator.wikimedia.org/T204275). The full report can be viewed at: https://wikimedia-research.github.io/Audiences-New_Readers-Hindi_Video_Campagin-April_2018/

- `data.R` contains the query for fetching the data. 
- `refine.R` is the script to clean and organize the data. 
- `eda.R` generates graphs for exploratory data analysis.
- `functions.R` contains functions used in the causal impact estimation.
- Files in the `models` directory repeat the workflow for each test time series: 1) use 10 fold cross validation to choose the best model parameters, 2) validate the best model, 3) apply the chosen model and estimate the causal impact.