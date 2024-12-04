# Emulator's experiments for MME design

This repository contains the data and codes of [Rohmer et al. 2024 submitted]() for:
- Analysing the 2100 GriIS [MME](./data) with the scripts 
	- [1-run_analysis_originalMME](./1-run_analysis_originalMME.R]) for ploting the histograms, the projections and the validaiton of the RF reference solution
	- [0-run_sensitivity](./0-run_sensitivity.R) for running the sensitivity analsyis
- Defining the experiments for
	- the validation with [2-run_exp_CV](./2-run_exp_CV.R) and stored in [exp_cv](./exp_cv)
	- the probabilistic projection with [3-run_exp_PROJEC](./3-run_exp_PROJEC.R) and stored in [exp_pred](./exp_pred)
- Analysing the results of 
	- the validation in [exp_cv](./exp_cv)
	- the projection in [exp_pred](./exp_pred)
with the scripts:
	- [4a-run_Figure_DsDh](4a-run_Figure_DsDh.R) for plotting the perturbation of the MME after applying the experiment
	- [4b-run_Figure_CV](4b-run_Figure_CV.R) for plotting the validation results
	- [4c-run_Figure_PROJEC](4c-run_Figure_PROJEC.R) for plotting the projection results

Necessary R packages:
- Random forest model [ranger](https://cran.r-project.org/web/packages/ranger/index.html)
- Plotting utilities [ggplot2](https://ggplot2.tidyverse.org/), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html) and [cowplot](https://cran.r-project.org/web/packages/cowplot/index.html)
- Data manipulation [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
