# Contra-Analysis 

The official repository for contra-analysis, a holistic method of data analysis for controlled experiments.
![Contra-Analysis Flowchart](https://github.com/bac7wj/contra/blob/98b1a2adae65abd003d6a804712fd1a52bc28332/resources/contra-analysis-flowshart.PNG)

This data analysis process includes:
1. Visualization of effect size from broadly related experiments.
2. Determination of the thresholds for meaningful and negligible effect size based on related experiments and real world considerations.
3. Decision-making procedure to determine which experiments show evidence of meaningful or negligible effect size.
4. Statistical hypothesis test performed within a single study to determine if the results have evidence of meaningful or negligible effect size.
<br>

This code used to generate all results and figures from the follow papers:<br>
[1] **Contra-Analysis: Prioritizing Meaningful Effect Size in Scientific Research** (https://arxiv.org/abs/2210.04867).<br>
[2] **Contra-Analysis for Determining Negligible Effect Size in Scientific Research** (https://arxiv.org/abs/2303.09428).<br>
[3] **The Least Difference in Means: A Statistic for Effect Size Strength and Practical Significance** (https://arxiv.org/abs/2205.12958).<br>
[4] **The Most Difference in Means: A Statistic for the Strength of Null and Near-Zero Results** (https://arxiv.org/abs/2201.01239).<br>
<br>

This repository contains code necessary to compute the following statistics 
1. Most difference in means (&delta;<sub>M</sub>): calculated with mdm_credint() in R/contra.r, with relative=FALSE 
2. Relative most difference in means (r&delta;<sub>M</sub>): calculated with mdm_credint() in R/contra.r, with relative=TRUE 
3. Least difference in means (&delta;<sub>L</sub>): calculated with ldm_credint() in R/contra.r, with relative=FALSE 
4. Relative Least difference in means (r&delta;<sub>L</sub>): calculated with ldm_credint() in R/contra.r, with relative=TRUE 
<br>

Note: run "set_wd_here.R" in base directoy first to set working directory to the base folder for the repository. All code assumes that is the location for the working directory.
<br>

## Folder Structure
  
1. __R/__: general r code for calculating statistics, integrated risk analysis, and correlation tests
   
   1. __candidate_stats_from_xlsx.R__: functions to import data from excel files for applied examples that are used in [1-4].
   2. __contra.r__: functions to calculate most difference in means and least difference in means statistics that are used in contra-analysis.
   3. __contra_experimental.r__: experimental functions (in-progress) for alternative statistics for contra-analysis (frenquitist approaches etc.).
   4. __contra_plot__: visualization used for contra-analysis. Displays interval estimates of relative effect size for a collection of braodly related experiments (measure the same output variable using different model systems and disease systems)
   5. __coverage_error_toolbox.R__: simulation toolbox to test coverage error of mdm and rmdm (coverage error is the complement of coverage probability used for confidence intervals, code tests how often the mdm and rmdm is wrongly less than the population difference in means and rel. difference in means).
   6. __credibility rate_toolbox.R__: simulation toolbox to test credibility of mdm and rmdm.
   7. __illustrative_plot.R__: helper plot functions for simple figures in each manuscript that illustrates basic concepts.
   8. __mirrored_transforms.R__: helper functions for transforms required for the contra_plot
   9. __parallel_utils.R__: helper functions for parallel processing.
   10. __row_stats_toolbox.R__: helper functions for parallel processing.
   11. __strength_risk_assessment.R__: simulation toolbox to test candidate measures in identifying experiments with higher null strength or effect strength (see [3-4]).
2. __contra_analysis_meaningful/__: scripts used to generate figures for manuscript [1].
3. __contra_analysis_negligible/__: scripts used to generate figures for manuscript [2].
4. __ldm_t/__: scripts used to generate figures for manuscript [3].  
5. __mdm_t/__: scripts used to generate figures for manuscript [4]. 
