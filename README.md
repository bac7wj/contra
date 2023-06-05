# Contra-Analysis 
The official repository for contra-analysis, a holistic method of analysis data analysis that include
1. Visualization of effect size from broadly related experiments
2. Determination of the thresholds for meaningful and negligible effect size based on related experiments and real world considerations.
3. Decision-making procedure to determine which experiments show evidence of meaningful or negligible effect size.
4. Statistical hypothesis test performed within a single study to determine if the results have evidence of meaningful or negligible effect size.
<br>

This code used to generate all results and figures from the follow papers:
1. Contra-Analysis: Prioritizing Meaningful Effect Size in Scientific Research (https://arxiv.org/abs/2210.04867).
2. Contra-Analysis for Determining Negligible Effect Size in Scientific Research (https://arxiv.org/abs/2303.09428).
3. The Least Difference in Means: A Statistic for Effect Size Strength and Practical Significance (https://arxiv.org/abs/2205.12958).
4. The Most Difference in Means: A Statistic for the Strength of Null and Near-Zero Results (https://arxiv.org/abs/2201.01239).
<br>

This repository contains code necessary to compute the following statistics 
1. Most difference in means (&delta;<sub>M</sub>): calculated with mdm_credint() in R/contra.r, with relative=FALSE 
2. Relative most difference in means (r&delta;<sub>M</sub>): calculated with mdm_credint() in R/contra.r, with relative=TRUE 
3. Least difference in means (&delta;<sub>L</sub>): calculated with ldm_credint() in R/contra.r, with relative=FALSE 
4. Relative Least difference in means (r&delta;<sub>L</sub>): calculated with ldm_credint() in R/contra.r, with relative=TRUE 
<br>

Note: run "set_wd_here.R" in base directoy first to set working directory to the base folder for the repository. All code assumes that is the location for the workign directory.
<br>

## Folder Structure
  
1. __R/__: general r code for calculating statistics, integrated risk analysis, and correlation tests
   
   1. __contra.r__: functions to calculate most difference in means and least difference in means statistics.
   
   2. __candidate_stats_from_xlsx.R__: functions to import data for applied examples.
   
   3. __coverage_error_toolbox.R__: simulation toolbox to test coverage error of mdm and rmdm (coverage error is the complement of coverage probability used for confidence intervals, code tests how often the mdm and rmdm is wrongly less than the pop. difference in means and rel. differnece in means).
   
   4. __credibility_rate_toolbox.R__: simulation toolbox to test credibility of mdm and rmdm.
   
   5. __illustrative_plot.R__: helper functions for producing plots to illustrate null strength and effect strength measures.
   
   6. __parallel_utils.R__: helper functions for parallel processing.
   
   7. __row_stats_toolbox.R__: helper functions for parallel processing.
   
   8. __strength_risk_assessment.R__: simulation toolbox to test candidate measures in identifying experiments with higher null strength or effect strength.
## Most difference in Means 
2. __mdm_t/__: scripts used to generate figures for &delta;<sub>M</sub> and r&delta;<sub>M</sub> manuscript (https://arxiv.org/abs/2201.01239)
   
   1. __figure2_covary_with_null_strength_SF2-5__: examines how candidate statistics respond to controlled changes of null strength. A valid statistics should respond in a consistent direction to increased null strength across all null strength measures.
   
   2. __figure2_illustrations_null_strength_measures.R__: A series of examples to illustrate the three parameters of unscaled agreement and relative agreement. Each example compares the difference in means or relative difference in means between a hypothetical control and experiment group.
   
   3. __figure3_risk_assessment_summary.R__: Runs a collection of risk assessments for test how often each candidate statistic incorrectly predicts which of two results have greater null strength.
   
   4. __figure4-5_calculate_stats_from_sxls.R__: creates excel table of results used as applied examples in manuscript.
   
   5. __sfigure1_mdm_credibility_rate.R__: calculates credibility rate of mdm with Monte Carlo method
   
   6. __sfigure1_rmdm_credibility_rate.R__: calculates credibility rate of rmdm with Monte Carlo method.
   
   7. __sfigure6-7_unscaled_risk_assessment_null.r__: Risk assessment for mu_dm, sigma_dm, df_dm, alpha_dm with null with identifying results with higher null strength.
   
   8. __sfigure8-9_unscaled_risk_assessment_critical.r__: Risk assessment for mu_dm, sigma_dm, df_dm, alpha_dm with identifying results with higher null strength.
   
   9. __sfigure10-11_relative_risk_assessment_null.R__: Risk assessment for rmu_dm, rsigma_dm, df_dm, alpha_dm with identifying results with higher null strength.
   
   10. __sfigure12-13_risk_assessment_contest_crit.r__: Risk assessment for rmu_dm, rsigma_dm, df_dm, alpha_dm with identifying results with higher null strength.

   
   ## Least difference in Means 
3. __ldm_t/__: scripts used to generate figures for &delta;<sub>L</sub> and r&delta;<sub>L</sub> manuscript (https://arxiv.org/abs/2205.12958)
   
   1. __figure2_covary_with_effect_strength_SF2-5__: examines how candidate statistics respond to controlled changes of effect strength. A valid statistics should respond in a consistent direction to increased effect strength across all effect strength measures.
   
   2. __figure2_illustrations_effect_strength_measures.R__: A series of examples to illustrate the three parameters of unscaled agreement and relative agreement. Each example compares the difference in means or relative difference in means between a hypothetical control and experiment group.
   
   3. __figure3_risk_assessment_summary.R__: Runs a collection of risk assessments for test how often each candidate statistic incorrectly predicts which of two results have greater effect strength.
   
   4. __figure4-5_calculate_stats_from_sxls.R__: creates excel table of results used as applied examples in manuscript.
   
   5. __sfigure1_mdm_credibility_rate.R__: calculates credibility rate of ldm with Monte Carlo method
   
   6. __sfigure1_rmdm_credibility_rate.R__: calculates credibility rate of rldm with Monte Carlo method.
   
   7. __sfigure6-7_unscaled_risk_assessment_null.r__: Risk assessment for mu_dm, sigma_dm, df_dm, alpha_dm with identifying results with higher effect strength.
   
   8. __sfigure8-9_unscaled_risk_assessment_critical.r__: Risk assessment for mu_dm, sigma_dm, df_dm, alpha_dm with identifying results with higher effect strength.
   
   9. __sfigure10-11_relative_risk_assessment_null.R__: Risk assessment for rmu_dm, rsigma_dm, df_dm, alpha_dm with identifying results with higher effect strength.
   
   10. __sfigure12-13_risk_assessment_contest_crit.r__: Risk assessment for rmu_dm, rsigma_dm, df_dm, alpha_dm with identifying results with higher effect strength.
