

#' Relative agreement contests, null region
#' Calculates comparison error for candidate statistics in determining which of 
#' two results have higher relative agreement. Each investigation varies single agreement 
#' parameter as independent variable (or varies multiple simultaneously). 
#' Selected independent variable(s) are used as ground truth to determine error rates.

# Load required packages
#-------------------------------------------------------------------------------
if (!require("pacman")) {install.packages("pacman")}; library(pacman)
p_load(ggplot2)
p_load(tibble)
p_load(broom)
p_load(tidyr)
p_load(dplyr)
p_load(boot)
p_load(readr)
p_load(gplots)
# User defined libraries
source("R/contra.r")
source("R/strength_risk_assessment.R")
base_dir = "mdm_t"
# Figure parameters
#-------------------------------------------------------------------------------
fig_num = "10" 
fig_path = paste(base_dir, "/figure/SF",fig_num, "/",sep="")
dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)
# Simulation parameters
#-------------------------------------------------------------------------------
# A simulation is a set of samples with a fixed set of parameters
# Parameters are randomly chosen
n_sims = 1e3
n_samples = 1e3
n_obs = 50
rand.seed = 1
use_pseudo_samples = TRUE
parallel_sims = TRUE
include_bf = TRUE
rscale_contest_path = paste(base_dir, "/figure/SF", fig_num, "/SF", fig_num,
                            "_rscale_contest_results.csv",sep="")
df_relative_null = list();
delta = 0.1


###############################################################################
#
# Relative Error
#
##############################################################################



# Contest 1) Lower rmu_dm
#
#------------------------------------------------------------------------------
set.seed(rand.seed)
gt_colnames = "is_rmudm_1hnst2"
fig_name = paste("F", fig_num, "_1_esize_contest_rmu_null", sep = "")
df_init <- generate_population_configs(n_samples=n_samples, n_sims=n_sims, rand.seed=rand.seed, 
                                   mus_1a  = 100, 
                                   sigmas_1a = 1, 
                                   rmus_1d  = runif(n_sims, 0.01, 0.05), 
                                   rsigmas_1d = 0.4,
                                   
                                   mus_2a  = 1000, 
                                   sigmas_2a = 1,
                                   rmus_2d  = runif(n_sims, 0.05, 0.09), 
                                   rsigmas_2d = 0.4,
                                   
                                   n_1a = n_obs, n_1b = n_obs,
                                   n_2a = n_obs, n_2b = n_obs,
                                   alpha_1 = 0.05, alpha_2 = 0.05,
                                   
                                   toggle_sign_rmu_d_hold_rsigma = TRUE,
                                   toggle_sign_mean_ab = FALSE,
                                   switch_group_ab = FALSE,
                                   switch_mu_ab_12 = FALSE,
                                   switch_mu_d_12 = FALSE,
                                   switch_rmu_d_12_hold_rsigma = TRUE,
                                   switch_sigma_ab_12 = FALSE,
                                   switch_alpha_12 = FALSE,
                                   switch_n_12 = FALSE,
                                   fig_name = paste(fig_name, ".tiff",sep = ""), fig_path = fig_path,
                                   gt_colnames = gt_colnames)
df_relative_null[[1]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames, 
                            measure_pretty_str = "abs(~r*mu[DM]*phantom(.))",
                            parallel_sims = parallel_sims,
                            fig_name = paste(fig_name, ".tiff",sep = ""),
                            fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                           use_pseudo_samples = use_pseudo_samples)





# Contest 2) Lower sigma_pool
#
#------------------------------------------------------------------------------
set.seed(rand.seed)
gt_colnames = "is_rsigmad_1hnst2"
fig_name = paste("F", fig_num, "_2_esize_contest_rsigma_null", sep = "")
df_init <- generate_population_configs(n_samples=n_samples, n_sims=n_sims, rand.seed=rand.seed,  
                                   mus_1a  = 100, 
                                   sigmas_1a = 1,
                                   rmus_1d  = 1, 
                                   rsigmas_1d = runif(n_sims, 4, 20),
                                   
                                   mus_2a  = 500, 
                                   sigmas_2a = 1,
                                   rmus_2d  = 1, 
                                   rsigmas_2d = runif(n_sims, 4, 20),
                                   
                                   n_1a = n_obs, n_1b = n_obs,
                                   n_2a = n_obs, n_2b = n_obs,
                                   alpha_1 = 0.05, alpha_2 = 0.05,
                                   
                                   toggle_sign_rmu_d_hold_rsigma = FALSE,
                                   toggle_sign_mean_ab = TRUE,
                                   switch_group_ab = FALSE,
                                   switch_mu_ab_12 = FALSE,
                                   switch_mu_d_12 = FALSE,
                                   switch_rmu_d_12_hold_rsigma = FALSE,
                                   switch_sigma_ab_12 = FALSE,
                                   switch_alpha_12 = FALSE,
                                   switch_n_12 = FALSE,
                                   fig_name = paste(fig_name, ".tiff",sep = ""), fig_path = fig_path,
                                   gt_colnames=gt_colnames) 
df_relative_null[[2]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames, 
                                    measure_pretty_str = "r*sigma[D]",
                                    parallel_sims = parallel_sims,
                                    fig_name = paste(fig_name, ".tiff",sep = ""),
                                    fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                                    use_pseudo_samples = use_pseudo_samples)





# Contest 3) Lower df_pool
#
#------------------------------------------------------------------------------
set.seed(rand.seed)
n1 <- runif(n_sims, 6, 20)
n2 <- runif(n_sims, 20, 60)
gt_colnames = "is_dfdm_1hnst2"
fig_name = paste("F", fig_num, "_3_esize_contest_df_null", sep = "")
df_init <- generate_population_configs(n_samples=n_samples, n_sims=n_sims, rand.seed=rand.seed,  
                                   mus_1a  = 100, 
                                   sigmas_1a = 1,
                                   mus_1ao  = seq(0.5, 1.4,length.out = n_sims),
                                   sigmas_1ao = 5,
                                   
                                   mus_2a  = 500, 
                                   sigmas_2a = 1,
                                   mus_2ao  =  seq(2.5, 6, length.out = n_sims),
                                   sigmas_2ao = 29,
                                   
                                   n_1a = n1, n_1b = n1,
                                   n_2a = n2, n_2b = n2,
                                   alpha_1 = 0.05, alpha_2 = 0.05,
                                   
                                   toggle_sign_rmu_d_hold_rsigma = TRUE,
                                   toggle_sign_mean_ab = FALSE,
                                   switch_group_ab = FALSE,
                                   switch_mu_ab_12 = FALSE,
                                   switch_mu_d_12 = FALSE,
                                   switch_rmu_d_12_hold_rsigma = FALSE,
                                   switch_sigma_ab_12 = FALSE,
                                   switch_alpha_12 = FALSE,
                                   switch_n_12 = TRUE,
                                   fig_name = paste(fig_name, ".tiff",sep = ""), fig_path = fig_path,
                                   gt_colnames=gt_colnames) 
df_relative_null[[3]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames, 
                                                   measure_pretty_str = "df[D]", 
                                                   parallel_sims = parallel_sims,
                                                   fig_name = paste(fig_name, ".tiff",sep = ""),
                                                   fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                                                  use_pseudo_samples = use_pseudo_samples)



# Contest 4) Lower alpha_dm
#
#------------------------------------------------------------------------------
set.seed(rand.seed)
gt_colnames = "is_alpha_1hnst2"
fig_name = paste("F", fig_num, "_4_esize_contest_alpha_null", sep = "")
mus_1ao = round(seq(1,8, length.out = n_sims),4)
df_init <- generate_population_configs(n_samples=n_samples, n_sims=n_sims, rand.seed=rand.seed, 
                                   mus_1a  = 100, 
                                   sigmas_1a = 1, 
                                   rmus_1d  = seq(0.02, 0.13, length.out = n_sims),
                                   rsigmas_1d = 0.5,
                                   
                                   mus_2a  = 1000, 
                                   sigmas_2a = 1,
                                   rmus_2d  = seq(0.02, 0.13, length.out = n_sims),
                                   rsigmas_2d = 0.5,
                                   
                                   n_1a = n_obs, n_1b = n_obs,
                                   n_2a = n_obs, n_2b = n_obs,
                                   alpha_1 = 0.05/runif(n_sims, 1, 2),
                                   alpha_2 = 0.05/runif(n_sims, 5, 10),

                                   toggle_sign_rmu_d_hold_rsigma = TRUE,
                                   toggle_sign_mean_ab = FALSE,
                                   switch_group_ab = FALSE,
                                   switch_mu_ab_12 = FALSE,
                                   switch_mu_d_12 = FALSE,
                                   switch_rmu_d_12_hold_rsigma = FALSE,
                                   switch_sigma_ab_12 = FALSE,
                                   switch_alpha_12 = TRUE,
                                   switch_n_12 = FALSE,
                                   fig_name = paste(fig_name, ".tiff",sep = ""), fig_path = fig_path,
                                   gt_colnames = gt_colnames)  
df_relative_null[[4]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames, 
                                                   measure_pretty_str = "alpha[DM]",
                                                   parallel_sims = parallel_sims,
                                                   fig_name = paste(fig_name, ".tiff",sep = ""),
                                                   fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                                                  use_pseudo_samples = use_pseudo_samples)




# Contest 6) Lower mu_dm, sigma_pool, df_pool
#
#------------------------------------------------------------------------------
set.seed(rand.seed+5)
n1 <- ceiling(runif(n_sims, 6-1, 15))
n2 <- ceiling(runif(n_sims, 16-1, 25))
gt_colnames = c("is_rmudm_1hnst2","is_rsigmad_1hnst2", "is_dfdm_1hnst2","is_alpha_1hnst2")
fig_name = paste("F", fig_num, "_5_esize_contest_free_null", sep = "")
df_init <- generate_population_configs(n_samples=n_samples, n_sims=n_sims, rand.seed=rand.seed, 
                                   mus_1a  = 10, 
                                   sigmas_1a = .1, 
                                   rmus_1d  = runif(n_sims,0.1, 0.14), 
                                   rsigmas_1d = runif(n_sims, .4, .5),
                                   
                                   mus_2a  = 50,  
                                   sigmas_2a = .1,
                                   rmus_2d  = runif(n_sims,0.14, 0.2), 
                                   rsigmas_2d = runif(n_sims, .5, .6),
                                   
                                   n_1a = n1, n_1b = n1,
                                   n_2a = n2, n_2b = n2,
                                   alpha_1 = 0.05/runif(n_sims, 1, 2),
                                   alpha_2 = 0.05/runif(n_sims, 5, 10),
                                   
                                   toggle_sign_rmu_d_hold_rsigma = TRUE,
                                   toggle_sign_mean_ab = FALSE,
                                   switch_group_ab = FALSE,
                                   switch_mu_ab_12 = FALSE,
                                   switch_mu_d_12 = FALSE,
                                   switch_rmu_d_12_hold_rsigma = TRUE,
                                   switch_sigma_ab_12 = FALSE,
                                   switch_rsigma_ab_12_hold_sigma_a = TRUE,
                                   switch_alpha_12 = TRUE,
                                   switch_n_12 = TRUE,
                                   fig_name = paste(fig_name, ".tiff",sep = ""), fig_path = fig_path,
                                   gt_colnames=gt_colnames)
df_relative_null[[5]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames[1], measure_pretty_str = "abs(~r*mu[DM]*phantom(.))",
                            parallel_sims = parallel_sims,
                            fig_name = paste(fig_name, "_rmu.tiff",sep = ""),
                            fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                           use_pseudo_samples = use_pseudo_samples)
df_relative_null[[6]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames[2], measure_pretty_str = "r*sigma[D]",
                            parallel_sims = parallel_sims,
                            fig_name = paste(fig_name, "_rsigma.tiff",sep = ""),
                            fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                           use_pseudo_samples = use_pseudo_samples)
df_relative_null[[7]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames[3], measure_pretty_str = "df[D]", 
                            parallel_sims = parallel_sims,
                            fig_name = paste(fig_name, "_df.tiff",sep = ""),
                            fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                           use_pseudo_samples = use_pseudo_samples)
df_relative_null[[8]] <- 
  process_strength_contest(df_init, gt_colname = gt_colnames[4], measure_pretty_str = "alpha[DM]",
                            parallel_sims = parallel_sims, 
                            fig_name = paste(fig_name, "_alpha.tiff",sep = ""),
                            fig_path = fig_path, delta = delta, is_delta_relative = TRUE,
                           use_pseudo_samples = use_pseudo_samples)



# Output results
dir.create(paste(base_dir, "/temp/",sep=""),recursive = TRUE,showWarnings = FALSE)
save(df_relative_null, file = paste(base_dir, "/temp/df_relative_null.RDS",sep=""))

