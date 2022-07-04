



source("R/contra_plot.R")
source("R/candidate_stats_from_xlsx.R")
library(readr )
library(tidyr)

base_dir = "contra_analysis"
fig_num = "2" 
dir.create(file.path(getwd(), base_dir,"figure"), showWarnings = FALSE)
fig_path = paste(getwd(),"/",base_dir,"/figure/F",fig_num, sep="")
dir.create(fig_path, showWarnings = FALSE)



# Cholesterol Dataset
df_chol <- read_csv(file.path(proj_path, base_dir, "chol_results.csv"))

# Calculate raw confidence Intervals for Difference in means
conf_ints_list <- t(sapply(1:nrow(df_chol), 
       function(x) norm_confint_dmeans(df_chol$mean_x[x], df_chol$s_x[x], df_chol$n_x[x], 
                                       df_chol$mean_y[x], df_chol$s_y[x], df_chol$n_y[x],
                                       conf.level = 0.95, relative = FALSE)))
df_conf_ints <- as.data.frame(matrix(unlist(conf_ints_list), ncol = ncol(conf_ints_list), 
       dimnames = list(NULL, colnames(conf_ints_list))))
# Contra-plot
df_contra <- cbind(df_chol[c("tx", "ctrl", "study", "spec", "year", "loc")], 
                   df_conf_ints[c("estimate", "lower", "upper")])
contra_plot(df = df_contra, sorted = NULL, col_x_pos = "auto",
            xlabel = "Difference in Means", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3, 7), fig_path = fig_path, fig_name = "chol_raw_conf_contra_plot.png")
  
  

# Calculate raw confidence Intervals for Difference in means
conf_ints_list <- t(sapply(1:nrow(df_chol), 
                           function(x) norm_confint_dmeans(df_chol$mean_x[x], df_chol$s_x[x], df_chol$n_x[x], 
                                                           df_chol$mean_y[x], df_chol$s_y[x], df_chol$n_y[x],
                                                           conf.level = 0.95, relative = TRUE)))
df_conf_ints <- as.data.frame(matrix(unlist(conf_ints_list), ncol = ncol(conf_ints_list), 
                                     dimnames = list(NULL, colnames(conf_ints_list))))
# Contra-plot
df_contra <- cbind(df_chol[c("tx", "ctrl", "study", "spec", "year", "loc")], 
                   df_conf_ints[c("estimate", "lower", "upper")])
contra_plot(df = df_contra, sorted = NULL, col_x_pos = "auto", 
            xlabel = "Rel. Difference in Means", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3, 7), fig_path = fig_path, fig_name = "chol_rel_conf_contra_plot.png")

