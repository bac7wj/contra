


if (!require("pacman")) {install.packages("pacman")}; library(pacman)
# p_load(ggplot2)
p_load(readr )
p_load(tidyr)

source("R/contra_plot.R")
source("R/candidate_stats_from_xlsx.R")


base_dir = "contra_analysis_meaningful"
fig_num = "2" 
dir.create(file.path(getwd(), base_dir,"figure"), showWarnings = FALSE)
fig_path = paste(getwd(),"/",base_dir,"/figure/F",fig_num, sep="")
dir.create(fig_path, showWarnings = FALSE)





## Cholesterol Dataset
#---------------------------------------------------------------------------

df_chol <- read_csv(file.path(proj_path, base_dir, "chol_results.csv"))
df_chol$study <- abbreviate(df_chol$study,6)
df_chol$ICV <- df_chol$mean_x /(df_chol$s_x/sqrt(df_chol$n_x))


# Interval estimations of relative difference in means
df_interval <- calculate_contra_stats(df_chol)


# Contra-plot
df_contra <- cbind(df_chol[c("study_id", "ctrl", "tx", "sp", "year", "study")],
                   df_interval[c("int_estimate", "int_lower", "int_upper", "rldm","rmdm")])
contra_plot(df = subset(df_contra, int_lower < 0 ), col_x_pos = "auto",
            xlabel = "Relative Difference in Means", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Chol(-)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(-0.6, 0), relative = TRUE, mirror_x_axis = FALSE, prune_colnames = "rmdm",
            cum_col_x_pos_adj = c(0, -.01, 0, 0.04, .02, 0))

contra_plot(df = subset(df_contra, int_upper > 0 ), col_x_pos = "auto", 
            xlabel = "Relative Difference in Means", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Chol(+)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(0, 5), relative = TRUE, mirror_x_axis = FALSE, prune_colnames = "rmdm",
            cum_col_x_pos_adj = c(0, -.01, 0, 0.04, .02, 0))







## Plaque Dataset
#---------------------------------------------------------------------------

df_plaq <- read_csv(file.path(proj_path, base_dir, "plaq_results.csv"))
df_plaq$study <- abbreviate(df_plaq$study,6)
df_plaq$ICV <- df_plaq$mean_x /(df_plaq$s_x/sqrt(df_plaq$n_x))

# Interval estimations of relative difference in means
df_interval <- calculate_contra_stats(df_plaq)

# Contra-plot
df_contra <- cbind(df_plaq[c("study_id", "ctrl", "tx", "sp", "year", "study")],
                   df_interval[c("int_estimate", "int_lower", "int_upper", "rldm","rmdm")])
contra_plot(df = subset(df_contra, int_lower < 0 ), col_x_pos = "auto",
            xlabel = "Relative Difference in Means", plot_title = "Plaque Area",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Plaq(-)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(-0.85, 0), relative = TRUE, mirror_x_axis = FALSE, prune_colnames = "rmdm",
            cum_col_x_pos_adj = c(0, -.01, 0, 0.04, .02, 0))

contra_plot(df = subset(df_contra, int_upper > 0 ), col_x_pos = "auto",
            xlabel = "Relative Difference in Means", plot_title = "Plaque Area",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Plaq(+)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(0, 1.5), relative = TRUE, mirror_x_axis = FALSE, prune_colnames = "rmdm",
            cum_col_x_pos_adj = c(0, -.01, 0, 0.04, .02, 0))



