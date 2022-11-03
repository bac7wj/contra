


if (!require("pacman")) {install.packages("pacman")}; library(pacman)
# p_load(ggplot2)
p_load(readr )
p_load(tidyr)

source("R/contra_plot.R")
source("R/candidate_stats_from_xlsx.R")


base_dir = "contra_analysis_neglgible"
fig_num = "2" 
dir.create(file.path(getwd(), base_dir,"figure"), showWarnings = FALSE)
fig_path = paste(getwd(),"/",base_dir,"/figure/F",fig_num, sep="")
dir.create(fig_path, showWarnings = FALSE)


## Cholesterol Dataset
#---------------------------------------------------------------------------
df_chol <- read_csv(file.path(proj_path, base_dir, "chol_results.csv"))
df_chol$study <- abbreviate(df_chol$study,6)
df_chol$ICV <- df_chol$mean_x /(df_chol$s_x/sqrt(df_chol$n_x))



df_interval <- calculate_contra_stats(df_chol)


# Contra-plot
source("R/contra_plot.R")
df_contra <- cbind(df_chol[c("study_id", "ctrl", "tx", "sp", "year", "study")],
                   df_interval[c("int_estimate", "int_lower", "int_upper", "rldm","rmdm")])
df_contra <- subset(df_contra, rldm < 200)

contra_plot(df = df_contra, col_x_pos = "auto",
            xlabel = "Relative Difference in Means", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Chol(-)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(-0.6, 2), relative = TRUE, least_colname = c("rldm",'"Ls%"'),
            most_colname = c("rmdm",'"Ms%"'),
            cum_col_x_pos_adj = c(0, .04, 0, -.01, 0.04, -.02, 0))


# least_col rldm
# most_col rmdm




contra_plot(df = subset(df_contra, upper > 0 ), sort_colname = "closest", col_x_pos = "auto", 
            xlabel = "Relative Difference in Means", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3.5, 10), fig_path = fig_path, fig_name = "Chol(+)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(0, 5), relative = TRUE, estimate_colname = "closest", estimate_label = '"Ls%"',
            cum_col_x_pos_adj = c(0, -.01, 0, 0.04, .02, 0))



## Plaque Dataset
#---------------------------------------------------------------------------

df_plaq <- read_csv(file.path(proj_path, base_dir, "plaq_results.csv"))
df_plaq$study <- abbreviate(df_plaq$study,6)
df_plaq$ICV <- df_plaq$mean_x /(df_plaq$s_x/sqrt(df_plaq$n_x))

conf_ints_list = list()
for (n in 1:nrow(df_plaq)) {
  conf_ints_list[[n]] <-
    norm_credint_dm(df_plaq$mean_x[n], df_plaq$s_x[n], df_plaq$n_x[n],
                        df_plaq$mean_y[n], df_plaq$s_y[n], df_plaq$n_y[n],
                        conf.level = 0.95, relative = TRUE)
}; conf_ints_list <- do.call(rbind,conf_ints_list)

df_conf_ints <- as.data.frame(matrix(unlist(conf_ints_list), ncol = ncol(conf_ints_list),
                                     dimnames = list(NULL, colnames(conf_ints_list))))
# Contra-plot
df_contra <- cbind(df_plaq[c("study_id","ctrl", "tx", "sp", "year", "study")],
                   df_conf_ints[c("estimate", "lower", "upper")])
contra_plot(df = df_contra, col_x_pos = "auto",
            xlabel = "Relative Difference in Means", plot_title = "Plaque Area",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Plaq(-)_rel_conf_closer_contra_plot.png",
            tf_xlims = c(-0.95, 0), relative = TRUE, estimate_colname = "closest", estimate_label = '"Ls%"',
            cum_col_x_pos_adj = c(0, -.01, 0, 0.04, .02, 0))


