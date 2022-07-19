


if (!require("pacman")) {install.packages("pacman")}; library(pacman)
# p_load(ggplot2)
p_load(readr )
p_load(tidyr)

source("R/contra_plot.R")
source("R/candidate_stats_from_xlsx.R")


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
                                                           conf.level = 0.95, relative = TRUE)))
# conf_ints_list = list()
# for (n in 1:nrow(df_chol)) {
#   conf_ints_list[[n]] <- 
#     norm_confint_dmeans(df_chol$mean_x[n], df_chol$s_x[n], df_chol$n_x[n], 
#                         df_chol$mean_y[n], df_chol$s_y[n], df_chol$n_y[n],
#                         conf.level = 0.95, relative = TRUE)
# } conf_ints_list <- do.call(rbind,conf_ints_list) 

df_conf_ints <- as.data.frame(matrix(unlist(conf_ints_list), ncol = ncol(conf_ints_list), 
                                     dimnames = list(NULL, colnames(conf_ints_list))))
# Contra-plot
source("R/contra_plot.R")
df_contra <- cbind(df_chol[c("tx", "ctrl",  "spec", "study")],
                   df_conf_ints[c("estimate", "lower", "upper")])
contra_plot(df = subset(df_contra, lower < 0 ), sort_colname = "closest", col_x_pos = "auto",
            xlabel = "Fold Change Product", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Neg_chol_rel_conf_closer_contra_plot.png",
            xlims = c(-0.65, 0.4), relative = TRUE, estimate_colname = "closest", estimate_label = "min")

contra_plot(df = subset(df_contra, upper > 0 ), sort_colname = "closest", col_x_pos = "auto", 
            xlabel = "Fold Change Product", plot_title = "Total Plasma Cholesterol",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "Pos_chol_rel_conf_closer_contra_plot.png",
            xlims = c(-0.3, 6), relative = TRUE, estimate_colname = "closest", estimate_label = "min")












source("R/contra_plot.R")


# Cholesterol Dataset
df_plaq <- read_csv(file.path(proj_path, base_dir, "plaq_results.csv"))


# Calculate raw confidence Intervals for Difference in means
conf_ints_list <- t(sapply(1:nrow(df_plaq),
                           function(x) norm_confint_dmeans(df_plaq$mean_x[x], df_plaq$s_x[x], df_plaq$n_x[x],
                                                           df_plaq$mean_y[x], df_plaq$s_y[x], df_plaq$n_y[x],
                                                           conf.level = 0.95, relative = TRUE)))

# conf_ints_list = list()
# for (n in 1:nrow(df_plaq)) {
#   conf_ints_list[[n]] <-
#     norm_confint_dmeans(df_plaq$mean_x[n], df_plaq$s_x[n], df_plaq$n_x[n],
#                         df_plaq$mean_y[n], df_plaq$s_y[n], df_plaq$n_y[n],
#                         conf.level = 0.95, relative = TRUE)
# }; conf_ints_list <- do.call(rbind,conf_ints_list)



df_conf_ints <- as.data.frame(matrix(unlist(conf_ints_list), ncol = ncol(conf_ints_list),
                                     dimnames = list(NULL, colnames(conf_ints_list))))
# Contra-plot
df_contra <- cbind(df_plaq[c("study_id", "tx", "ctrl",  "spec", "study")],
                   df_conf_ints[c("estimate", "lower", "upper")])
contra_plot(df = subset(df_contra, lower < 0 ), sort_colname = "closest", col_x_pos = "auto",
            xlabel = "Rel. Difference in Means", plot_title = "Plaque Area",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "neg_plaq_rel_conf_closer_contra_plot.png",
            xlims = c(-0.8, 0.45), relative = TRUE, estimate_colname = "closest", estimate_label = "min")

contra_plot(df = subset(df_contra, upper > 0 ), sort_colname = "closest", col_x_pos = "auto",
            xlabel = "Rel. Difference in Means", plot_title = "Plaque Area",
            ggsize = c(3.5, 6.5), fig_path = fig_path, fig_name = "pos_plaq_rel_conf_closer_contra_plot.png",
            xlims = c(-0.3, 20), relative = TRUE, estimate_colname = "closest", estimate_label = "min")

