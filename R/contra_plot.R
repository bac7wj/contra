

library(ggplot2)
# library(patchwork)
library(gridExtra)
library(grid)
library(stringr)
library(cowplot)

confint_dm <- function(x, y, conf.level = 0.95, num_param_sims = 250/(1-conf.level), 
                        plot=FALSE, relative = FALSE){
  #
  
  xbar <- mean(x)
  ybar <- mean(y)
  s2x <- var(x)
  s2y <- var(y)
  m <- length(x)
  n <- length(y)
  
  
  xbar_sims <- rnorm(n = num_param_sims, mean = xbar, sd = sqrt(s2x / m))
  ybar_sims <- rnorm(n = num_param_sims, mean = ybar, sd = sqrt(s2y / n))

  
  if(!relative){
    dm <- ybar_sims - xbar_sims
    estimate <- ybar - xbar
  } else {
    dm <- (ybar_sims - xbar_sims)/xbar_sims
    estimate <- (ybar - xbar)/ xbar
  }
  dm_bounds <- quantile(dm, c((1-conf.level)/2, 1-(1-conf.level)/2)) 
  
  
  out <- list(conf_int = dm_bounds, estimate = estimate, relative = relative, conf.level = conf.level)

  
  return(out)
}
  


contra_plot <- function(df = df, sorted = NULL, col_x_pos = "auto", 
                        ggsize = c(3, 6), fig_path = getwd(), fig_name = "contra_plot.png") {
  #' @description produces a contra_plot, which visualizes the fold difference 
  #' in means between a control group and experiment group in a series of studies.
  #' 
  #' @param df: dataframe that contains metadata for plot, must include the 
  #' columns c(estimate, lower, upper) and then at least one column of metadata 
  #' that is to be reproduced as a table in the plot
  #' @param sorted: specifies which column to sort the dataframe by, if any (NULL 
  #' mean no sorting done)
  #' @param col_x_pos: vector of relation x positions for table columns 
  #' reproduced in plot
  #' @return none
  
  save(list = ls(all.names = TRUE), file = "temp/contra_plot.RData", 
       envir = environment())
  # load(file = "temp/contra_plot.RData")
  base_font_size = 6
  xaxis_font_size = 5
  
  # Make data frame for plot of intervals, add alternating background color
  df_plot <- df
  if (!is.null(sorted)) {
    if (is.null(df_plot[[sorted]])) { stop("Column name for 'sorted' argument does not exist")}
    df_plot <- df_plot[order(df_plot[[sorted]]),]
  }
  df_plot$index <- 1:nrow(df_plot)
  df_plot$color <- rep(c("white", "gray95"), nrow(df_plot))[1:nrow(df_plot)]

  gg_plt <- ggplot(df_plot, aes(x = estimate, y = index, xmin = lower, xmax = upper)) +
    # geom_hline(aes(yintercept = index, color = color), size = 20) + 
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index-0.5, ymax = index+0.5, fill = color)) +
    geom_pointrange(shape = 22, fill = "black", size = .5, fatten =.5) +
    # annotate("segment", x = 0, y = 0, xend = 0, yend = ncol(df_plot)+0.5) +
    geom_segment(aes(x = 0, xend = 0, y = 0, yend = max(index) + 0.5), 
                 linetype = 1, size = .5, color = "gray90") +
    xlab("Fold Difference") + theme_classic() + 
    coord_cartesian(ylim = c(0.5, max(df_plot$index) + 1.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, max(df_plot$index) + 1.5)) +
    scale_color_identity() + scale_fill_identity() +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y=element_blank(),
          axis.title = element_text(size = 7),
          axis.text.x = element_text(size=7),
          plot.margin = unit(c(0, 0, 0, 0), "null"))
  # gg_plt
  
  # Data frame of metadata for contra_plot
  meta_list <- colnames(subset( df_plot, select = -c(lower,upper, color, index) ))
  df_meta = subset( df_plot, select = -c(lower,upper) )
  
  # Get max Character
  # Add extra blank row
  df_meta[nrow(df_meta) + 1,] <- rep(NA, ncol(df_meta))
  df_meta$index[nrow(df_meta)] <- nrow(df_meta)
  # levels(df_meta$study) <- c(levels(df_meta$study),str_to_title(colnames(df_meta)[1]))
  df_meta[nrow(df_meta),1:(ncol(df_meta)-2)] <-  
    paste("bold(",str_to_title(colnames(df_meta)[1:(ncol(df_meta)-2)]), ")", sep="")

  if (length(col_x_pos)==1 && col_x_pos=="auto") {
    max_nchars <- sapply(1:length(meta_list), function(x) max(c(nchar(df_meta[[meta_list[x]]]),nchar(meta_list[x]))))
    col_x_pos = cumsum(max_nchars/sum(max_nchars)*1.07) - max_nchars/sum(max_nchars)[1]
    
    col_x_pos[1]=-.05
    # col_x_pos[length(col_x_pos)] = 1.02
  }

  
  gg_tbl <- ggplot(data = df_meta, aes(y = index)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, 
                  ymin = index - 0.5, 
                  ymax = index + 0.5, fill = color)) +
    geom_hline(yintercept = nrow(df_meta)-0.5, color = "black", size = .5) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, max(df_meta$index) + 0.5)) +
    scale_colour_identity() +  scale_fill_identity() +
    xlab("Force This Label White") + theme_classic()+ xlim(0, 1) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y=element_blank(),
          axis.title = element_text(size = 7),
          axis.text.x = element_text(size=7, colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.line.x.bottom = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          plot.margin = unit(c(0, 0, 0, 0), "null"))
  

  # Fill in metadata columns
  for (n in seq_along(col_x_pos)) {
    gg_tbl <- gg_tbl + geom_text(x = col_x_pos[n],label = df_meta[[meta_list[n]]], 
                                 hjust = 0.5 - as.numeric(n==1)/2, # + as.numeric(n == length(col_x_pos))/2,
                                 parse = TRUE, size = 2.5)
  }
  
  # Arrange plot and table side by side
  gg_grid_plot <- grid.arrange(gg_plt, gg_tbl, ncol = 2) 
  save_plot(paste(fig_path, '/', fig_name, sep = ""),
           gg_grid_plot, dpi = 600, base_height = ggsize[1], 
           base_width = ggsize[2])
 
}




df = data.frame(study = c("Auckland", "Block", "Doran", "Gamsu", "Morrison", "Papageorgiou", "Tauesch"),
                control = c("36", "1", "4", "14", "3", "1", "8"),
                treatment = c("60", "5", "11", "20", "7", "7", "10"),
                estimate  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
                lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831))

df = rbind(df,df,df)
contra_plot(df = df, sorted = "lower", col_x_pos="auto") 

