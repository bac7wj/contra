

if (!require("pacman")) {install.packages("pacman")}; library(pacman)
p_load(ggplot2)
# library(patchwork)
p_load(gridExtra)
p_load(grid)
p_load(stringr)
p_load(cowplot)



pretty_number <- function(x, relative) {
  if (relative) {x = x*100}
  
  if (x==0) { 
    num = "0"
  } else if (abs(x)>1000) {
    num <- str_replace(str_replace(str_replace(sprintf("%.1e", x),"e0", "e"), "-0","-"), "\\+0","+")
  } else if (abs(x) >= 100) {
    num <- sprintf("%.0f",x) 
  } else if (abs(x) >= 10) {
    num <- sprintf("%.0f", x) 
  } else if (abs(x) >= 1) {
    num <- sprintf("%.1f", x) 
  } else if (abs(x) >= .1) {
    num <- sprintf("%.2f", x) 
  } else {
    num <- str_replace(str_replace(str_replace(sprintf("%.1e", x),"e0", "e"), "-0","-"), "\\+0","+")
  }
  if (relative) {num = paste0(num,'%')}
  return(num)
}

norm_confint_dmeans <- function(mean_x, s_x, n_x, mean_y, s_y, n_y, 
                                conf.level = 0.95, num_param_sims = 500/(1-conf.level), 
                        plot = FALSE, relative = FALSE) {
  #' @description calculate confidence interval of the 95% difference in means 
  #' between group x (control) and group y (experiment) assuming normal 
  #' distributions for the means of both.
  #' Relative dm is calculated in unscaled or scaled difference in means, i.e.
  #' y-x, or (y-x)/x respectively. Confidence intervals are calcualted with monte
  #' carlo sampling from the sampling distributions. The number of monte carlo 
  #' trials is based on the specified significance level.
  #' 
  #' @param mean_x 
  #' @param s_x 
  #' @param n_x 
  #' @param mean_y 
  #' @param s_y 
  #' @param n_y 
  #' @param conf.level 
  #' @param plot 
  #' @param relative 
  #' 
  #' @return 

  save(list = ls(all.names = TRUE), file = "temp/norm_confint_dmeans.RData", 
       envir = environment())
  # load(file = "temp/norm_confint_dmeans.RData")
  
  mean_x_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(s_x^2 / n_x))
  mean_y_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(s_y^2 / n_y))

  mean_x_sims[mean_x_sims < 0] = 0
  mean_y_sims[mean_y_sims < 0] = 0
  
  save(list = ls(all.names = TRUE), file = "temp/norm_confint_dmeans.RData", 
       envir = environment())
  
  if (!relative) {
    dm <- mean_y_sims - mean_x_sims
    estimate <- mean_y - mean_x
  } else {
    dm <- (mean_y_sims - mean_x_sims)/mean_x_sims
    estimate <- (mean_y - mean_x)/ mean_x
  }
  # Two tail quantiles
  quants = c((1-conf.level)/2, 1-(1-conf.level)/2)
  dm_bounds <- quantile(dm,  quants) 
  
  # Produce named list of output
  out <- tibble(estimate = estimate, 
                lower = unname(dm_bounds[1]), 
                upper = unname(dm_bounds[2]), 
           lower_quantile = quants[1], upper_quantile = quants[2],
           relative = relative, conf.level = conf.level)
  return(out)
}
  




contra_plot <- function(df = df, sort_colname = NULL, col_x_pos = "auto", xlabel = "Fold Mean Difference",
                        ggsize = c(3, 6), fig_path = getwd(), fig_name = "contra_plot.png",
                        estimate_label = "est", plot_title = "Measurement", xlims = c(NA,NA),
                        relative = FALSE, estimate_colname = "estimate", rel_plot_widths = c(0.6,0.4),
                        null_sort_colname = "estimate") {
  #' @description produces a contra_plot, which visualizes the fold difference 
  #' in means between a control group and experiment group in a series of studies.
  #' 
  #' @param df: dataframe that contains metadata for plot, must include the 
  #' columns c(estimate, lower, upper) and then at least one column of metadata 
  #' that is to be reproduced as a table in the plot
  #' @param sort_colname: specifies which column to sort the dataframe by, if any (NULL 
  #' mean no sorting done)
  #' @param col_x_pos: vector of relation x positions for table columns 
  #' reproduced in plot
  #' @return none
  
  save(list = ls(all.names = TRUE), file = "temp/contra_plot.RData", 
       envir = environment())
  # load(file = "temp/contra_plot.RData")
  
  base_font_size = 6
  xaxis_font_size = 5

  # Separate results between negative sign, zero, and positive sign
  effect_sign <- ((sign(df$lower)==1) & (sign(df$upper)==1)) - 
    ((sign(df$lower) == -1) & (sign(df$upper) == -1))
  
  # Negative effect size
  df_neg <-  df[effect_sign == -1,]
  df_neg$closest <- df_neg$upper 
  # Null effect size
  df_null <- df[effect_sign ==  0,]
  df_null$closest <- 0
  # Negative effect size
  df_pos <-  df[effect_sign ==  1,]
  df_pos$closest <- df_pos$lower 
  
  # If sort requested, sort each result type and rewrite result index
  if (!is.null(sort_colname)) {
    if (is.null(df[[sort_colname]]) && (sort_colname != "closest") )
    { stop("Column name for 'sort_colname' argument does not exist")}
    # dividers between result type
    div_index = c(0,0)
    # Sort neative, null, and positive results
    df_neg <- df_neg[order(df_neg[[sort_colname]], decreasing = FALSE),]
    df_null <- df_null[order(df_null[[null_sort_colname]], decreasing = FALSE),]
    df_pos <- df_pos[order(df_pos[[sort_colname]], decreasing = FALSE),]
    
    df_plot <- rbind(df_neg, df_null, df_pos)
    df_plot$index <- 1:nrow(df_plot)
    
    # Add dividers
    div_index[1] <- nrow(df_neg) +.5
    div_index[2] <- nrow(df_neg) + nrow(df_null) +.5
    # Remove dividers at top or bottom of plot
    div_index <- div_index[!(div_index == 0.5 | div_index == (nrow(df_plot)+0.5))]

    
  } else {
    df_plot <- rbind(df_neg, df_null, df_pos)
  }
  
  # Transforms to visualize 
  # fc_prod <- function(x) {x[x<0] = -1/(x[x<0] + 1); x[x>0] = x[x>0] + 1; return(x)}
  # contract_1n1 <- function(x)  {x[x > 0] = x[x > 0] - 1; x[x < 0] = x[x < 0] + 1; return(x)}
  
  fc_prod <- function(x) {x[x<0] = -1/(x[x<0] + 1); x[x>0] = x[x>0] + 1; return(x)}
  contract_1n1 <- function(x)  {x[x > 0] = x[x > 0] - 1; x[x < 0] = x[x < 0] + 1; return(x)}
  
  # Add FCP transformed columns
  df_plot$fcp_estimate <- contract_1n1( fc_prod(df_plot$estimate) )
  df_plot$fcp_lower    <- contract_1n1( fc_prod(df_plot$lower)    )
  df_plot$fcp_upper    <- contract_1n1( fc_prod(df_plot$upper)    )
  
  
  fc_prod(df_plot$estimate[1])
  fc_prod(df_plot$lower[1])
  fc_prod(df_plot$upper[1]) 
  
  
  # Add alternating color boxes for readability
  df_plot$color <- rep(c("white", "gray95"), nrow(df_plot))[1:nrow(df_plot)]

  gg_plt <- ggplot(df_plot, aes(x = fcp_estimate, y = index, xmin = fcp_lower, xmax = fcp_upper)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index-0.5, ymax = index+0.5, fill = color)) +
    geom_pointrange(shape = 22, fill = "black", size = .5, fatten =.5) +
    annotation_custom(grid::textGrob(plot_title, gp = gpar(col = "black", fontsize = 8)),
                      xmin = -Inf, xmax = Inf,
                      ymin = max(df_plot$index)+1, ymax = max(df_plot$index)+1) +
    annotate("segment", x = 0, y = 0, xend = 0, yend = max(df_plot$index)+0.5) +
    # geom_hline(yintercept = div_index, size = .75, alpha = .2) +
    xlab(xlabel) + theme_classic() + 
    scale_y_continuous(expand = c(0, 0), breaks = df_plot$index) +
    coord_cartesian(ylim = c(0.5, max(df_plot$index) + 1.5))+#, xlim = xlims) +
    # scale_x_continuous(limits = xlims) +
    scale_color_identity() + scale_fill_identity() +
    theme( axis.title.y = element_blank(), #axis.text.y = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y=element_blank(),
          axis.title = element_text(size = 7),
          axis.text.x = element_text(size=7),
          plot.margin = unit(c(0,5, 0, 1), "pt"))
  gg_plt
  
  # Change xlabels for Fold change product transform
  x_labs <- ggplot_build(gg_plt)$layout$panel_params[[1]]$x$get_labels()
  zero_ind <- match("0", x_labs)
  # subtract 1 to xlabels below zero, remove negative sign, add exponent
  prev_neg_xlabs <- as.character(as.numeric(x_labs[1:zero_ind-1])-1)
  if (length(prev_neg_xlabs)!=0) {new_neg_xlabs <- paste0("1/",sub('.', '', prev_neg_xlabs))
  } else {new_neg_xlabs=NULL}
  new_zero_pos_xlabs <- as.character(as.numeric(x_labs[zero_ind:length(x_labs)])+1)
  new_xlabs <- c(new_neg_xlabs, new_zero_pos_xlabs)
  gg_plt <- gg_plt + scale_x_continuous(labels = parse(text = new_xlabs))
  gg_plt
 
  
  # Data frame of metadata for contra_plot
  meta_list <- c(estimate_label, colnames(subset( 
    df_plot, select = -c(estimate, closest, lower,upper, color, index,
                         fcp_lower, fcp_upper, fcp_estimate) )))
  # TODO NEED TO CONTROL WHICH VARIABLE ESTIMATE
  df_meta = cbind(data.frame(estimate = sapply(1:nrow(df_plot), function(x) 
    pretty_number(df_plot[[estimate_colname]][x], relative = relative))),
                  subset( df_plot, select = -c(lower, upper, estimate, closest,
                                               fcp_lower, fcp_upper, fcp_estimate)))
  names(df_meta)[1] <- estimate_label
  
  # Get max Character
  df_meta[nrow(df_plot) + 1,] <- rep(NA, ncol(df_meta))
  df_meta$index[nrow(df_meta)] <- max(df_meta$index+1, na.rm = TRUE)
  # Add column title as last entry to column title
  pretty_hdrs <- paste0("bold(",str_to_title(colnames(df_meta)[1:(ncol(df_meta)-2)]), ")")
  df_meta[nrow(df_meta),1:(ncol(df_meta)-2)] <-
    paste0("bold(",str_to_title(colnames(df_meta)[1:(ncol(df_meta)-2)]), ")")

  if (length(col_x_pos)==1 && col_x_pos=="auto") {
    # Find max length chars for each column (for loop used for debugging multibyte chars)
    # If apply is used instead of for, we can't see where the error was tripped
    max_nchars = rep(0, length(meta_list))
    for (n in seq_along(meta_list)) {
      max_nchars[n] <- max( c(nchar(df_meta[[meta_list[n]]][1:nrow(df_meta)-1]),
                           nchar(meta_list[n]))  ) + 5
    }
    rel_widths = max_nchars/sum(max_nchars)
    raw_x_pos = cumsum(max_nchars/sum(max_nchars))
    col_x_pos = raw_x_pos - raw_x_pos[1] 
    
    col_x_pos[length(col_x_pos)] = 1.04
    col_x_pos[1]= -0.05
    col_x_pos[length(col_x_pos)-1] = col_x_pos[length(col_x_pos)-1] + 0.02
  }

  gg_tbl <- ggplot(data = df_meta, aes(y = index)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, 
                  ymin = index - 0.5, 
                  ymax = index + 0.5, fill = color)) +
    geom_hline(yintercept = nrow(df_plot)+0.5, color = "black", size = .5) +
    geom_hline(yintercept = div_index, color = "black", size = .5, alpha = 0.5) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, max(df_plot$index) + 1.5)) +
    # scale_x_continuous(expand = c(0, 0)) + #, limits = c(0,1.02)) 
    coord_cartesian(clip="off") + xlim(0, 1) +
    scale_colour_identity() +  scale_fill_identity() + 
    xlab("Force This Label White") + theme_classic() +  
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title = element_text(size = 7),
          axis.title.x = element_text(colour = "white"),
          axis.line.x.bottom = element_line(color = "white"),
          axis.ticks.x = element_line(color = "white"),
          axis.text.x = element_text(size = 7, colour = "white"),
          plot.margin = unit(c(0, 0, 0, 0), "null"))
  gg_tbl
  # Fill in metadata columns
  for (n in seq_along(col_x_pos)) {
    gg_tbl <- gg_tbl + 
      geom_text(x = col_x_pos[n],
                label = c(df_meta[[meta_list[n]]][1:max(df_meta$index)-1], ""),
                hjust = 0.5 - as.numeric(n == 1)/2 + as.numeric(n == length(col_x_pos))/2,
                parse = FALSE, size = 2.5, color = c(rep("black", max(df_meta$index) - 1), "white"))
    gg_tbl <- gg_tbl +
      geom_text(x = col_x_pos[n], label = c(rep("", max(df_meta$index)-1), pretty_hdrs[n]),
                hjust = 0.5 - as.numeric(n == 1)/2 + as.numeric(n == length(col_x_pos))/2,
                parse = TRUE, size = 2.5)
  }
  gg_tbl
  
  # Arrange plot and table side by side
  # plot_grid(gg_plt, gg_tbl, align = "h", ncol = 2, rel_plot_widths = c(.6, .4))
  gg_grid_plot <- grid.arrange(gg_plt, gg_tbl, ncol = 2, widths = unit(rel_plot_widths, "npc")) 
  save_plot(paste(fig_path, '/', fig_name, sep = ""),
           gg_grid_plot, dpi = 600, base_height = ggsize[1], 
           base_width = ggsize[2])
 
}



# Example data to run
# df = data.frame(study = c("Auckland", "Block", "Doran", "Gamsu", "Morrison", "Papageorgiou", "Tauesch"),
#                 control = c("36", "1", "4", "14", "3", "1", "8"),
#                 treatment = c("60", "5", "11", "20", "7", "7", "10"),
#                 estimate  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
#                 lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
#                 upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831))
# 
# df = rbind(df,df,df)
# contra_plot(df = df, sort_colname = "lower", col_x_pos="auto") 

