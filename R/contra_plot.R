

if (!require("pacman")) {install.packages("pacman")}; library(pacman)
p_load(ggplot2)
p_load(gridExtra)
p_load(grid)
p_load(stringr)
p_load(cowplot)
source("R/mirrored_transforms.R")


abbreviate <- function(x, max.length=6) {
  x[nchar(x) > (max.length)+1] <- paste0(substr(x[nchar(x) > (max.length)+1],0,max.length),".")
  return(x)
}


pretty_number <- function(x, relative = FALSE) {
  #' @description formatted printing for an inpu number
  #'
  #' @param x numeric to be converted into a pretty string
  #' @param relative boolean for whether x is a percent (versus raw number)
  #' @return formatted string of input number x
  
  
  if (relative) {x = x*100}

  if (x==0) { 
    num = "0"
  } else if (abs(x)>10000) {
    num <- str_replace(str_replace(str_replace(sprintf("%.1e", x),"e0", "e"), "-0","-"), "\\+0","")
  } else if (abs(x) >= 100) {
    num <- sprintf("%.0f",x) 
  } else if (abs(x) >= 10) {
    num <- sprintf("%.0f", x) 
  } else if (abs(x) >= 1) {
    num <- sprintf("%.1f", x) 
  } else if (abs(x) >= .1) {
    num <- sprintf("%.2f", x) 
  }  else if (abs(x) >= .01) {
      num <- sprintf("%.3f", x) 
  } else {
    num <- str_replace(str_replace(str_replace(sprintf("%.1e", x),"e0", "e"), "-0","-"), "\\+0","")
  }
  # if (relative) {num = paste0(num,'%')}
  return(num)
}
pretty_numbers <- function(x, relative) {
  x <- sapply(1:length(x), function(n) pretty_number(x[n], relative = relative))
  return(x)
}


ldm_from_interval_bounds <- function(lo, hi) {
  ldm <- (lo+hi)/2 * (sign(lo)==sign(hi)) *  min(abs(c(lo,hi)))
  return(ldm)  
}

fract_2_number <- function(s) {
  sp <- strsplit(s, '[/|\\]')

  if (length(sp[[1]])==1) {
    x <- as.numeric(sp[[1]])
  } else if (length(sp[[1]])==2) {
    x <- as.numeric(sp[[1]][1]) / as.numeric(sp[[1]][2])
  } else {
    stop("multiple slashes detected in fraction")
  }
  return(x)
}

calculate_contra_stats <- function(df) {
    #' Calculates relative difference in means interval estimates, along with 
    #' most difference in means and least difference in means
  save(list = ls(all.names = TRUE), file = "temp/calculate_contra_stats.RData", 
       envir = environment())
  # load(file = "temp/calculate_contra_stats.RData")
  
   conf_ints_list = list();
    # Calculate interval estimates
    for (n in 1:nrow(df)) {
      conf_ints_list[[n]] <-
        norm_credint_dm(df$mean_x[n], df$s_x[n], df$n_x[n],
                        df$mean_y[n], df$s_y[n], df$n_y[n],
                        conf.level = 1-fract_2_number(df$alpha_dm[n]), relative = TRUE)
    }
    # Calculate contra statistics
    bound_conf_ints <- do.call(rbind, conf_ints_list)
    df_interval <- as.data.frame(matrix(unlist(bound_conf_ints), ncol = ncol(bound_conf_ints), 
                                         dimnames = list(NULL, colnames(bound_conf_ints))))
    
    df_interval$rldm <- sapply(1:nrow(df_interval), function(x) 
      ldm_from_interval_bounds(df_interval$int_lower[x], df_interval$int_upper[x]))
    
    
    df_interval$rmdm <- sapply(1:nrow(df), function(x) 
      mdm_credint_stats(mean_x = df$mean_x[x], var_x = df$s_x[x]^2, n_x = df$n_x[x],
                        mean_y = df$mean_y[x], var_y = df$s_y[x]^2, n_y = df$n_y[x],
                        conf.level = 1-fract_2_number(df$alpha_dm[n]), sharedVar = FALSE,
                        relative = TRUE))
    
    return(df_interval)
    
    
  }

norm_credint_dm <-
  function(mean_x, s_x, n_x, mean_y, s_y, n_y, conf.level = 0.95, 
           num_param_sims = 250/(1-conf.level), sharedVar = FALSE, 
           relative = FALSE, rand.seed = NA) {
    
    save(list = ls(all.names = TRUE), file = "temp/norm_credint_dm.RData", 
         envir = environment())
    # load(file = "temp/norm_credint_dm.RData")
    
    
    # x control group
    # y experiment group
    if (!is.na(rand.seed)) {set.seed(rand.seed)}
    if(sharedVar){
      shape <- .5*(n_x + n_y - 2)
      scale <- .5*((n_x - 1)*s_x^2 + (n_y - 1) * s_y^2)
      ssSims <- 1/rgamma(num_param_sims, shape = shape, rate = scale)
      mux_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(ssSims/n_x))
      muy_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(ssSims/n_y))
    }else{ # different variances
      shape1 <- .5*(n_x - 1)
      scale1 <- .5*(n_x - 1) * s_x^2
      shape2 <- .5*(n_y - 1)
      scale2 <- .5*(n_y - 1) * s_y^2
      ss1Sims <- 1/rgamma(n = num_param_sims, shape = shape1, rate = scale1)
      ss2Sims <- 1/rgamma(n = num_param_sims, shape = shape2, rate = scale2)
      mux_sims  <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(ss1Sims / n_x))
      muy_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(ss2Sims / n_y))
    }
    
    if (!relative) {
      dm <- muy_sims - mux_sims
      estimate <- mean_y - mean_x
    } else {
      dm <- (muy_sims - mux_sims)/mux_sims
      estimate <- (mean_y - mean_x)/ mean_x
    }
    quants <- c((1-conf.level)/2,1 - (1-conf.level)/2)
    dm_bounds <- quantile(dm,  quants, type = 1) 
    
    # Produce named list of output
    out <- tibble(int_estimate = estimate, 
                  int_lower = unname(dm_bounds[1]), 
                  int_upper = unname(dm_bounds[2]), 
                  int_lower_quant = quants[1], int_upper_quant = quants[2],
                  int_relative = relative, int_conf.level = conf.level)
    
    
    return(out)
  }



rnorm_confint_dm <- function(mean_x, sd_x, n_x, mean_y, sd_y, n_y, conf.level = 0.95, 
                       verbose = FALSE,  var.equal = FALSE, method = "fieller")  {
  #' @description Calculates the relative most difference in means assuming with
  #' rmdm = mdm/X, X being the control group and Y the experimental
  #' 
  #' @param xn_x vector of measurements in control group
  #' @param y_y vector of measurements in experimental group
  #' @param conf.level significance level for calculating upper mdm
  #' 
  #' @return relative most difference in means
  
  # Equation for pooled variance taken from:
  # https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals5.html
  
  # Calculate basic sample statistics
  se_x = sd_x/sqrt(n_x)
  se_y = sd_y/sqrt(n_y)
  
  mean_dm = (mean_y - mean_x)/mean_x
  df <- n_x + n_y - 2
  
  # sSquared <- (sum((x - mean_x)^2) + sum((y - mean_y)^2))/df
  sSquared <- ((n_x-1)*sd_x^2 + (n_y-1)*sd_y^2)/df
  
  
  fieller_int <- tryCatch(get_FiellerInterval(mean_x, mean_y, sSquared, 
                                              1/n_x, 1/n_y, df, v12 = 0, alpha=1-conf.level), 
                          error = function(c) data.frame(upper = NaN, lower = NaN))
  
  rmdm <- max(abs(c(fieller_int$lower,fieller_int$upper)))
  
  # Produce named list of output
  out <- tibble(estimate = mean_dm, 
                lower = fieller_int$lower, 
                upper = fieller_int$upper, 
                lower_quantile = (1-conf.level)/2, upper_quantile = 1-(1-conf.level)/2,
                relative = "TRUE", conf.level = conf.level)
  
  return(rmdm)
}




norm_confint_dm <- function(mean_x, s_x, n_x, mean_y, s_y, n_y, 
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
  # save(list = ls(all.names = TRUE), file = "temp/norm_confint_dmeans.RData", 
  #      envir = environment())
  # load(file = "temp/norm_confint_dmeans.RData")
  
  mean_x_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(s_x^2 / n_x))
  mean_y_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(s_y^2 / n_y))

  # Truncate all samples to have measurements > 0, since y,x > 0 is assumed
  mean_x_sims[mean_x_sims < 0] = 0
  mean_y_sims[mean_y_sims < 0] = 0

  if (!relative) {
    dm <- mean_y_sims - mean_x_sims
    estimate <- mean_y - mean_x
  } else {
    dm <- (mean_y_sims - mean_x_sims)/mean_x_sims
    estimate <- (mean_y - mean_x)/ mean_x
  }
  # Two tail quantiles
  quants = c((1-conf.level)/2, 1-(1-conf.level)/2)
  if (any(is.nan(dm))) {
    save(list = ls(all.names = TRUE), file = "temp/norm_confint_dmeans.RData", 
         envir = environment())
    stop()
  }
  dm_bounds <- quantile(dm,  quants) 
  
  # Produce named list of output
  out <- tibble(estimate = estimate, 
                lower = unname(dm_bounds[1]), 
                upper = unname(dm_bounds[2]), 
           lower_quantile = quants[1], upper_quantile = quants[2],
           relative = relative, conf.level = conf.level)
  return(out)
}
  

contra_plot <- function(df = df, signed_sort_colname = "rldm", col_x_pos = "auto", xlabel = "Fold Mean Difference",
                        ggsize = c(3, 6), fig_path = getwd(), fig_name = "contra_plot.png",
                        least_colname = c("rldm",'"Ls%"'), most_colname = c("rmdm",'"Ms%"'),
                        estimate_label = "est", plot_title = "Measurement", tf_xlims = NULL,
                        relative = FALSE, estimate_colname = "estimate", rel_plot_widths = c(0.5,0.5),
                        null_sort_name = "int_estimate", cum_col_x_pos_adj = rep(0,ncol(df)-3),
                        pretty_cols = c(), threshold = NULL, mirror_x_axis = FALSE) {
  #' @description produces a contra_plot, which visualizes the fold difference 
  #' in means between a control group and experiment group in a series of studies.
  #' 
  #' @param df dataframe that contains metadata for plot, must include the 
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

  # Separate results between negative sign, zero, and positive sign
  effect_sign <- ((sign(df$int_lower)==1) & (sign(df$int_upper)==1)) - 
    ((sign(df$int_lower) == -1) & (sign(df$int_upper) == -1))
  
  # Negative effect size
  df_neg <-  df[effect_sign == -1,]
  df_null <- df[effect_sign ==  0,]
  df_pos <-  df[effect_sign ==  1,]
   
  # If sort requested, sort each result type and rewrite result index
  if (!is.null(signed_sort_colname)) {
    if (is.null(df[[signed_sort_colname]]) && (signed_sort_colname != "closest") )
    { stop("Column name for 'signed_sort_colname' argument does not exist")}
    # dividers between result type
    div_index = c(0,0)
    # Sort negative, null, and positive results
    df_neg <- df_neg[order(df_neg[[signed_sort_colname]], decreasing = FALSE),]
    df_null <- df_null[order(df_null[[null_sort_name]], decreasing = FALSE),]
    df_pos <- df_pos[order(df_pos[[signed_sort_colname]], decreasing = FALSE),]
    
    df_plot <- rbind(df_neg, df_null, df_pos)
    df_plot$index <- 1:nrow(df_plot)
    
    # Add dividers
    div_index[1] <- nrow(df_neg) + 0.5
    div_index[2] <- nrow(df_neg) + nrow(df_null) + 0.5
    # Remove dividers at top or bottom of plot
    div_index <- div_index[!(div_index == 0.5 | div_index == (nrow(df_plot)+0.5))]

  } else {
    df_plot <- rbind(df_neg, df_null, df_pos)
  }
  
  # Stretch fold change on the negative x axis scale
  if (mirror_x_axis) {
    tf_function = function(x) mirror_rc(x, forward = TRUE)
  } else {
    tf_function = function(x) x
  }
  
  # Add FCP transformed columns
  df_plot$tf_estimate <- tf_function(df_plot$int_estimate)
  df_plot$tf_lower    <- tf_function(df_plot$int_lower)
  df_plot$tf_upper    <- tf_function(df_plot$int_upper)    
  
  # Add alternating color boxes for readability
  df_plot$color <- rep(c("white", "gray95"), nrow(df_plot))[1:nrow(df_plot)]

  gg_plt <- ggplot(df_plot, aes(x = tf_estimate, y = index, xmin = tf_lower, xmax = tf_upper)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index-0.5, ymax = index+0.5, fill = color)) +
    annotate("segment", x = 0, y = 0, xend = 0, yend = max(df_plot$index)+0.5) +
    geom_pointrange(shape = 22, fill = "black", size = .5, fatten = .5) +
    annotation_custom(grid::textGrob(plot_title, gp = gpar(col = "black", fontsize = 8)),
                      xmin = -Inf, xmax = Inf,
                      ymin = max(df_plot$index)+1, ymax = max(df_plot$index)+1) +
    xlab(xlabel) + theme_classic() + 
    scale_y_continuous(expand = c(0, 0), labels = c(df_plot$study_id,"ID"), breaks = c(df_plot$index,max(df_plot$index)+1)) +
    coord_cartesian(ylim = c(0.5, max(df_plot$index) + 1.5),xlim = tf_function(tf_xlims)) +
    scale_color_identity() + scale_fill_identity() +
    theme( axis.title.y = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y=element_blank(),
          axis.title = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          plot.margin = unit(c(0,0, 0, 0), "pt"))
  if (!is.null(threshold)) {
    gg_plt <- annotate("segment", x = threshold, y = 0, xend = threshold, 
                       yend = max(df_plot$index)+0.5, color = "darkgoldenrod1")
  }
  gg_plt
  
  # If x-axis mirrored chosen, switch axis labels back to pre transform values
  if (mirror_x_axis){
    xlabs <- ggplot_build(gg_plt)$layout$panel_params[[1]]$x$get_labels()
    # Remove NAs (sometimes there are hidden empty ticks)
    xlabs <- xlabs[!is.na(xlabs)]

    x_breaks <- ggplot_build(gg_plt)$layout$panel_params[[1]]$x$get_breaks()
    x_breaks <- x_breaks[!is.na(x_breaks)]
    
    new_xlabs_num <- mirror_rc(as.numeric(xlabs), forward = FALSE)
    new_xlabs <- as.character(new_xlabs_num)
   
    gg_plt <- gg_plt + scale_x_continuous(labels = new_xlabs, breaks = x_breaks)
    gg_plt
  }
 
 
  
  # least_colname= c("rldm",'"Ls%"'), most_colname = c("rmdm",'"Ms%"'),
  
  removed_cols <- 
    c("int_estimate", "int_lower", "int_upper", "tf_lower", "color","index",
      "tf_upper", "tf_estimate", "study_id", least_colname[1], most_colname[1])
  
  # Data frame of metadata for contra_plot
  meta_list <-
    c(least_colname[1], most_colname[1],
      colnames(df_plot)[!is.element(colnames(df_plot), removed_cols)])
  

  df_meta <- df_plot[,meta_list]

  pretty_colnames <-c(least_colname[2], most_colname[2], 
                      tools::toTitleCase(meta_list[3:length(meta_list)]))
  
  
  df_meta[[least_colname[1]]] <-pretty_numbers(df_plot[[least_colname[1]]], relative = relative)
  df_meta[[most_colname[1]]] <-pretty_numbers(df_plot[[most_colname[1]]], relative = relative)

  
  # Get max Character
  df_meta[nrow(df_plot) + 1,] <- rep(NA, ncol(df_meta))
  df_meta$index[nrow(df_meta)] <- max(df_plot$index, na.rm = TRUE)+1
  # Add column title as last entry to column title
  pretty_hdrs <- paste0("bold(",pretty_colnames, ")")
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
    
    col_x_pos = col_x_pos + cumsum(cum_col_x_pos_adj)
    
    col_x_pos[length(col_x_pos)] = 1 #1.04
    col_x_pos[1]= 0  #-0.05
    col_x_pos[length(col_x_pos)-1] = col_x_pos[length(col_x_pos)-1] + 0.02
    
  }

  
  df_meta$index <- c(df_plot$index, max(df_plot$index)+1)
  df_meta$color <- c(df_plot$color, "white")
  df_meta$dummy_x = seq(0.02, 0.98, length.out = nrow(df_meta))
  
  gg_tbl <- ggplot(data = df_meta, aes(y = index)) +
    geom_point(aes(x=dummy_x), color = "white") +
    geom_rect(aes(xmin = -Inf, xmax = Inf, 
                  ymin = index - 0.5, 
                  ymax = index + 0.5, fill = color)) +
    geom_hline(yintercept = nrow(df_plot)+0.5, color = "black", size = .5) +
    geom_hline(yintercept = div_index, color = "black", size = .5, alpha = 0.5) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, max(df_plot$index) + 1.5)) +
    # scale_x_continuous(expand = c(0, 0)) + #, limits = c(0,1.02)) 
    coord_cartesian(clip="off") + #xlim(0, 1) +
    scale_colour_identity() +  scale_fill_identity() + 
    xlab(xlabel) + theme_classic() +  
    theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title = element_text(size = 7),
          axis.title.x = element_text(colour = "white"),
          axis.line.x.bottom = element_line(color = "white"),
          axis.ticks.x = element_line(color = "white"),
          axis.text.x = element_text(size = 7, color = "white"),
          plot.margin = unit(c(0, 0, 0, 0), "null"))
  gg_tbl
  # Fill in metadata columns
  for (n in seq_along(col_x_pos)) {
    gg_tbl <- gg_tbl + 
      geom_text(x = col_x_pos[n],
                label = c(df_meta[[meta_list[n]]][1:max(df_meta$index)-1], ""),
                hjust = 0.5 - as.numeric(n == 1)/2 + as.numeric(n == length(col_x_pos))/2,
                parse = FALSE, size = 2.2, color = c(rep("black", max(df_meta$index) - 1), "white"))
    gg_tbl <- gg_tbl +
      geom_text(x = col_x_pos[n], label = c(rep("", max(df_meta$index)-1), pretty_hdrs[n]),
                hjust = 0.5 - as.numeric(n == 1)/2 + as.numeric(n == length(col_x_pos))/2,
                parse = TRUE, size = 2.2)
  }
  gg_tbl
  
  # Arrange plot and table side by side
  # plot_grid(gg_plt, gg_tbl, align = "h", ncol = 2, rel_plot_widths = c(.6, .4))
  gg_grid_plot <- grid.arrange(gg_plt, gg_tbl, ncol = 2, widths = unit(rel_plot_widths, "npc")) 
  save_plot(paste(fig_path, '/', fig_name, sep = ""),
           gg_grid_plot, dpi = 600, base_height = ggsize[1], 
           base_width = ggsize[2])
 
  # Export table
  
  # Export table with certain columns removed
  
  # write_csv(file.path(proj_path, base_dir,  str_replace(fig_name,"[.].*$",".csv")))

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

