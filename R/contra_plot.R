

if (!require("pacman")) {install.packages("pacman")}; library(pacman)
p_load(ggplot2)
p_load(gridExtra)
p_load(grid)
p_load(stringr)
p_load(cowplot)
source("R/mirrored_transforms.R")
source("R/contra.r")

abbreviate <- function(x, max.length=6) {
  x[nchar(x) > (max.length)+1] <- paste0(substr(x[nchar(x) > (max.length)+1],0,max.length),".")
  return(x)
}



pretty_numbers <- function(x, relative) {
  
  
  
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

  x <- sapply(1:length(x), function(n) pretty_number(x[n], relative = relative))
  return(x)
}

ldm_from_interval_bounds <- function(lo, hi) {
  # Restore sign of bounds  *  force zero if bounds flank zero * closest bound
  ldm <- sign(mean(c(lo,hi))) * (sign(lo)==sign(hi)) *  min(abs(c(lo,hi)))

  return(ldm)  
}

fract.as.numeric <- function(s) {
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
        dm_credint_stats(mean_x = df$mean_x[n], var_x = df$s_x[n]^2, n_x = df$n_x[n],
                         mean_y = df$mean_y[n], var_y = df$s_y[n]^2, n_y = df$n_y[n],
                        conf.level = 1-fract.as.numeric(df$alpha_dm[n]), relative = TRUE,
                        sharedVar = FALSE, rand.seed = 0)
          
    }
    # Calculate contra statistics
   df_interval <- do.call(rbind, conf_ints_list)
    # df_interval <- as.data.frame(matrix(unlist(bound_conf_ints), ncol = ncol(bound_conf_ints), 
    #                                      dimnames = list(NULL, colnames(bound_conf_ints))))
    
    # Saturate estimates beyond rdm<=-1
    df_interval$int_lower[df_interval$int_lower< -1] <- -1  
    
    df_interval$rldm <- sapply(1:nrow(df_interval), function(x) 
      ldm_from_interval_bounds(df_interval$int_lower[x], df_interval$int_upper[x]))

    df_interval$rmdm <- sapply(1:nrow(df), function(x) 
      mdm_credint_stats(mean_x = df$mean_x[x], var_x = df$s_x[x]^2, n_x = df$n_x[x],
                        mean_y = df$mean_y[x], var_y = df$s_y[x]^2, n_y = df$n_y[x],
                        conf.level = 1-fract.as.numeric(df$alpha_dm[n]), relative = TRUE,
                        sharedVar = FALSE, rand.seed = 0))
    return(df_interval)

  }




contra_plot <- function(df, signed_sort_colname = "rldm", col_x_pos = "auto", xlabel = "Relative Difference in Means",
                        ggsize = c(3, 6), fig_path = getwd(), fig_name = "contra_plot.png",
                        least_colname = c("rldm",'"Ls%"'), most_colname = c("rmdm",'"Ms%"'),
                        prune_colnames = NULL, estimate_label = "est", plot_title = "Measurement", tf_xlims = NULL,
                        relative = FALSE, estimate_colname = "estimate", rel_plot_widths = c(0.5,0.5),
                        null_sort_name = "rmdm", cum_col_x_pos_adj = rep(0,ncol(df)-3),
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
    
    # Split null into negative and positive null based on rdm estimate
    df_null_pos <- df_null[df_null$int_estimate >= 0,]
    df_null_pos <- df_null_pos[order(df_null_pos[[null_sort_name]], decreasing = FALSE),]
    df_null_neg <- df_null[df_null$int_estimate < 0,]
    df_null_neg <- df_null_neg[order(df_null_neg[[null_sort_name]], decreasing = TRUE),]
    
    df_pos <- df_pos[order(df_pos[[signed_sort_colname]], decreasing = FALSE),]
    
    df_plot <- rbind(df_neg, df_null_neg, df_null_pos, df_pos)
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
    
    x_sigdigs = unname(sapply(xlabs, function(x) length(str_replace(x, "^[-0.]*",""))))
    
    
    new_xlabs_num <- mirror_rc(as.numeric(xlabs), forward = FALSE)
    new_x_sigdigs = unname(sapply(as.character(new_xlabs_num), function(x) length(str_replace(x, "^[-0.]*",""))))
    new_xlabs = rep("",length(new_xlabs_num))
    
    for (n in seq_along(new_xlabs_num)) {
      new_xlabs[n] <- sprintf(paste0("%.",x_sigdigs[n]+ as.numeric(x_breaks<0)[n],"g"), new_xlabs_num[n])
    }
   
    gg_plt <- gg_plt + scale_x_continuous(labels = new_xlabs, breaks = x_breaks)
    gg_plt
  }
 
  # List of columns to remove from dataframe
  removed_cols <- 
    c("int_estimate", "int_lower", "int_upper", "tf_lower", "color","index",
      "tf_upper", "tf_estimate", "study_id", least_colname[1], most_colname[1])
  
  # Initialize metadata dataframe for table of metadata
  meta_list <-
    c(least_colname[1], most_colname[1],
      colnames(df_plot)[!is.element(colnames(df_plot), removed_cols)])
  df_meta <- df_plot[,meta_list]
  pretty_colnames <-c(least_colname[2], most_colname[2], 
                      tools::toTitleCase(meta_list[3:length(meta_list)]))
  # Pretty print numbers
  df_meta[[least_colname[1]]] <-pretty_numbers(df_plot[[least_colname[1]]], relative = relative)
  df_meta[[most_colname[1]]] <-pretty_numbers(df_plot[[most_colname[1]]], relative = relative)

  
  # Prune any requested columns
  if (!is.null(prune_colnames)) {
    df_meta <- df_meta[ , !(names(df_meta) %in% prune_colnames)]
    meta_list <- setdiff(meta_list, prune_colnames)
    # df_meta <- subset(df_meta, select = -prune_colnames)
  }
  # browser()
  
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

