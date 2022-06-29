

library(ggplot2)
# library(patchwork)
library(gridExtra)
library(grid)
library(stringr)

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


# https://stackoverflow.com/questions/56640426/add-table-corresponding-to-y-axis-outside-the-plot33

df = data.frame(study = c("Auckland", "Block", "Doran", "Gamsu", "Morrison", "Papageorgiou", "Tauesch"),
                control = c("36", "1", "4", "14", "3", "1", "8"),
                treatment = c("60", "5", "11", "20", "7", "7", "10"),
                estimate  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017),
                lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831))
# df$study = factor(study)

contra_plot <- function(data=df, study, interval_type = "conf", xlabel = "Relative Diff Means") {

  
  #https://stackoverflow.com/questions/62246541/forest-plot-with-table-ggplot-coding
  
  ## Method that works
  df_plot <- df
  # df_plot[nrow(df) + c(1,2),] <- rep(NA, ncol(df))
  df_plot$index <- 1:nrow(df_plot)
  df_plot$color <- rep(c("white", "gray95"), nrow(df_plot))[1:nrow(df_plot)]
  df_plot$study <- factor(df_plot$study)
  # df_plot[nrow(df_plot) + 1,] <- rep(NA, ncol(df_plot))
  # df_plot$index[nrow(df_plot)] <- nrow(df_plot)
  
  
  # df_plot$index[nrow(df_plot)+1]<-nrow(df_plot)+1
    
  df_meta = subset( df_plot, select = -c(lower,upper) )
  df_meta[nrow(df_meta) + 1,] <- rep(NA, ncol(df_meta))
  df_meta$index[nrow(df_meta)] <- nrow(df_meta)
  levels(df_meta$study) <- c(levels(df_meta$study),str_to_title(colnames(df_meta)[1]))
  df_meta[nrow(df_meta),1:(ncol(df_meta)-2)] <- str_to_title(colnames(df_meta)[1:(ncol(df_meta)-2)])
  
  p <- ggplot(df_plot, aes(x = estimate, y = index, xmin = lower, xmax = upper)) +
    # geom_hline(aes(yintercept = index, color = color), size = 20) + 
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = index-0.5, ymax = index+0.5, fill = color)) +
    geom_pointrange(shape = 22, fill = "black") +
    geom_vline(xintercept = 1, linetype = 3) +
    xlab("Relative Difference in Means") +
    ylab("Adjusted Relative Risk with 95% Confidence Interval") +
    theme_classic() + 
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, nrow(df_plot)+1.5)) +
    scale_color_identity() + scale_fill_identity() 
    # theme(axis.text.y = element_blank(), axis.title.y = element_blank(), 
          # axis.line.y = element_blank())
  p

  data_table <- ggplot(data = df_meta, aes(y = index)) +
    # geom_hline(aes(yintercept = study, color = df_plot$color), size = 20) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, 
                  ymin = index-0.5, 
                  ymax = index+0.5, fill = color)) +
    geom_text(aes(x = 0, label = study), hjust = 0) +
    geom_text(aes(x = 5, label = control)) +
    geom_text(aes(x = 7, label = treatment), hjust = 1) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, nrow(df_meta)+0.5)) +
    scale_colour_identity() +  scale_fill_identity() +
    theme_void() +
    theme(plot.margin = margin(5, 0, 35, 0))
  data_table
  
  
 g <- grid.arrange(p,data_table, ncol = 2)
  
  
  
  
# Method 1: cannot align table with top and bottom of chart
df_plot <- df
# df_plot[nrow(df) + c(1,2),] <- rep(NA, ncol(df))
df_plot$index <- 1:nrow(df_plot)
df_meta = subset( df, select = -c(lower,upper) )
gg_plt <- ggplot(data = df_plot, aes(y = index, x = estimate, xmin = lower, xmax = upper)) +
    geom_point() + 
    geom_errorbarh(height = 0.1) +
    expand_limits(y = ncol(df_plot) + 1) +
    xlab(xlabel) + ylab("") +
    theme_classic() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.line.y = element_blank() ) 
print(gg_plt)

adjustTheme <- ttheme_default(
  core = list(fg_params=list(cex = 0.8)),
  colhead = list(fg_params=list(cex = 0.7)),
  rowhead = list(fg_params=list(cex = 0.7)))


tableObject = tableGrob(df_meta,theme = gridExtra::ttheme_default(
  core = list(padding=unit(c(5, 39), "pt"))))
# tableObject$widths <- unit(rep(1/ncol(tableObject), ncol(tableObject)), "npc")
# tableObject$heights <- unit(rep(1/nrow(tableObject), nrow(tableObject)), "npc")
grid.arrange(arrangeGrob(tableObject, gg_plt, ncol = 2,widths = c(1/2,1/2)))




# Method 1.2: cannot align table with top and bottom of chart
df_plot <- df
# df_plot[nrow(df) + c(1,2),] <- rep(NA, ncol(df))
df_plot$index <- 1:nrow(df_plot)
df_meta = subset( df, select = -c(lower,upper) )
gg_plt <- ggplot(data = df_plot, aes(y = index, x = estimate, xmin = lower, xmax = upper)) +
  geom_point() + 
  geom_errorbarh(height = 0.1) +
  expand_limits(y = ncol(df_plot) + 1) +
  xlab(xlabel) + ylab("") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank() ) 

gtable_table(df_meta)
g <- ggplotGrob(gg_plt)

tableObject = tableGrob(df_meta,theme= adjustTheme)
tableObject$widths <- unit(rep(1/ncol(tableObject), ncol(tableObject)), "npc")
tableObject$heights <- unit(rep(1/nrow(tableObject), nrow(tableObject)), "npc")
grid.arrange(arrangeGrob(tableObject, gg_plt, ncol = 2,widths = c(1/2,1/2)))







# Method 2: cow_plot : can't align
df_plot <- df
# df_plot[nrow(df) + c(1,2),] <- rep(NA, ncol(df))
df_plot$index <- 1:nrow(df_plot)
df_meta = subset( df, select = -c(lower,upper) )
gg_plt <- ggplot(data = df_plot, aes(y = index, x = estimate, xmin = lower, xmax = upper)) +
  geom_point() + 
  geom_errorbarh(height = 0.1) +
  expand_limits(y = ncol(df_plot) + 1) +
  xlab(xlabel) + ylab("") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank() ) 
print(gg_plt)
tableObject = tableGrob(df_meta,theme= adjustTheme)
tableObject$widths <- unit(rep(1/ncol(tableObject), ncol(tableObject)), "npc")
tableObject$heights <- unit(rep(1/nrow(tableObject), nrow(tableObject)), "npc")

grid.arrange(arrangeGrob(cowplot::plot_grid(gg_plt, tableObject, align = "v", ncol=2)))





adjustTheme <- ttheme_default(
  core = list(fg_params=list(cex = 0.8)),
  colhead = list(fg_params=list(cex = 0.7)),
  rowhead = list(fg_params=list(cex = 0.7)))

grid.arrange(arrangeGrob(tableObject, gg_plt, ncol = 2,widths = c(1/2,1/2)))



# Method 2: cannot align table with top and bottom of chart

data_table <- ggplot(df_meta, aes(x = study, y = factor(City),
                              label = format(value, nsmall = 1), colour = City)) +
  geom_text(size = 3.5) + theme_bw() + scale_y_discrete(formatter = abbreviate,
                                                        limits = c("Minneapolis", "Raleigh", "Phoenix")) +
  opts(panel.grid.major = none, legend.position = "none",
       panel.border = none, axis.text.x = none,
       axis.ticks = none) + opts(plot.margin = unit(c(-0.5,
                                                      1, 0, 0.5), "lines")) + xlab(NULL) + ylab(NULL)



# Method 3: qplot
library(ggplot2)
library(gridExtra)
library(grid)
tg <- tableGrob(df_meta, rows=NULL)
tg$widths <- unit(rep(1/ncol(tg),ncol(tg)),"npc")
tg$heights <- unit(rep(1/nrow(tg),nrow(tg)),"npc")

gg_plt <- ggplot(data = df_plot, aes(y = index, x = estimate, xmin = lower, xmax = upper)) +
  geom_point() + 
  geom_errorbarh(height = 0.1) +
  expand_limits(y = ncol(df_plot) + 1, x = 5) +
  xlab(xlabel) + ylab("") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(), axis.line.x = element_blank()) +
  annotation_custom(ymin=-Inf, ymax=Inf, xmin=3, xmax=5, tg) 
  

print(gg_plt)






}





df_plot <- df
# df_plot[nrow(df) + c(1,2),] <- rep(NA, ncol(df))
df_plot$index <- 1:nrow(df_plot)
df_meta = subset( df, select = -c(lower,upper) )

gg_plt <- ggplot(data = df_plot, aes(y = index, x = estimate, xmin = lower, xmax = upper)) +
  geom_point() + 
  geom_errorbarh(height = 0.1) +
  expand_limits(y = ncol(df_plot) + 1) +
  xlab(xlabel) + ylab("") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.line.y = element_blank() )



tab_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), ## centering title on text
        axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
        axis.line=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())


tab1 <- tab_base + 
  geom_text(aes(x=1, label=n)) + 
  ggtitle("n")

tab2 <- tab_base +
  geom_text(aes(x=1, label=total)) + 
  ggtitle("total")



adjustTheme <- ttheme_default(
  core = list(fg_params=list(cex = 0.8)),
  colhead = list(fg_params=list(cex = 0.7)),
  rowhead = list(fg_params=list(cex = 0.7)))

tableObject = tableGrob(df_meta,theme= adjustTheme)
tableObject$widths[-1] <- rep(unit(1/2,"null"), 4)
tableObject$heights <- unit(rep(1/nrow(tableObject), nrow(tableObject)), "npc")


grid.arrange(arrangeGrob(tableObject, gg_plt, ncol = 2,widths = c(1/2,1/2)))








