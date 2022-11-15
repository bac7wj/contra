
# MDM Package: most difference in means
#  Calculates the upper bounds of the mean of the effect size from a single distribution
# or difference between two distributions.

# Load package manager
if (!require("pacman")) {install.packages("pacman")}; library(pacman)
p_load(docstring)
# p_load(cubature)



posterior_norm_dm <- 
  function(mean_x, var_x, n_x, mean_y, var_y, n_y, relative = TRUE, conf.level = 0.95,
           sharedVar = FALSE, num_param_sims = 250/(1-conf.level), rand.seed = NA,
           return_cdf = TRUE, plot = FALSE) {
    #' Calculates empirical cumulative distribution function of posterior of raw
    #' or relative difference in means (or returns the raw monte carlo samples 
    #' with return_cdf)
    #' 
    #' @description
    #' 
    #' @param mean_x mean of measurements from X
    #' @param var_x variance of measurements from X
    #' @param n_x number of x measurements
    #' @param mean_y mean of measurements from Y
    #' @param var_y variance of measurements from Y
    #' @param n_y number of Y measurements
    #' @param conf.level confident level of credibility interval
    #' @param sharedVar Boolean for whether variance is assumed to be shared
    #' @return ecdf or raw monte carlo samples of posterior of raw or relative 
    #' difference in means
    #' @examples
    #' x <- rnorm(10,10,.1); y <- rnorm(10,15,.1); mdm_credint(x,y, rand.seed = 0);
    # save(list = ls(all.names = TRUE), file = "temp/mdm_credint.RData",envir = environment())
    # load(file = "temp/mdm_credint.RData")
    
    if (!is.na(rand.seed)) {set.seed(rand.seed)}
    
    if(sharedVar){
      shape_xy <- .5*(n_x + n_y - 2)
      scale_xy <- .5*((n_x-1)*var_x + (n_y-1)*var_y)
      var_xy_sims <- 1/rgamma(num_param_sims, shape = shape_xy, rate = scale_xy)
      mu_x_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(var_xy_sims/n_x))
      mu_y_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(var_xy_sims/n_y))
    }else{ # different variances
      shape_x <- .5*(n_x-1)
      scale_x <- .5*(n_x-1)*var_x
      shape_y <- .5*(n_y-1)
      scale_y <- .5*(n_y-1)*var_y
      var_x_sims <- 1/rgamma(n = num_param_sims, shape = shape_x, rate = scale_x)
      var_y_sims <- 1/rgamma(n = num_param_sims, shape = shape_y, rate = scale_y)
      mu_x_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(var_x_sims/n_x))
      mu_y_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(var_y_sims/n_y))
    }
    
    if(plot & !relative) {
      hist(mu_x_sims - mu_y_sims)
      abline(v=upper,col="red"); abline(v=-upper,col="red")
    } else if(plot & relative){
      hist((mu_x_sims - mu_y_sims)/mu_y_sims)
      abline(v=upper,col="red"); abline(v=-upper,col="red")
    }
    
    if (return_cdf) { # Return empirical distribution function
      if(!relative){ # Y-X
        cdf <- ecdf(mu_y_sims - mu_x_sims)
      }else{         # (Y-X)/X
        cdf <- ecdf((mu_y_sims- mu_x_sims)/mu_x_sims)
      } 
      return(cdf)
      
    } else { # Return monte Carlo Trials
      if(!relative){ # Y-X
        mct <- mu_y_sims - mu_x_sims
      }else{         # (Y-X)/X
        mct <- (mu_y_sims- mu_x_sims)/mu_x_sims
      } 
      return(mct)
    }
  }



dm_credint <- function(x,y, conf.level= 0.95, num_param_sims = 250/(1-conf.level), 
                       sharedVar = FALSE, relative = FALSE, rand.seed = NA) {
  #' @param x measurements from group x
  #' @param y measurements from group y
  #' @param conf.level confident level of credibility interval
  #' @param num_param_sims number of monte carlo samples
  #' @param sharedVar Boolean for whether variance is assumed to be shared
  #' @param relative flag whether relative or raw difference in mean is estimated
  #' @param sharedVar flag to assume shared variance for posterior of difference 
  #' in means
  #' @return credibility interval bounds for raw or relative difference in means
  
  if (!is.na(rand.seed)) {set.seed(rand.seed)}
  dm_bounds <- dm_credint_stats(mean_x = mean(x), var_x = var(x), n_x = length(x),
                                mean_y = mean(y), var_y = var(y), n_y = length(y),
                                relative = relative, conf.level = conf.level,
                                num_param_sims = num_param_sims, sharedVar = sharedVar, 
                                rand.seed = rand.seed)
  return(dm_bounds)
}


dm_credint_stats <- function(mean_x, var_x, n_x, mean_y, var_y, n_y, relative = TRUE,
                             conf.level, num_param_sims = 250/(1-conf.level),
                             sharedVar = FALSE, rand.seed =  NA) {
  #' @param x measurements from group x
  #' @param y measurements from group y
  #' @param conf.level confident level of credibility interval
  #' @param num_param_sims number of monte carlo samples
  #' @param sharedVar Boolean for whether variance is assumed to be shared
  #' @param relative flag whether relative or raw difference in mean is estimated
  #' @param sharedVar flag to assume shared variance for posterior of difference 
  #' in means
  #' @return credibility interval bounds for raw or relative difference in means
  
  if (!is.na(rand.seed)) {set.seed(rand.seed)}
  dm_sims <- posterior_norm_dm(mean_x = mean_x, var_x = var_x, n_x = n_x,
                               mean_y = mean_y, var_y = var_y, n_y = n_y,
                               relative = relative, conf.level = conf.level,
                               sharedVar = sharedVar, rand.seed = rand.seed,
                               num_param_sims = num_param_sims, return_cdf = FALSE)
  dm_b <- unname(quantile(dm_sims,c(1-conf.level/2, conf.level/2), type = 4))
  
  df <- data.frame(int_estimate = (mean_y-mean_x)/mean_x,int_lower = dm_b[1], 
                   int_upper = dm_b[2])
  return(df)
}


mdm_credint <- 
  function(x, y, relative = FALSE, conf.level = 0.95, num_param_sims = 250/(1-conf.level), 
           plot = FALSE, sharedVar = FALSE, rand.seed = NA){
    #' Upper credibility bound of most different in means statistic
    #' 
    #' @description calculates the (raw/relative) most difference in means, a 
    #' statistic that estimates the largest absolute difference in means supported
    #' by the data (based on credible interval)
    #' 
    #' @param x vector of measurements from group 1 (experiment)
    #' @param y vector of measurements from group 2  (control)
    #' @param conf.level confidence level
    #' @param num_param_sims number of monte carlo trials to calculate ldm
    #' @param plot flag to plot histogram of data
    #' @param relative flag to calculate relative or raw difference in means
    #' @return raw or relative difference in means
    #' @references https://arxiv.org/abs/2201.01239
    #' @examples
    #' x <- rnorm(10,10,.1); y <- rnorm(10,15,.1); mdm_credint(x,y, rand.seed = 0);
    # save(list = ls(all.names = TRUE), file = "temp/mdm_credint.RData",envir = environment())
    # load(file = "temp/mdm_credint.RData")
    
    if (!is.na(rand.seed)) {set.seed(rand.seed)}
    mdm <- mdm_credint_stats(mean_x = mean(x), var_x = var(x), n_x = length(x), 
                             mean_y = mean(y), var_y = var(y), n_y = length(y), 
                             sharedVar= sharedVar, num_param_sims = num_param_sims,
                             conf.level = conf.level)
    return(mdm)
  }


mdm_credint_stats <- 
  function(mean_x, var_x, n_x, mean_y, var_y, n_y, relative = TRUE, conf.level = 0.95,
           sharedVar = FALSE, num_param_sims = 250/(1-conf.level), plot = FALSE, 
           rand.seed = NA) {
    #' @param mean_x mean of measurements from X
    #' @param var_x variance of measurements from X
    #' @param n_x number of x measurements
    #' @param mean_y mean of measurements from Y
    #' @param var_y variance of measurements from Y
    #' @param n_y number of Y measurements
    #' @param conf.level confident level of credibility interval
    #' @param sharedVar Boolean, true assumes equal variance
    #' @return value of raw or relative most difference in means
    save(list = ls(all.names = TRUE), file = "temp/mdm_credint_stats.RData",envir = environment())
    # load(file = "temp/mdm_credint_stats.RData")
    # print(sprintf("x: %f+-%f, %i    y: %f+-%f, %i", mean_x,var_x,n_x,mean_y,var_y,n_y))
    dm_sims <- posterior_norm_dm(mean_x=mean_x, var_x = var_x, n_x = n_x,
                                 mean_y = mean_y, var_y = var_y, n_y = n_y,
                                 relative = relative, conf.level = conf.level,
                                 sharedVar = sharedVar,rand.seed = rand.seed,
                                 num_param_sims = num_param_sims, return_cdf = FALSE)
    upper <- unname(quantile(dm_sims,conf.level, type = 4))
    
    return(abs(upper))
  }



ldm_credint <- function(x, y, conf.level = 0.95, num_param_sims = 250/(1-conf.level), 
                        plot = FALSE, relative = FALSE, SharedVar = FALSE, 
                        keepSign = TRUE, rand.seed = NA) {
  #' @description calculates the (raw/relative) least difference in means, a 
  #' statistic that estimates the smallest difference in means supported by the 
  #' data. Uses credibility interval.
  #' 
  #' Equation:
  #' ldm <- sign(x_bar - mean_y) \* (sign(b_lo) == sign(b_hi)) \* max(abs( c(b_lo, b_hi) ))
  #'         original effect sign \* force zero if interval includes zero \* select closest bound to 0
  #' 
  #' Where b_lo and b_hi are credible bounds for u_dm whjen relative= FALSE and
  #'   r_u_dm when relative=TRUE
  #' 
  #' @param x vector of measurements from group a, experiment 1
  #' @param y vector of measurements from group a, experiment 1
  #' @param conf.level vector of measurements from group a, experiment 1
  #' @param num_param_sims vector of measurements from group a, experiment 1
  #' @param plot string of number label used for basename of all figures
  #' @param relative path to export figures to disk
  #' @param keepSign base name for exported figures
  #' @return value of ldm ro rldm
  #' @examples
  #' x <- rnorm(10,10,.1); y <- rnorm(10,15,.1); ldm_credint(x,y, rand.seed = 0);
  #' 
  save(list = ls(all.names = TRUE), file = "temp/ldm_credint.RData",envir = environment())
  # load(file = "temp/ldm_credint.RData")
  
  ldm <- ldm_credint_stats(mean_x = mean(x), var_x = var(x), n_x = length(x), 
                           mean_y = mean(y), var_y = var(y), n_y = length(y),
                           conf.level = conf.level, SharedVar = SharedVar, 
                           KeepSign = KeepSign, rand.seed = rand.seed) 
  return(ldm)
}


ldm_credint_stats <- function(mean_x, var_x, n_x, mean_y, var_y, n_y,
                              conf.level, SharedVar = FALSE, KeepSign, rand.seed =  NA) {
  #' @param mean_x mean of measurements from X
  #' @param var_x variance of measurements from X
  #' @param n_x number of x measurements
  #' @param mean_y mean of measurements from Y
  #' @param var_y variance of measurements from Y
  #' @param n_y number of Y measurements
  #' @param conf.level confident level of credibility interval
  #' @param sharedVar Boolean for whether variance is assumed to be shared
  #' @param KeepSign boolean whether sign of effect size should be preserved
  #' @return value of mdm or rmdm
  
  dm_sims <- posterior_norm_dm(mean_x=mean_x, var_x = var_x, n_x = n_x,
                               mean_y = mean_y, var_y = var_y, n_y = n_y,
                               relative = relative, conf.level = conf.level,
                               sharedVar = sharedVar, rand.seed = rand.seed,
                               num_param_sims = num_param_sims, return_cdf = FALSE)
  dm_b <- unname(quantile(dm_sims,c(1-conf.level, conf.level), type = 4))
  
  # Least difference in means
  ldm <- sign(sign(dm_b[1]) + sign(dm_b[2])) * min(abs( c(dm_b[1], dm_b[2]) ))
  
  
  # Keep sign of effect size if requested, since sign matters for practical sig.
  if (!keepSign) {ldm <- abs(ldm)}
  return(ldm) 
}




