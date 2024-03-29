
mdm_confint <- function(x, y, conf.level = 0.95, num_param_sims = 250/(1-conf.level), 
                        plot=FALSE, relative = FALSE){
  # save(list = ls(all.names = TRUE), file = "temp/mdm_credint.RData",envir = environment())
  # load(file = "temp/mdm_credint.RData")
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  m <- length(x)
  n <- length(y)
  
  
  mu_x_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(var_x / m))
  mu_y_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(var_y / n))
  # shape_x <- .5*(m-1)
  # scale_x <- .5*(m-1)*var_x
  # shape_y <- .5*(n-1)
  # scale_y <- .5*(n-1)*var_y
  # var_x_sims <- 1/rgamma(n = num_param_sims, shape = shape_x, rate = scale_x)
  # var_y_sims <- 1/rgamma(n = num_param_sims, shape = shape_y, rate = scale_y)
  # mu_x_sims <- rnorm(n = num_param_sims, mean = mean_x, sd = sqrt(var_x_sims/m))
  # mu_y_sims <- rnorm(n = num_param_sims, mean = mean_y, sd = sqrt(var_y_sims/n))
  
  # mu_x_sims <-rowMeans(matrix(rnorm(n = m*num_param_sims, mean = mean_x, sd = sqrt(var_x)), ncol = m))
  # mu_y_sims <-rowMeans(matrix(rnorm(n = n*num_param_sims, mean = mean_y, sd = sqrt(var_y)), ncol = n))
  
  if(!relative){
    cdf <- ecdf(mu_x_sims - mu_y_sims)
  } else {
    cdf <- ecdf((mu_x_sims - mu_y_sims)/mu_y_sims)
  }
  # solve $F(c) - F(-c) = .95
  upper <- uniroot(function(x){ cdf(x) - cdf(-x) - conf.level},
                   lower = 0,
                   upper = max(c(abs(x),abs(y))),
                   extendInt = "yes")$root
  if(plot & !relative){
    hist(mu_x_sims - mu_y_sims)
    abline(v=upper,col="red")
    abline(v=-upper,col="red")
  }else if(plot & relative){
    hist((mu_x_sims - mu_y_sims)/mu_y_sims)
    abline(v=upper,col="red")
    abline(v=-upper,col="red")
  }
  return(abs(upper))
}




mdm_tdist <- function(x, y = NULL, conf.level = 0.95) {
  #' @description Calculate most difference in means statistic from integrating 
  #' over a folded t-distribution to an extent dicated by the conf.level
  #'
  #' @param x measurements from first group
  #' @param y measurements from second group (optional)
  #' @param conf.level confidence level for statistics (default 0.95)
  #' @return Returns most difference in means (single value)
  #' @usage mdm_tdist(x, y, conf.level = 0.95)
  #' @examples
  #' x <- rnorm(n=3,mean=0,sd=1); y <- rnorm(n=3,mean=0,sd=1);
  #' mdm_tdist(x,y, conf.level = 0.95)
  
  # save(list = ls(all.names = TRUE), file = "temp/mdm_tdist.RData",envir = environment())
  # load(file = "temp/mdm_tdist.RData")
  
  # Calculate basic stats x and y
  n_x <- length(x); n_y <- length(y)
  sd_x <- sd(x); sd_y <- sd(y)
  
  # Calculate difference in means stats
  if (is.null(y)) {
    # 1-sample case
    # Degrees of freedom and upper search bound for quantile function calculated 
    # from built-in welch's t-test
    wtt <- t.test(x=x,y=NULL, var.equal = FALSE, conf.level = 1-(1-conf.level)/4)
    df_d <- wtt$parameter
    
    mean_x_dm <- mean(x)
    sd_d <- sd_x
    sd_dm = sd_x / sqrt(n_x)
    
  } else {
    # 2-sample case
    # Degrees of freedom and upper search bound for quantiel function calculated 
    # from built-in welch's t-test
    wtt <- t.test(x=y,y=x, var.equal = FALSE, conf.level = 1-(1-conf.level)/4)
    df_d <- wtt$parameter
    
    mean_x_dm <- mean(y) - mean(x)
    sd_d <- sqrt( (( n_x - 1) * sd_x^2  +  (n_y - 1) * sd_y^2 ) / df_d)
    sd_dm = sqrt( sd_x^2 / n_x  + sd_y^2 / n_y)
  }
  
  # Calculate search bounds for mdm, used for uniroot call
  lo.bound= abs(mean_x_dm)
  hi.bound = max(abs(wtt$conf.int))
  if (lo.bound>hi.bound) {browser();}
  
  # Quantile with prob set to alpha and sample estimates as parameters
  mdm <-  tryCatch(
    qft(p = conf.level, df = df_d, mu = mean_x_dm, sigma = sd_dm,
        lo.bound, hi.bound),
    error = function(c) NaN)
  
  # If integration fails pause execution
  if (is.nan(mdm) || is.null(mdm)) {browser();}
  
  return(mdm)
}




macb_tdist_2sample <- function (x, y, conf.level = 0.95) {
  #' Most absolute one-tailed confidence bounds of t-distribution
  #' 
  lo_b <- t.test(x = x, y = y, conf.level = conf.level, alternative = "greater")$conf.int[1]
  up_b <- t.test(x = x, y = y, conf.level = conf.level, alternative = "less")$conf.int[2]
  
  macb <- max(abs(c(lo_b, up_b)))
  
  return(macb)
}







rmdm_tdist <- function(x_ctrl, y_exp, conf.level = 0.95, 
                       verbose = FALSE,  var.equal = FALSE, method = "fieller")  {
  #' @description Calculates the relative most difference in means assuming with
  #' rmdm = mdm/X, X being the control group and Y the experimental
  #' 
  #' @param x_ctrl vector of measurements in control group
  #' @param y_exp vector of measurements in experimental group
  #' @param conf.level significance level for calculating upper mdm
  #' 
  #' @return relative most difference in means
  
  # Equation for pooled variance taken from:
  # https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals5.html
  
  # Calculate basic sample statistics
  mean_ctrl = mean(x_ctrl)
  sd_ctrl = sd(x_ctrl)
  n_ctrl = length(x_ctrl)
  se_ctrl = sd_ctrl/sqrt(n_ctrl)
  
  mean_exp = mean(y_exp)
  sd_exp = sd(y_exp)
  n_exp = length(y_exp)
  se_exp = sd_exp/sqrt(n_exp)
  
  mean_dm = mean_exp - mean_ctrl
  df <- n_ctrl + n_exp - 2
  
  sSquared <- (sum((x_ctrl - mean_ctrl)^2) + sum((y_exp - mean_exp)^2))/df
  
  fieller_int <- tryCatch(get_FiellerInterval(mean_ctrl, mean_exp, sSquared, 
                                              1/n_ctrl, 1/n_exp, df, v12 = 0, alpha=1-conf.level), 
                          error = function(c) data.frame(upper = NaN, lower = NaN))
  
  rmdm <- max(abs(c(fieller_int$lower,fieller_int$upper)))
  
  
  return(rmdm)
}





ldm_tdist <- function(x, y = NULL, conf.level = 0.95) {
  #' @description Calculate least difference in means statistic from integrating 
  #' over a folded t-distribution to an extent dictated by (1-conf.level)
  #'
  #' @param x measurements from first group
  #' @param y measurements from second group (optional)
  #' @param conf.level confidence level for statistics (default 0.95)
  #' @return Returns most difference in means (single value)
  #' @usage mdm_tdist(x, y, conf.level = 0.95)
  #' @examples
  #' x <- rnorm(n=6,mean=0,sd=1); y <- rnorm(n=6,mean=0,sd=1);
  #' ldm_tdist(x,y, conf.level = 0.95)
  
  # save(list = ls(all.names = TRUE), file = "temp/mdm_tdist.RData",envir = environment())
  # load(file = "temp/mdm_tdist.RData")
  
  # Calculate basic stats x and y
  n_x <- length(x); n_y <- length(y)
  sd_x <- sd(x); sd_y <- sd(y)
  
  # Calculate difference in means stats
  if (is.null(y)) {
    # 1-sample case
    # Degrees of freedom and upper search bound for quantile function calculated 
    # from built-in welch's t-test
    wtt <- t.test(x=x,y=y, var.equal = FALSE, conf.level = 1-(1-conf.level)/4)
    df_d <- wtt$parameter
    
    mean_x_dm <- mean(x)
    sd_d <- sd_x
    sd_dm = sd_x / sqrt(n_x)
    
  } else {
    # 2-sample case
    # Degrees of freedom and upper search bound for quantiel function calculated 
    # from built-in welch's t-test
    wtt <- t.test(x=y,y=x, var.equal = FALSE, conf.level = 1-(1-conf.level)/4)
    df_d <- wtt$parameter
    
    mean_x_dm <- mean(y) - mean(x)
    sd_d <- sqrt( (( n_x - 1) * sd_x^2  +  (n_y - 1) * sd_y^2 ) / df_d)
    sd_dm = sqrt( sd_x^2 / n_x  + sd_y^2 / n_y)
  }
  
  # Calculate search bounds for ldm, used for uniroot call
  lo.bound <- 0
  hi.bound <- abs(mean_x_dm)
  if (lo.bound>hi.bound) {browser();}
  
  
  # Calculate lower bound of mu_dm
  lo.bound <- abs(mean_x_dm) - qt(conf.level, df=df_d)*sd_dm
  # Equivalent to:
  # wtt <-t.test(x=y,y=x, var.equal = FALSE, conf.level = 1-2*(1-conf.level))
  if (lo.bound < 0) {
    ldm <- 0
  } else {
    # Lower quantile of folded normal
    ldm <-  tryCatch(qft(p = 1-conf.level, df = df_d, mu = mean_x_dm, sigma = sd_dm,
                         lo.bound, hi.bound), error = function(c) NaN)
  }
  
  # If integration fails pause execution
  if (is.nan(ldm) || is.null(ldm)) {browser();}
  
  return(ldm)
}



lacb_tdist_2sample <- function (x, y, conf.level = 0.95) {
  #' Least absolute one-tailed confidence bounds of t-distribution
  #' 
  lo_b <- t.test(x = x, y = y, conf.level = conf.level, alternative = "greater")$conf.int[1]
  up_b <- t.test(x = x, y = y, conf.level = conf.level, alternative = "less")$conf.int[2]
  
  lacb <- min(abs(c(lo_b, up_b)))
  if (sign(lo_b)!= sign(up_b)) {lacb <- 0}
  
  return(lacb)
}


rldm_tdist <- function(x_ctrl, y_exp, conf.level = 0.95, 
                       verbose = FALSE,  var.equal = FALSE)  {
  #' @description Calculates the relative most difference in means assuming in 
  #' the form of rldm = (y_exp - x_ctrl)/x_ctrl
  #' 
  #' @param x_ctrl vector of measurements in control group
  #' @param y_exp vector of measurements in experimental group
  #' @param conf.level significance level for calculating ldm
  #' 
  #' @return relative least difference in means
  # Equation for pooled variance taken from:
  # https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals5.html
  
  # Calculate basic sample statistics
  mean_ctrl = mean(x_ctrl); sd_ctrl = sd(x_ctrl)
  n_ctrl = length(x_ctrl);se_ctrl = sd_ctrl/sqrt(n_ctrl)
  
  mean_exp = mean(y_exp); sd_exp = sd(y_exp)
  n_exp = length(y_exp); se_exp = sd_exp/sqrt(n_exp)
  
  mean_dm = mean_exp - mean_ctrl
  df <- n_ctrl + n_exp - 2
  
  sSquared <- (sum((x_ctrl - mean_ctrl)^2) + sum((y_exp - mean_exp)^2))/df
  
  fieller_int <- tryCatch(get_FiellerInterval(mean_ctrl, mean_exp, sSquared, 
                                              1/n_ctrl, 1/n_exp, df, v12 = 0, alpha=1-conf.level), 
                          error = function(c) data.frame(upper = NaN, lower = NaN))
  
  # RLDM is the bounds closest to zero, or zero of the bounds flank zero
  rldm <- min(abs(c(fieller_int$lower,fieller_int$upper))) * 
    (sign(fieller_int$lower)== sign(fieller_int$upper))
  
  return(rldm)
}




dft <- function(x, df, mu, sigma) {
  #' @description: Probability density function for noncentral folded t-distribution
  #' Equation from:
  #'  https://en.wikipedia.org/wiki/Folded-t_and_half-t_distributions
  #' @param x 
  #' @param df degree of freedom
  #' @param mu mean
  #' @param sigma standard deviation
  # save(list = ls(all.names = TRUE), file = "temp/dft.RData",envir = environment())
  # load(file = "temp/dft.RData")
  
  v = df
  t1 <- (1 +  (1/v) * ((x - mu)^2 / sigma^2 )) ^ ( -(v+1) / 2)
  t2 <- (1 +  (1/v) * ((x + mu)^2 / sigma^2 )) ^ ( -(v+1) / 2)
  
  d <- gamma( (v+1)/2 ) / (gamma(v/2) * sqrt(v*pi*sigma^2) ) * (t1 + t2)
  # d is zero for all x<0 by definition, so force d to 0 when x is negative
  d[x < 0] <- 0
  
  return(d)
}

pft <- function(upper, df, mu, sigma, lower = -Inf) {
  #' @description: cumulative distribution function for noncentral folded 
  #' t-distribution
  #' @param upper upper bounds of integration
  #' @param df degree of freedom
  #' @param mu mean
  #' @param sigma standard deviation
  #' @param lower lower bounds of integration, default -InF
  # save(list = ls(all.names = TRUE), file = "temp/pft.RData",envir = environment())
  # load(file = "temp/pft.RData")
  
  d <- integrate(f = function(y) dft(y, df, abs(mu),sigma),
                 lower = lower, upper = upper,
                 rel.tol = 1e-8, abs.tol = 1e-9)$value
  
  return(d)
}


qft <- function(p, df, mu, sigma, lo.bound, hi.bound) {
  #' @description Quantile function for folded t-distribution
  #'
  #' @param p probability
  # save(list = ls(all.names = TRUE), file = "temp/qft.RData",envir = environment())
  # load(file = "temp/qft.RData")
  
  # Integration is also used to calculate p, but it can be a very sparse 
  # integration (long spans where f(x)=0 and then a very small width spike in the pdf)
  # To deal with this, integration is broken up into separate intervals to make 
  # sure the spike is not missed
  start_area = tryCatch(
    pft(upper = max(c(lo.bound - 10*sigma,0)), df = df, mu = abs(mu), 
        sigma = sigma, lower = 0) +
      pft(upper = lo.bound, df = df, mu = abs(mu), 
          sigma = sigma, lower = max(c(lo.bound - 10*sigma,0))),
    error = function(c) NaN)
  
  # Then we integrate from mean_x_dm and above to for the root, added the start_area
  # to the integration
  xroot <-  tryCatch(
    uniroot( function(z)
      pft(upper = z, df = df, mu = abs(mu), sigma = sigma, lower = lo.bound) + 
        start_area - p,
      lower = lo.bound, upper = hi.bound, extendInt = "no")$root,
    error = function(c) NaN)
  
  return(xroot)
}

get_FiellerInterval <- function(mean_x, mean_y, sSquared, v11, v22, f, v12 = 0, alpha=.025){
  tQuantile <- qt(1-alpha, f)
  A <- mean_x^2 - v11*sSquared*tQuantile^2
  if(A <= 0)
    stop("confidence interval not available (unless you are okay with two disjoint intervals)")
  B <- -2*((mean_y - mean_x)*mean_x + sSquared*tQuantile^2*(v11 - v12))
  C <- (mean_y - mean_x)^2 - sSquared*tQuantile^2*(v22 + v11 - 2*v12)
  discriminant <- B^2 - 4*A*C
  if(discriminant <= 0)
    stop("confidence interval not available (complex-valued)")
  center <- -B/2/A
  width <- sqrt(discriminant)/2/A
  list(lower = center - width, upper = center + width)
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
