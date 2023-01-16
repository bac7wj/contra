



library(stringr)



fc_to_mfc <- function(x) {x[x < 1]<- -1/x[x < 1]; return(x)}
mfc_to_fc <- function(x) {x[x < 0]<- -1/x[x < 0]; return(x)}


contract1 <- function(x) {
  x1<-x
  x1[x <= -1] <- x1[x <= -1] + 1
  x1[x >=  1] <- x1[x >=  1] - 1
  x1[(x >  -1) & ((x <  1))] <- NaN
  return(x1)
}

rev_contract1 <- function(x) {
  x1<-x
  x1[x < 0] <- x1[x < 0] - 1
  x1[x >= 0] <- x1[x >= 0] + 1
  return(x1)
}



# Convert data from relative change to mirrored relative change 
mirror_rc <- function(x, forward = TRUE) {
  #' @description converts elements of numeric vector x from units of relative 
  #' change to units of mirrored relative change.
  #' 
  #' The equation for rc is:
  #' rc = (y-x)/x
  #' 
  #' @param x numeric vector x
  #' @param forward boolean for direction of conversion. TRUE: rc to mrc. 
  #' FALSE: mrc to rc.
  #' 
  #' @return x modified vector with converted elements.

  # Define conversion functions
  rc_pos_to_neg <- function(x)  { - 1/(x + 1) + 1}
  rc_neg_to_pos <- function(x)  { - 1/(x - 1) - 1}
  
  if (forward) {
    x[x<0] = rc_pos_to_neg(x[x<0]); 
  } else if (!forward) {
    x[x<0] = rc_neg_to_pos(x[x<0]);
  } else {
    stop(sprintf("Argument for direction: must be boolean", direction))
  }
  
  return(x);
}


# Fold Change to Mirrored Fold Change
mirror_fc <- function(x, forward = TRUE) {
  #' @description converts elements of numeric vector x from units of fold 
  #' change (fc) to units of mirrored fold change (mfc).
  #' 
  #' The equation for fc is:
  #' fc = y/x
  #' 
  #' fc is a measure of amount rather than change. An FC of 1 is no change.
  #' 
  #' @param x numeric vector x
  #' @param forward boolean for direction of conversion. TRUE: fc to mfc. 
  #' FALSE: mfc to fc.
  #' 
  #' @return x modified vector with converted elements.

  fc_to_mfc <- function(x) {x[x < 1]<- -1/x[x < 1]; return(x)}
  mfc_to_fc <- function(x) {x[x < 0]<- -1/x[x < 0]; return(x)}
  
  
  if (forward) {
    x1 <- fc_to_mfc(x)
  } else if (!forward) {
    x1 <- mfc_to_fc(x)
  }
  
  # # Define forward and reverse conversion functions
  # fc_pos_to_neg <- function(x) { -1/x }
  # fc_neg_to_pos <- function(x) { -1/x }
  # 
  # 
  # if (forward) {
  #   x[x<1] <- fc_pos_to_neg(x[x<1])
  # } else if (!forward) {
  #   x[x<0] <- fc_neg_to_pos(x[x<0])
  # } else {
  #   stop(sprintf("Argument for direction: must be boolean", direction))
  # }
  
  return(x1);
  
}



gg_revaxis_mfc<- function(gg, ax = "y", num_format = "decimal") {
  xlabs <- ggplot_build(gg)$layout$panel_params[[1]][[ax]]$get_labels()
  # Remove NAs (sometimes there are hidden empty ticks)
  xlabs <- xlabs[!is.na(xlabs)]
  
  x_breaks <- ggplot_build(gg)$layout$panel_params[[1]][[ax]]$get_breaks()
  x_breaks <- x_breaks[!is.na(x_breaks)]
  x_sigdigs = unname(sapply(xlabs, function(x) length(str_replace(x, "^[-0.]*",""))))
  
  new_xlabs_num <- rev_contract1(as.numeric(xlabs))
  
  # Convert axis labels to friendly output
  if (num_format == "decimal") {
    new_xlabs_num[new_xlabs_num<1] <-  -1/new_xlabs_num[new_xlabs_num<1] 
    new_x_sigdigs = unname(sapply(as.character(new_xlabs_num), function(x) length(str_replace(x, "^[-0.]*",""))))
    new_xlabs = rep("",length(new_xlabs_num))
    for (n in seq_along(new_xlabs_num)) {
      new_xlabs[n] <- sprintf(paste0("%.",x_sigdigs[n]+ as.numeric(x_breaks<0)[n],"g"),
                              new_xlabs_num[n])
    }
    
  } else if (num_format == "power") {
    new_xlabs <- as.character(new_xlabs_num)
    new_xlabs[new_xlabs_num<1] <- paste(as.character(abs(new_xlabs_num[new_xlabs_num<1])),"^-1",sep="")
    
    
  
    
    
  } else if (num_format == "fraction") {
    new_xlabs <- as.character(new_xlabs_num)
    new_xlabs[new_xlabs_num<1] <- paste("1/", as.character(abs(new_xlabs_num[new_xlabs_num<1])),sep="")
    
  } else {
    stop("num_format only supports: decimal, power, or fraction")
  }
  
 
  
  if (ax=='x') {
    gg2<- gg + scale_x_continuous(labels = parse(text = new_xlabs), breaks = x_breaks)
  } else if (ax=='y'){
    gg2<- gg + scale_y_continuous(labels = parse(text = new_xlabs), breaks = x_breaks)
  } else (stop("gg_revaxis_mfc:only x and y axis supported for input argument"))
  
  
  return(gg2)
}




