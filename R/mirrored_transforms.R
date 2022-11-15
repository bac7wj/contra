



library(stringr)



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
  rc_pos_to_neg <- function(x) {-( 1/(x + 1) - 1)}
  rc_neg_to_pos <- function(x) {-(1 + 1/(x - 1) )}
  
  
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

  # Define forward and reverse conversion functions
  fc_pos_to_neg <- function(x) { -1/x }
  fc_neg_to_pos <- function(x) { -1/x }
  

  if (forward) {
    x[x<1] <- fc_pos_to_neg(x[x<1])
  } else if (!forward) {
    x[x<0] <- fc_neg_to_pos(x[x<0])
  } else {
    stop(sprintf("Argument for direction: must be boolean", direction))
  }
  
  return(x);
  
}
