



# General strategy
# Use actual negative FC/RC values, but map them to -(positive values)

# Solution 1: Relative Change
#----------------------------------
# f(x) = (y-x)/x     [-1, Inf]
#Percent increase/decrease, 100% = 2x = double
#
# Problems:
#    Discontinuous function for transforming pos to neg values, is that fair?
#
# Pros
#    Preserves sign symmetry and linearity
df_rc = as.data.frame( matrix(c(
  0,      -1 + (2- 0.0)/2,
  0.01,   -1 + (2- 0.01)/2,   
  0.1,    -1 + (2- 0.1)/2,
  0.25,   -1 + (2- 0.25)/2,
  0.50,   -1 + (2- 0.5)/2,
  0.75,   -1 + (2- 0.75)/2,   
  # Discontinuity with equations, how to we join this to one equation?
  1,      -1 + 1/2,
  2,      -1 + 1/4,
  3,      -1 + 1/6,
  4,      -1 + 1/8,
  5,      -1 + 1/10,
  10,     -1 + 1/20,
  100,    -1 + 1/200
),  ncol = 2, dimnames = list(c(),c("pos","neg")), byrow = TRUE))
# Strategy 1: my own made up attempt at transform
relchange_p2n <- function(x) { x[x<1] <- -1 + (2-x[x< 1])/2;x[x>= 1] <- -1 + 1/(2*x[x>=1]); return(x) }
relchange_n2p <- function(x) { x[x<=-0.5] <- -1/(2*(x[x<=-0.5]+1)); x[x>-.5] <- 2 - 2*(x[x>-.5] + 1); return(x)}

# Strategy 2: Convert to FC, flip sign, and back
# Convert positive +RC to +FC, convert to -FC, then -RC
relchange_p2n <- function(x) { foldchange_p2n(x+1)-1}
relchange_p2n(df_rc$pos)
# Convert positive -RC to -FC, convert to +FC, then +RC
relchange_n2p <- function(x) { foldchange_n2p(x+1)-1}
relchange_n2p(relchange_p2n(df_rc$pos))

# Solution 2: Fold/Ratio Change
#----------------------------------
# f(x) = y/x     [0, Inf]
# Percent: 2 FC is 100% increase
#
# Problems: not % increase/decrease
#
# Pros
#    # Equations much simpler
df_fc = as.data.frame( matrix(c(
  1,      1,
  1.1,    1/1.1,
  1.2,    1/1.2,
  1.25,   1/1.25,
  1.5,    1/1.5,
  1.75,   1/1.75,
  2,      1/2,
  3,      1/3,
  4,      1/4,
  5,      1/5,
  10,     1/10,
  100,    1/100
),  ncol = 2, dimnames = list(c(),c("pos","neg")), byrow = TRUE))
foldchange_p2n <- function(x) 1/x 
foldchange_n2p <- function(x) 1/x 


