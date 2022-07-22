



# y = c(.1, .25, 1/3, 0.5, 0.75, 1.25, 1.5, 2+ 2/3, 4,5, 10)

# (y-1)/1



a = c(1/c( 10, 5, 3, 2), 2/3, .9)
b =c(1.1,1.5,2,3,5,10)
# rdm = (y-x)/x 
rdm = c(a,b) 


rel_change = rdm-1


# x = x0-1

rdm_goal = c(rev(-c(1.1,1.5,2,3,5,10)),c(1.1,1.5,2,3,5,10))


# Fold product: x0 * f


fc_prod <- function(x0) {
  x <- x0
  x[x0 < 1] = -1/x[x0 < 1]
  x[x0 > 1] = x[x0 > 1]
  return(x)
}


fc_prod <- function(x) {x[x < 1] = -1/x[x < 1]; return(x)}



















fc_prod <- function(x) {x[x<0] = -1/(x[x < 0] + 1); x[x > 0] = x[x > 0] - 1; return(x)} 





fc_stretch = function(x)     {x[x<0]<- 1/(-1-x[x<0]); return(x)}
fc_stretch_rev = function(x) {x[x<0]<- -1/x[x<0] -1; return(x)}


y = c(0, .01, .1,.2, .25, .5, .666666, .75, .9, .99, 1, 1.01, 1.1, 1.25, 1.3333, 1.5, 2, 3, 5, 6, 11, 101)




rc_stretch = function(x)     {x[x<0] <- 1/(-1-x[x<0])+1; return(x)}
rc_stretch_rev = function(x) {x[x<0] <- -(1/(x[x<0]-1)+1); return(x)}


fc_stretch <- function(x) {x[x<1] <- 1/(x[x<1]); return(x)}


#                               x   x     x     x   x          
df_rc = data.frame(pos =  c(0, .01, .1, 0.25, 0.5, .75, 2  , 3  , 4), 
                   neg = -c(0, .01, .1, 0.25, 0.5/2, .75/2, 1/2, 1/3, 4))



# Percent increase/decrease, 100% = 2x = double

# Problem: linearity between + and - not the same
# Pos: 2x of 1 is 2
# Neg: 2x of  
df = as.data.frame( matrix(c(
  0,      -0,
  0.01,   -0.01 /2,   # if pos >= 1, neg = 1/(pos+1)
  0.1,    -0.10 /2,
  0.25,   -0.50 /2,
  0.50,   -0.25 /2,
  0.75,   -0.75 /2,   
  1,      -1    /2,      # if pos >= 1, neg = 1/(pos+1)
  2,      -1    /4,
  3,      -1    /6,
  4,      -1    /8,
  5,      -1    /10,
  10,     -1    /20,
  100,    -1    /200
),  ncol = 2, dimnames = list(c(),c("pos","neg")), byrow = TRUE))
f_p2n <- function(x)  {x[x<=1] =  -x[x<=1]/2; x[x>1] = -1/(2*x[x>1]); return(x)}


# Calcute FC
# Caculate mirrored fc
# Map fc values to mirrored FC



mirror_fc <- function(y,x) sign(y-x)*abs(y-x)/x

# increase decrease
y = c(c(.9, .8, .75, .5, .2, .1), 0, c(.1, .25, 3, 6, 11))




f_p2n <- function(x) {x = (2*x)^sign(x); return(x)}



f_p2n <- function(x)  {x <- -min(x,1)/(2*ceiling(x)); return(x);}



fx <- function(x)  x / (ceiling(x)+1)


# Percent increase/decrease, 100% = 2x = double

# fx = (y-x) / x



# pos    neg_val    #neg map
df = as.data.frame( matrix(c(
  0,      -0,        0.0,
  0.01,   -0.01,    -0.01,
  0.1,    -0.10,    -0.10,
  0.25,   -0.25,    -0.25,
  0.50,   -0.50,    -0.5,
  0.75,   -0.75,    -0.75,
  1,      -1/1,     -1,
  2,      -1/2,     -2,
  3,      -1/3,     -3,
  4,      -1/4,     -4,
  5,      -1/5,     -5,
  10,     -1/10,    -10,
  100,    -1/100    -100,
),  ncol = 2, dimnames = list(c(),c("pos","neg")), byrow = TRUE))




# Fold change: Y/x
# Relative Change : (Y-X)/X

df_comp <- data.frame(x = 1, y=y, fc = y/1, rc = (y-1)/1)

df_comp$ratio_inv <- ratio_inv(df_comp$ratio)
df_comp


df_comp$fc_stretch <- fc_stretch(df_comp$fc)
df_comp$fc_stretch_rev <- fc_stretch_rev(fc_stretch(df_comp$fc))
df_comp



-1/.01-1
# FC 
# (20-80)/80
# [1] -0.75


