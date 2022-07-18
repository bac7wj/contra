



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