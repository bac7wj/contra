


y_rats <- c(10.1/10, 11/10, 12.5/10, 15/10, 2, 4, 5, 10, 20, 30, 50, 100)


df_rc = data.frame(rc=c(1,1.5,2,4,5,7.5,10,20))
df_rc$nrc = -(1-1/(2*df_rc$rc))

plot(c(0,df_rc$rc), c(0,df_rc$nrc))




df_rc = data.frame(rc=c(.001,.01,.1,.25,.5,.75, 1,1.5,2,4,5,7.5,10,20))
# df_rc$nrc = -(1-1/(2*df_rc$rc))
# COnvert to FC
df_rc$nrc = -(1 - 1/(df_rc$rc+1))
df_rc$nrc_to_rc =  1/(1+df_rc$nrc)-1
df_rc

# add one, then inverse, then subtract by one



plot(c(0,df_rc$rc), c(0,df_rc$nrc))

plot(df_rc$rc, -(1-1/(df_rc$rc+1)))

plot(df_rc$rc, -(1-1/(2*df_rc$rc+1)))

# df_rc = data.frame(x=1,y=y_rats)
# df_rc$rc = (df_rc$y-df_rc$x)/df_rc$x
# df_rc$y2 <- df_rc$x*-df_rc$rc + df_rc$x


df = data.frame( x = 1, y = c(1/rev(y_rats),1,y_rats))
df$rc <- (df$y-df$x)/df$x
df$fc <- df$y/df$x


#2 fold is +100%
#3 fold is +200%
#4 fold is +300%

# Fold change is not a measure of change, it includes how much you have to start with
# FC = Y/X
# Relative change is a true measure of change
# RC = (Y-X)/X

# Convert percent change to hold change
rc_2_fc <- function(x) {x+1}
fc_2_rc <- function(x) {x-1}
rc_2_fc(df$rc)
fc_2_rc(df$fc)

# Fold Change to Mirrored Fold Change
fc_to_mirrored_fc <- function(x) { c(-1/x[x<1], x[x>=1])}
mirrored_fc_to_fc <- function(x) { c(-1/x[x<0], x[x>=0])}
# mirrored_fc_base_label <-function(x) {-1/x[x<0]}

df$mfc <- fc_to_mirrored_fc(df$fc)
df$lab_mfc <- mirrored_fc_to_fc(df$mfc)

# Percent Change to Mirrored Fold Change
rc_to_mirrored_fc <- function(x) { c(-1/(x[x<0]+1), (x[x>=0]+1)) }
# Use this function to back-calcualte labels
mirrored_fc_to_rc <- function(x) { c(-1/(x[x<1])-1, (x[x>=1]-1)) }
df$rc2mfc <- rc_to_mirrored_fc(df$rc)
df$rc2mfc2rc <- mirrored_fc_to_rc(rc_to_mirrored_fc(df$rc))
df










y_rats <- c(10.1/10, 11/10, 12.5/10, 15/10, 2, 4, 5, 10, 20, 30, 50, 100)

df = data.frame( x = 1, y = y_rats)
df$rc <- (df$y-df$x)/df$x


