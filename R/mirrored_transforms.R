


y_rats <- c(10.1/10, 11/10, 12.5/10, 15/10, 2, 4, 5, 10, 20, 30, 50, 100)


df = data.frame(x=1,y=c(1/rev(y_rats),1,y_rats))
df$pc <- (df$y-df$x)/df$x
df$fc <- df$y/df$x


# Convert percent change to hold change
pc_2_fc <- function(x) {x+1}
fc_2_pc <- function(x) {x-1}
pc_2_fc(df$pc)
fc_2_pc(df$fc)

# Fold Change to Mirrored Fold Change
fc_to_mirrored_fc <- function(x) { c(-1/x[x<1], x[x>=1])}
mirrored_fc_to_fc <- function(x) { c(-1/x[x<0], x[x>=0])}
# mirrored_fc_base_label <-function(x) {-1/x[x<0]}

df$mfc <- fc_to_mirrored_fc(df$fc)
df$lab_mfc <- mirrored_fc_to_fc(df$mfc)


# Percent Change to Mirrored Fold Change
pc_to_mirrored_fc <- function(x) { c(-1/(x[x<0]+1), (x[x>=0]+1)) }
# Use this function to back-calcualte labels
mirrored_fc_to_pc <- function(x) { c(-1/(x[x<1])-1, (x[x>=1]-1)) }
df$pc2mfc <- pc_to_mirrored_fc(df$pc)
df$pc2mfc2pc <- mirrored_fc_to_pc(pc_to_mirrored_fc(df$pc))
df









