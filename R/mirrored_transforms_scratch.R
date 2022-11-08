



# Relative change is a true measure of change
# RC = (Y-X)/X

# Define conversion table between rc values x<0 and x>=0
y_rats <- c(10.1/10, 11/10, 12.5/10, 15/10, 2, 4, 5, 10, 20, 30, 50, 100)
df_rc = data.frame(prc=c(1,1.5,2,4,5,7.5,10,20))
df_rc$nrc = -(1-1/(2*df_rc$prc))

# Testing if conversion functions work
rc_test <- data.frame(rc = c(rev(df_rc$nrc), 1, df_rc$prc))
rc_test$rc_2_mrc <- mirror_rc(rc_test$rc)
rc_test$mrc_2_rc <- mirror_rc(rc_test$rc_2_mrc, forward = FALSE)




# Fold change is not a measure of change, it includes how much you have to start with
# FC = Y/X

# Define Conversion table between fc<1 and fc>=1
y_rats <- c(10.1/10, 11/10, 12.5/10, 15/10, 2, 4, 5, 10, 20, 30, 50, 100)
df_fc = data.frame(nfc = 1/rev(y_rats), pfc = y_rats)

# Test dataframe for fc conversion functions
fc_test <- data.frame(fc = c(df_fc$nfc, 1, df_fc$pfc))
fc_test$fc_2_mfc <- mirror_fc(fc_test$fc)
fc_test$mfc_2_fc <- mirror_fc(fc_test$fc_2_mfc, forward = FALSE)



#