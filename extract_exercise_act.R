###############################################################################
############## Collect Exercise Timing of Primary Sleep Respodants ##############
###############################################################################
act_before_sleep1 = read.csv( "D:/ATUS/Processed_Data/act_duration.1hr.debug.csv", check.names=FALSE)
exer.act = read.csv("D:/ATUS/Processed_Data/exercise_activity.csv")
library(dplyr)
sum_exer = rep(0,nrow(act_before_sleep1))

sum_exer = rowSums(act_before_sleep1[163:184])

length(which(sum_exer != 0))

library(GLDEX)
sum_nonzero_exer = fun.zero.omit(sum_exer) 

mean(sum_nonzero_exer)

act_before_sleep1 = cbind(act_before_sleep1, sum_exer)