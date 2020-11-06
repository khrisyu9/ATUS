###############################################################################
############## Collect Exercise Timing of Primary Sleep Respodants ##############
###############################################################################
act_before_sleep1 = read.csv("D:/ATUS/Processed_Data/act_duration.1hr.debug.csv", check.names=FALSE)
exer.act = read.csv("D:/ATUS/Processed_Data/exercise_activity.csv")
library(dplyr)
sum_exer = rep(0,nrow(act_before_sleep1))

sum_exer = rowSums(act_before_sleep1[163:184])

length(which(sum_exer != 0))

library(GLDEX)
sum_nonzero_exer = fun.zero.omit(sum_exer) 

max(sum_exer)

min(sum_exer)

length(which(sum_exer > 0))

for (i in 1:length(sum_exer)){
  if (sum_exer[i]<0){
    sum_exer[i] = 0
  }
}

length(which(sum_exer != 0))

mean(sum_nonzero_exer)

exer_before_sleep1 = cbind(act_before_sleep1[1:3], sum_exer)

write.csv(exer_before_sleep1, "D:/ATUS/Processed_Data/exer_before_sleep1.csv")
