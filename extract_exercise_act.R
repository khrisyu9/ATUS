###############################################################################
############## Collect Exercise Timing of Primary Sleep Respodants ##############
###############################################################################
act_before_sleep2 = read.csv("D:/ATUS/Processed_Data/act_duration.2hr.debug.csv", check.names=FALSE)
exer.act = read.csv("D:/ATUS/Processed_Data/exercise_activity.csv")
library(dplyr)

sum_exer = rowSums(act_before_sleep2[179:207])

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

exer_before_sleep2 = cbind(act_before_sleep2[2:3], sum_exer)

write.csv(exer_before_sleep2, "D:/ATUS/Processed_Data/exer_before_sleep2.csv")

non_zero_exer_df = data.frame("time before primary sleep" = c("5-4h","4-3h","3-2h","2-1h","<=1h"), "non zero number" = c(2397, 3814, 3682, 2145, 1405))
write.csv(non_zero_exer_df, "D:/ATUS/Processed_Data/non_zero_exer_comp.csv")
